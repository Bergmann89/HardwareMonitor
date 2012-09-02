unit uDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, Windows, ActiveX, ComObj, Variants,
  GDIPOBJ, GDIPAPI, uUSBDisplay;

type
  TDownload = class(TBasicModul)
  private
    fHandle: HDC;
    fBitmap: HBITMAP;
    fGraphic: TGPGraphics;
    fBrush: TGPSolidBrush;
    fPen: TGPPen;
    fUpdateThread: TThread;

    fPenDown: Boolean;
    fIsSmall: Boolean;
    fSizeMin: TPoint;
    fSizeMax: TPoint;
    fColor: Cardinal;
    fBgColor: Cardinal;
    fFont: TGPFont;
    fFontData: TModulFontData;
    fFontMetric: TTEXTMETRIC;
    fSmallFont: TGPFont;
    fSmallFontMetric: TTEXTMETRIC;
    fDataArr: array of Cardinal;
    fDataMax: packed record
      min, max: Cardinal;
    end;
    fDataID: Integer;
    fLineChart: Boolean;

    procedure CreateFonts;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); override;
    procedure Update; override;
    function Draw: TDrawResult; override;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean; override;
    procedure SetSettings(const aData: PSettingsItem); override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  wbemFlagForwardOnly = $00000020;

type
  TwmiDownloadThread = class(TCycleThread)
    SWbemLocator: OLEVariant;
    WMIService: OLEVariant;
    Download: packed record
      Currently: Cardinal;
      StartUp: Cardinal;
    end;

    procedure Init; override;
    procedure Update; override;
    procedure Finish; override;
  end;

function ToStr(const f: Single; const Digits: Integer = -3): String;
var
  format: TFormatSettings;
  p: Integer;
begin
  format.DecimalSeparator := '.';
  result := FloatToStrF(f, ffFixed, 12, -Digits, format);
  if Digits < 0 then begin
    p := pos('.', result);
    if (p <= 0) then begin
      result := result + '.0';
      p := Length(result) - 1;
    end;
    while (Length(result) - p) < -Digits do
      result := result + '0';

    Delete(result, p-Digits+1, Length(result));
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiDownloadThread.Init;
begin
  CoInitialize(nil);
  SWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  WMIService   := SWbemLocator.ConnectServer('', 'root\CIMV2', '', '');
  Update;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiDownloadThread.Update;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
  StartUpOld: Cardinal;
  c: Cardinal;
begin
  StartUpOld := Download.StartUp;
  FillChar(Download, SizeOf(Download), 0);
  FWbemObjectSet := WMIService.ExecQuery('SELECT BytesReceivedPersec FROM Win32_PerfRawData_Tcpip_NetworkInterface',
    'WQL', wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, nil) = 0 do begin
    c := FWbemObject.Properties_.Item('BytesReceivedPersec').Value;
    Download.StartUp := Download.StartUp + c;
  end;
  Download.Currently := Download.StartUp - StartUpOld;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiDownloadThread.Finish;
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDownload.CreateFonts;
begin
  if Assigned(fFont) then
    fFont.Free;
  fFont := TGPFont.Create(fFontData.Name, fFontData.Size, fFontData.Style);
  fFontMetric := GetTextMetric(fHandle, fFont, fGraphic);

  if Assigned(fSmallFont) then
    fSmallFont.Free;
  fSmallFont := TGPFont.Create(fFontData.Name, 0.8*fFontData.Size, fFontData.Style);
  fSmallFontMetric := GetTextMetric(fHandle, fSmallFont, fGraphic);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDownload.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
begin
  fIsSmall := aSmall;
  fSizeMin := Classes.Point(aSmallW, aSmallH);
  fSizeMax := Classes.Point(aLargeW, aLargeH);

  if Assigned(fGraphic) then
    fGraphic.Free;
  if fBitmap > 0 then
    DeleteObject(fBitmap);
  if fIsSmall then
    fBitmap := CreateBitmap(aSmallW, aSmallH, 1, 32, nil)
  else
    fBitmap := CreateBitmap(aLargeW, aLargeH, 1, 32, nil);
  SelectObject(fHandle, fBitmap);

  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if (aLargeW-7) <> Length(fDataArr) then begin
    SetLength(fDataArr, aLargeW-7);
    fDataID := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDownload.Update;

  procedure NextID(var aID: Integer);
  begin
    dec(aID);
    if aID < 0 then
      aID := High(fDataArr);
  end;

var
  i, id: Integer;
begin
  with (fUpdateThread as TwmiDownloadThread) do begin
    DoUpdate;
    if Length(fDataArr) > 0 then begin
      fDataArr[fDataID] := Download.Currently;
      fDataMax.max := 0;
      fDataMax.min := 0;
      id := fDataID;
      for i := 0 to High(fDataArr) do begin
        if fDataArr[id] > fDataMax.max then
          fDataMax.max := fDataArr[id];
        if (i < (fSizeMin.x-7)) and (fDataArr[id] > fDataMax.min) then
          fDataMax.min := fDataArr[id];
        NextID(id);
      end;

      if fDataMax.min = 0 then
        fDataMax.min := 1;
      if fDataMax.max = 0 then
        fDataMax.max := 1;

      inc(fDataID);
      if fDataID > High(fDataArr) then
        fDataID := 0;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.Draw: TDrawResult;

  function FormatByte(const aBytes: Cardinal): String;
  const PREFIX: array[0..4] of String = ('B', 'KB', 'MB', 'GB', 'TB');
  var i: Integer; s: Double;
  begin
    s := aBytes;
    i := 0;
    while (s > 1000) and (i < 4) do begin
      s := s / 1024;
      inc(i);
    end;
    result := ToStr(s, -1)+PREFIX[i];
  end;

  procedure NextID(var aID: Integer);
  begin
    dec(aID);
    if aID < 0 then
      aID := High(fDataArr);
  end;

var
  s: String;
  i, id, max: Integer;
  p1, p2: TGPPointF;
  size: TPoint;
begin
  if fIsSmall then begin
    size := fSizeMin;
    max  := fDataMax.min;
  end else begin
    size := fSizeMax;
    max  := fDataMax.max;
  end;
  if max = 0 then
    max := 1;
  result.Handle      := fHandle;
  result.Transparent := true;
  result.Width  := size.x;
  result.Height := size.y;

  fGraphic.Clear(fBgColor);

  fBrush.SetColor(fFontData.Color);
  s := 'Download';  
  fGraphic.DrawString(s, Length(s), fFont, MakePoint(4.0, 4.0), nil, fBrush);
  s := FormatByte((fUpdateThread as TwmiDownloadThread).Download.StartUp) + sLineBreak +
    FormatByte((fUpdateThread as TwmiDownloadThread).Download.Currently);
  fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(4.0, 4.0+fFontMetric.tmAscent+fFontMetric.tmDescent), nil, fBrush);

  fGraphic.IntersectClip(MakeRect(4.0, 4.0, size.x-8.0, size.y-8.0));
  fGraphic.SetSmoothingMode(SmoothingModeAntiAlias);
  fPen.SetColor(fColor);
  id := fDataID;
  if fLineChart then begin
    NextID(id);
    p2 := MakePoint(size.x - 4.0, (size.y-8.0)*(1 - fDataArr[id]/max));
    NextID(id);
    for i := 1 to High(fDataArr) do begin
      p1 := MakePoint(size.x - i - 4.0, (size.y-8.0)*(1 - fDataArr[id]/max));
      fGraphic.DrawLine(fPen, p1, p2);
      if p1.X < 4.0 then
        break;
      p2 := p1;
      NextID(id);
    end;
  end else begin
    for i := 0 to High(fDataArr) do begin
      p1 := MakePoint(size.x - i - 4.0, (size.y-8.0)*(1 - fDataArr[id]/max));
      p2 := p1;
      p2.Y := (size.y-8.0);
      fGraphic.DrawLine(fPen, p1, p2);
      NextID(id);
      if p1.X < 4.0 then
        break;
    end;
  end;
  fGraphic.SetSmoothingMode(SmoothingModeDefault);
  fGraphic.ResetClip;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;
begin
  result := inherited SendTouchReport(aPoint, aPressure, aMode);
  if (aMode = tmPenDown) then
    fPenDown := true;
  if (aMode = tmPenUp) and fPenDown then begin
    result := true;
    fPenDown := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDownload.SetSettings(const aData: PSettingsItem);

  procedure CheckColor(var aColor: Cardinal);
  begin
    if (aColor shr 24) >= $FF then
      aColor := (aColor and $FFFFFF) or $FE000000;
  end;

begin
  inherited SetSettings(aData);
  CheckColor(fFontData.Color);
  CheckColor(fBgColor);
  CheckColor(fColor);
  CreateFonts;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TDownload.Create;
begin
  inherited Create;

  fHandle := CreateCompatibleDC(GetDC(0));
  fBitmap := CreateBitmap(1, 1, 1, 32, nil);
  SelectObject(fHandle, fBitmap);

  fLineChart := true;
  fColor     := $FEFFFFFF;
  fBgColor   := $60000000;
  with fFontData do begin
    Name  := 'Arial';
    Size  := 12;
    Style := 1;
    Color := $FEFFFFFF;
  end;

  fBrush := TGPSolidBrush.Create(fBgColor);
  fPen := TGPPen.Create(fColor, 1.0);
  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);
  CreateFonts;

  SetLength(fSettingsArr, 4);
  with fSettingsArr[0] do begin
    Name     := 'Farbe';
    DataType := dtColor;
    Data     := @fColor
  end;
  with fSettingsArr[1] do begin
    Name     := 'Hintergrundfarbe';
    DataType := dtColor;
    Data     := @fBgColor;
  end;
  with fSettingsArr[2] do begin
    Name     := 'Liniendiagramm';
    DataType := dtBool;
    Data     := @fLineChart;
  end;
  with fSettingsArr[3] do begin
    Name     := 'Font';
    DataType := dtFont;
    Data     := @fFontData;
  end;

  fUpdateThread := TwmiDownloadThread.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TDownload.Destroy;
begin
  fUpdateThread.Free;

  fFont.Free;
  fGraphic.Free;

  DeleteObject(fBitmap);
  DeleteDC(fHandle);
  inherited Destroy;
end;

end.

