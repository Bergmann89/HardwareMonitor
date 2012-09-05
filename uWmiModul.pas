unit uWmiModul;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, Windows, ActiveX, ComObj, Variants,
  GDIPOBJ, GDIPAPI, uUSBDisplay;

const
  wbemFlagForwardOnly = $00000020;

type

  { TwmiThread }

  TwmiThread = class(TCycleThread)
    SWbemLocator: OLEVariant;
    WMIService: OLEVariant;
    LastTime: Cardinal;
    Data: Pointer;
    Namespace: String;

    procedure Init; override;
    procedure Finish; override;

    constructor Create; virtual;
  end;
  TwmiThreadClass = class of TwmiThread;

  { TwmiGraphModul }

  TwmiGraphModul = class(TBasicModul)
  private
    fHandle: HDC;
    fBitmap: HBITMAP;
    fGraphic: TGPGraphics;
    fBrush: TGPSolidBrush;
    fPen: TGPPen;

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
    fDataArr: array of Single;
    fDataMax: packed record
      min, max: Single;
    end;
    fDataID: Integer;
    fLineChart: Boolean;
    fDrawGraph: Boolean;
    fSecondLineSize: Single;

    procedure CreateFonts;
  protected
    fFirstLineFormat: String;
    fSecondLineFormat: String;
    fUpdateThread: TwmiThread;

    procedure ClearData;
    function GetData: Single; virtual;
    function GetLargeDisplayString: String; virtual;
    function GetSmallDisplayString: String; virtual;
    function GetFontColor: Cardinal; virtual;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); override;
    procedure Update; override;
    function Draw: TDrawResult; override;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean; override;
    procedure SetSettings(const aData: PSettingsItemRec); override;

    constructor Create(const aThreadClass: TwmiThreadClass);
    destructor Destroy; override;
  end;

  function FormatByte(const aBytes: Single): String;
  function ToStr(const f: Single; const Digits: Integer = -3): String;

implementation

function FormatByte(const aBytes: Single): String;
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
procedure TwmiThread.Init;
var
  s: Variant;
begin
  s := Namespace;
  CoInitialize(nil);
  SWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  WMIService   := SWbemLocator.ConnectServer('', s, '', '');
  Update;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiThread.Finish;
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TwmiThread.Create;
begin
  inherited Create;
  namespace := 'root\CIMV2';
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiGraphModul.CreateFonts;
begin
  if Assigned(fFont) then
    fFont.Free;
  fFont := TGPFont.Create(fFontData.Name, fFontData.Size, fFontData.Style);
  fFontMetric := GetTextMetric(fHandle, fFont, fGraphic);

  if Assigned(fSmallFont) then
    fSmallFont.Free;
  fSmallFont := TGPFont.Create(fFontData.Name, fSecondLineSize*fFontData.Size, fFontData.Style);
  fSmallFontMetric := GetTextMetric(fHandle, fSmallFont, fGraphic);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiGraphModul.ClearData;
begin
  FillChar(fDataArr[0], Length(fDataArr) * SizeOf(fDataArr[0]), 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TwmiGraphModul.GetData: Single;
begin
  result := 0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TwmiGraphModul.GetLargeDisplayString: String;
begin
  result := '';
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TwmiGraphModul.GetSmallDisplayString: String;
begin
  result := '';
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TwmiGraphModul.GetFontColor: Cardinal;
begin
  result := fFontData.Color;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiGraphModul.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
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
procedure TwmiGraphModul.Update;

  procedure NextID(var aID: Integer);
  begin
    dec(aID);
    if aID < 0 then
      aID := High(fDataArr);
  end;

var
  i, id: Integer;
begin
  with (fUpdateThread as TwmiThread) do begin
    DoUpdate;
    if Length(fDataArr) > 0 then begin
      fDataArr[fDataID] := GetData;
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
function TwmiGraphModul.Draw: TDrawResult;

  procedure NextID(var aID: Integer);
  begin
    dec(aID);
    if aID < 0 then
      aID := High(fDataArr);
  end;

var
  s: Widestring;
  i, id: Integer;
  max: Single;
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
  max := max * 1.1;
  result.Handle      := fHandle;
  result.Transparent := true;
  result.Width  := size.x;
  result.Height := size.y;

  fGraphic.Clear(fBgColor);

  if fDrawGraph or not fIsSmall then begin
    fGraphic.IntersectClip(MakeRect(4.0, 4.0, size.x-8.0, size.y-8.0));
    fGraphic.SetSmoothingMode(SmoothingModeAntiAlias);
    fPen.SetColor(fColor);
    id := fDataID;
    if fLineChart then begin
      NextID(id);
      p2 := MakePoint(size.x - 4.0, (size.y-5.0)*(1 - fDataArr[id]/max));
      NextID(id);
      for i := 1 to High(fDataArr) do begin
        p1 := MakePoint(size.x - i - 4.0, (size.y-5.0)*(1 - fDataArr[id]/max));
        fGraphic.DrawLine(fPen, p1, p2);
        if p1.X < 4.0 then
          break;
        p2 := p1;
        NextID(id);
      end;
    end else begin
      for i := 0 to High(fDataArr) do begin
        p1 := MakePoint(size.x - i - 4.0, (size.y-4.0)*(1 - fDataArr[id]/max));
        p2 := p1;
        p2.Y := (size.y-4.0);
        fGraphic.DrawLine(fPen, p1, p2);
        NextID(id);
        if p1.X < 4.0 then
          break;
      end;
    end;
    fGraphic.SetSmoothingMode(SmoothingModeDefault);
    fGraphic.ResetClip;
  end;

  fBrush.SetColor(GetFontColor);
  s := UTF8Decode(GetLargeDisplayString);
  fGraphic.DrawString(s, Length(s), fFont, MakePoint(4.0, 4.0), nil, fBrush);
  s := UTF8Decode(GetSmallDisplayString);
  fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(4.0, 4.0+fFontMetric.tmAscent+fFontMetric.tmDescent), nil, fBrush);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TwmiGraphModul.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;
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
procedure TwmiGraphModul.SetSettings(const aData: PSettingsItemRec);
begin
  inherited SetSettings(aData);
  CreateFonts;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TwmiGraphModul.Create(const aThreadClass: TwmiThreadClass);
begin
  inherited Create;

  fHandle := CreateCompatibleDC(GetDC(0));
  fBitmap := CreateBitmap(1, 1, 1, 32, nil);
  SelectObject(fHandle, fBitmap);

  fLineChart := true;
  fDrawGraph := true;
  fSecondLineSize := 0.7;
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

  fSettingItems.Add(CreateSettingsItem(
    'color', 'Farbe', SETTINGS_FLAG_COLOR, dtInt32, @fColor));
  fSettingItems.Add(CreateSettingsItem(
    'background_color', 'Hintergrundfarbe', SETTINGS_FLAG_COLOR, dtInt32, @fBgColor));
  fSettingItems.Add(CreateSettingsItem(
    'linechart', 'Liniendiagramm', SETTINGS_FLAG_NONE, dtBool, @fLineChart));
  fSettingItems.Add(CreateSettingsItem(
    'drawchart', 'Graphen zeichnen', SETTINGS_FLAG_NONE, dtBool, @fDrawGraph));
  CreateFontSettingItems(fSettingItems, 'font', 'Font', @fFontData);
  fSettingItems.Add(CreateSettingsItem(
    '2nd_line_size', 'Größe 2. Zeile', SETTINGS_FLAG_NONE, dtFloat32, @fSecondLineSize));
  fSettingItems.Add(CreateSettingsItem(
    'line1_format', 'Zeile 1', SETTINGS_FLAG_NONE, dtString, @fFirstLineFormat));
  fSettingItems.Add(CreateSettingsItem(
    'line2_format', 'Zeile 2', SETTINGS_FLAG_NONE, dtString, @fSecondLineFormat));

  fUpdateThread := aThreadClass.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TwmiGraphModul.Destroy;
begin
  fUpdateThread.Free;

  fFont.Free;
  fBrush.Free;
  fGraphic.Free;

  DeleteObject(fBitmap);
  DeleteDC(fHandle);
  inherited Destroy;
end;

end.

