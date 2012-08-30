unit uCPULoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, ActiveX, ComObj, Variants, windows,
  GDIPAPI, GDIPOBJ, syncobjs, uUSBDisplay;

type
  TCPUInfo = packed record
    Name, Identifier: String;
  end;

  TCPULoad = packed record
    Name, Identifier: String;
    Value, Max: Single;
  end;

  TcpuState = (cpusCloseBtDown = 1, cpusChartButtonX = 10);
  TcpuStates = set of TcpuState;
  TCPU = class(TBasicModul)
  private
    fHandle: HDC;
    fBitmap: HBITMAP;
    fSizeMin, fSizeMax: TPoint;
    fFirstUpdate: Boolean;
    fIsSmall: Boolean;
    fIsKeyDown: Boolean;

    fBgColor1: Cardinal;
    fBgColor2: Cardinal;
    fColor: Cardinal;
    fFontData: TModulFontData;

    fStrechData: Boolean;
    fDataDist: Byte;
    fDataCount: Integer;
    fDataIndex: Integer;
    fDataRect: TGPRectF;
    fCpuLoadData: array of packed record
      Max: Single;
      Enabled: Boolean;
      ButtonRect: TGPRectF;
      Values: array of Single;
    end;

    fCloseButtonRect: TGPRectF;   //Width/Height = x2/y2!!!
    fStates: TcpuStates;

    fBrush: TGPSolidBrush;
    fPen: TGPPen;
    fGraphic: TGPGraphics;
    fFont: TGPFont;
    fSmallFont: TGPFont;
    fFontMetric: TTEXTMETRIC;
    fSmallFontMetric: TTEXTMETRIC;

    fWMIThread: TCycleThread;

    procedure InitCpuLoadData;
    procedure CreateFonts;
    procedure CreateSettings;
    procedure FreeSettings;
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

uses
  Math;

const
  wbemFlagForwardOnly = $00000020;
  CHART_BUTTON_WIDTH  = 50;
  CHART_BUTTON_HEIGHT = 20;

type
  TwmiCPUThread = class(TCycleThread)
    SWbemLocator : OLEVariant;
    WMIService   : OLEVariant;
    CPUInfoArr: array of TCPUInfo;
    CPULoadArr: array of TCPULoad;
    UpdateTime: Cardinal;
    Updated: Boolean;

    procedure CreateCPUInfo;
    procedure CreateCPULoad;

    procedure Init; override;
    procedure Update; override;
    procedure Finish; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiCPUThread.Init;
begin
  CoInitialize(nil);
  SWbemLocator  := CreateOleObject('WbemScripting.SWbemLocator');
  WMIService    := SWbemLocator.ConnectServer('localhost', 'root\OpenHardwareMonitor', '', '');
  CreateCPUInfo;
  CreateCPULoad;
  Update;
  UpdateTime := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiCPUThread.Update;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
  i: Integer;
  s: Variant;
begin
  if Length(CPUInfoArr) <= 0 then
    exit;

  Updated := GetTickCount - UpdateTime > 1000;
  if Updated then begin
    UpdateTime := GetTickCount;
    s := 'SELECT * FROM Sensor WHERE Parent = "'+CPUInfoArr[0].Identifier+'" AND SensorType = "Load"';
    FWbemObjectSet:= WMIService.ExecQuery(s,'WQL',wbemFlagForwardOnly);
    oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
    while oEnum.Next(1, FWbemObject, nil) = 0 do begin
      i := FWbemObject.Properties_.Item('Index').Value;
      if (i >= 0) and (i < Length(CPULoadArr)) then with CPULoadArr[i] do begin
        Value := FWbemObject.Properties_.Item('Value').Value;
      end;
    end;
  end;
  for i := 0 to High(CPULoadArr) do with CPULoadArr[i] do
    if Value > Max then
      Max := Value
    else if Max > 0 then
      Max := Max - 1.0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiCPUThread.Finish;
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiCPUThread.CreateCPUInfo;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
begin
  SetLength(CPUInfoArr, 0);
  FWbemObjectSet := WMIService.ExecQuery('SELECT * FROM Hardware WHERE HardwareType = "CPU"','WQL',wbemFlagForwardOnly);
  oEnum          := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, nil) = 0 do begin
    SetLength(CPUInfoArr, Length(CPUInfoArr) + 1);
    with CPUInfoArr[High(CPUInfoArr)] do begin
      Name       := FWbemObject.Properties_.Item('Name').Value;;
      Identifier := FWbemObject.Properties_.Item('Identifier').Value;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TwmiCPUThread.CreateCPULoad;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
  c, i: Integer;
  s: Variant;
begin
  SetLength(CPULoadArr, 0);
  if Length(CPUInfoArr) <= 0 then
    exit;
  s := 'SELECT * FROM Sensor WHERE Parent = "'+CPUInfoArr[0].Identifier+'" AND SensorType = "Load"';
  FWbemObjectSet:= WMIService.ExecQuery(s,'WQL',wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  c := 0;
  while oEnum.Next(1, FWbemObject, nil) = 0 do
    inc(c);
  oEnum.Reset();
  SetLength(CPULoadArr, c);
  while oEnum.Next(1, FWbemObject, nil) = 0 do begin
    i := FWbemObject.Properties_.Item('Index').Value;
    if (i >= 0) and (i < Length(CPULoadArr)) then with CPULoadArr[i] do begin
      Name       := FWbemObject.Properties_.Item('Name').Value;
      Identifier := FWbemObject.Properties_.Item('Identifier').Value;
      Value      := FWbemObject.Properties_.Item('Value').Value;;
      Max        := Value;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.InitCpuLoadData;
var
  fH: Single;
  gWidth, i: Integer;
begin
  fH := fFontMetric.tmAscent + fFontMetric.tmDescent;
  with (fWMIThread as TwmiCPUThread) do begin
    gWidth := (fSizeMax.x - CHART_BUTTON_WIDTH - 12);
    fDataRect.X      := CHART_BUTTON_WIDTH + 8;
    fDataRect.Y      := fH+6.0;
    fDataRect.Width  := gWidth;
    fDataRect.Height := fSizeMax.y-fH-10.0;
    i := Ceil(gWidth/fDataDist);
    if (Length(CPULoadArr) > 0) and (i <> fDataCount) then begin
      fDataIndex := 0;
      fDataCount := i;
      SetLength(fCpuLoadData, Length(CPULoadArr));
      for i := 0 to High(fCpuLoadData) do begin
        fCpuLoadData[i].Max        := 0;
        fCpuLoadData[i].Enabled    := true;
        fCpuLoadData[i].ButtonRect := MakeRect(4.0, fDataRect.Y+i*(CHART_BUTTON_HEIGHT+4.0), CHART_BUTTON_WIDTH, CHART_BUTTON_HEIGHT);
        SetLength(fCpuLoadData[i].Values, fDataCount);
        if Length(fCpuLoadData[i].Values) > 0 then
          FillChar(fCpuLoadData[i].Values[0], Length(fCpuLoadData[i].Values)*SizeOf(Single), 0);
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.CreateFonts;
begin
  if Assigned(fFont) then
    fFont.Free;
  fFont := TGPFont.Create(fFontData.Name, fFontData.Size, fFontData.Style);
  fFontMetric := GetTextMetric(fHandle, fFont, fGraphic);

  if Assigned(fSmallFont) then
    fSmallFont.Free;
  fSmallFont := TGPFont.Create(fFontData.Name, 9, fFontData.Style);
  fSmallFontMetric := GetTextMetric(fHandle, fSmallFont, fGraphic);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.CreateSettings;
begin
  SetLength(fSettingsArr, 6);
  FillChar(fSettingsArr[0], SizeOf(TSettingsItem)*Length(fSettingsArr), 0);

  with fSettingsArr[0] do begin
    Name     := 'Farbe';
    DataType := dtColor;
    Data     := @fColor;
  end;
  with fSettingsArr[1] do begin
    Name     := 'Prim. Hintergrundfarbe';
    DataType := dtColor;
    Data     := @fBgColor1;
  end;
  with fSettingsArr[2] do begin
    Name     := 'Sek. Hintergrundfarbe';
    DataType := dtColor;
    Data     := @fBgColor2;
  end;
  with fSettingsArr[3] do begin
    Name     := 'Font';
    DataType := dtFont;
    Data     := @fFontData;
  end;
  with fSettingsArr[4] do begin
    Name     := 'Diagramm autom. anpassen';
    DataType := dtBool;
    Data     := @fStrechData;
  end;
  with fSettingsArr[5] do begin
    Name     := 'Datenabstand';
    DataType := dtByte;
    Data     := @fDataDist;
    Min      := 1;
    Max      := 10;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.FreeSettings;
begin
  SetLength(fSettingsArr, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
var
  fH: Single;
begin
  fSizeMin := Classes.Point(aSmallW, aSmallH);
  fSizeMax := Classes.Point(aLargeW, aLargeH);
  fIsSmall := aSmall;

  if Assigned(fGraphic) then
    fGraphic.Free;
  if fIsSmall then
    fBitmap := CreateBitmap(aSmallW, aSmallH, 1, 32, nil)
  else
    fBitmap := CreateBitmap(aLargeW, aLargeH, 1, 32, nil);
  SelectObject(fHandle, fBitmap);
  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);

  fH := fFontMetric.tmAscent + fFontMetric.tmDescent;
  fCloseButtonRect := MakeRect(fSizeMax.x-fH-2.0, 6.0, fSizeMax.x-6.0, fH+2.0);
  InitCpuLoadData;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.Update;
var
  i: Integer;

  function GetMax: Single;
  var j: Integer;
  begin
    result := 0;
    for j := 0 to High(fCpuLoadData[i].Values) do
      if (fCpuLoadData[i].Values[j] > result) then
        result := fCpuLoadData[i].Values[j];
  end;

begin
  with (fWMIThread as TwmiCPUThread) do begin
    DoUpdate;
    if (Length(CPULoadArr) <> Length(fCpuLoadData)) then
      InitCpuLoadData;
    if Updated and (Length(CPULoadArr) = Length(fCpuLoadData)) then begin
      for i := 0 to High(CPULoadArr) do begin
        fCpuLoadData[i].Values[fDataIndex] := CPULoadArr[i].Value;
        if fStrechData then
          fCpuLoadData[i].Max := GetMax;
      end;
      inc(fDataIndex);
      if fDataIndex >= fDataCount then
        fDataIndex := 0;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TCPU.Draw: TDrawResult;
var
  w, h, fontHeight, lineH, max: Single;
  i: Integer;
  rect: TGPRectF;
  s: WideString;

  procedure NextID(var id: Integer);
  begin
    dec(id);
    if id < 0 then
      id := fDataCount-1;
  end;

  procedure DrawDataLine(const LineID: Integer; const max: Single; const aRect: TGPRectF);
  var
    i, id: Integer;
    p1, p2: TGPPointF;
  begin
    if LineID > High(fCpuLoadData) then
      exit;
    id := fDataIndex;
    NextId(id);
    p2.X := aRect.X + aRect.Width;
    p2.Y := aRect.Y + aRect.Height * (1 - fCpuLoadData[LineID].Values[id]/max);
    NextID(id);
    for i := 1 to fDataCount-1 do begin
      p1.X := aRect.X + aRect.Width - i*fDataDist;
      if p1.X < 0 then
        exit;
      p1.Y := aRect.Y + aRect.Height * (1 - fCpuLoadData[LineID].Values[id]/max);
      fGraphic.DrawLine(fPen, p1, p2);
      p2 := p1;
      NextID(id);
    end;
  end;

begin
  if fIsSmall then begin
    result.Width       := fSizeMin.x;
    result.Height      := fSizeMin.y;
    result.Transparent := true;
    result.Handle      := fHandle;

    fGraphic.Clear(fBgColor1);
    fGraphic.IntersectClip(MakeRect(4, 4, fSizeMin.x-8, fSizeMin.y-8));
    with (fWMIThread as TwmiCPUThread) do begin
      if Length(CPUInfoArr) > 0 then begin
        fBrush.SetColor(fFontData.Color);
        s := UTF8Decode('CPU Usage');
        fGraphic.DrawString(s, Length(s), fFont, MakePoint(2.0, 2.0), fBrush);
      end;
      if Length(CPULoadArr) > 0 then begin
        fontHeight := fFontMetric.tmAscent + fFontMetric.tmDescent;
        w := (fSizeMin.x - 4) / Length(CPULoadArr);
        h := fSizeMin.y - fontHeight - 8.0;
        for i := 0 to High(CPULoadArr) do begin
          fBrush.SetColor(fBgColor2);
          fGraphic.FillRectangle(fBrush, MakeRect(4.0+i*w, fontHeight+4.0, w-4.0, h));

          lineH := h * CPULoadArr[i].Max/100;
          fBrush.SetColor(fBgColor2);
          fGraphic.FillRectangle(fBrush, MakeRect(4.0+i*w, fontHeight+(h-lineH)+4.0, w-4.0, lineH));

          lineH := h * CPULoadArr[i].Value/100;
          fBrush.SetColor(fColor);
          fGraphic.FillRectangle(fBrush, MakeRect(4.0+i*w, fontHeight+(h-lineH)+4.0, w-4.0, lineH));
          if i = 0 then
            s := 'T'
          else
            s := UTF8Decode(IntToStr(i));
          fBrush.SetColor(fFontData.Color);
          fGraphic.MeasureString(s, Length(s), fSmallFont, MakePoint(0.0, 0.0), rect);
          fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(3.0 + i*w + (w - rect.Width)/2, fontHeight + 5.0), fBrush);
        end;
      end;
    end;
    fGraphic.ResetClip;
  end else begin
    result.Width       := fSizeMax.x;
    result.Height      := fSizeMax.y;
    result.Transparent := true;
    result.Handle      := fHandle;

    fontHeight := fFontMetric.tmAscent + fFontMetric.tmDescent;
    fGraphic.Clear(fBgColor1);
    fGraphic.IntersectClip(MakeRect(4, 4, fSizeMax.x-8, fSizeMax.y-8));
    with (fWMIThread as TwmiCPUThread) do begin
      if (Length(CPUInfoArr) > 0) then begin
        s := UTF8Decode(CPUInfoArr[0].Name);
        fBrush.SetColor(fFontData.Color);
        fGraphic.DrawString(s, Length(s), fFont, MakePoint(2.0, 2.0), fBrush);
      end;
    end;

    fBrush.SetColor(fBgColor1);
    fGraphic.FillRectangle(fBrush, fDataRect);
//LINES
    max := 0;
    if fStrechData then
      for i := 0 to High(fCpuLoadData) do
        if fCpuLoadData[i].Enabled and (fCpuLoadData[i].Max > max) then
          max := fCpuLoadData[i].Max;
    if max = 0 then
      max := 100;
    rect := MakeRect(fDataRect.X+4, fDataRect.Y+4, fDataRect.Width-8, fDataRect.Height-8);
    fGraphic.IntersectClip(rect);
    fGraphic.SetSmoothingMode(SmoothingModeAntiAlias);
    for i := 0 to High(fCpuLoadData) do
      if fCpuLoadData[i].Enabled then begin
        fPen.SetColor(HSVColor(360*i/Length(fCpuLoadData), 1, 1, 1));
        fPen.SetWidth(1.0);
        DrawDataLine(i, max, rect);
      end;
    fGraphic.ResetClip;

//BESCHRIFTUNG
  fBrush.SetColor(fFontData.Color);
  s := IntToStr(Round(max))+'%';
  fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(fDataRect.X, fDataRect.Y), fBrush);
  s := '0%';
  fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(fDataRect.X, fDataRect.Y +
    fDataRect.Height - fSmallFontMetric.tmAscent - fSmallFontMetric.tmDescent), fBrush);

//BUTTONS
    for i := 0 to High(fCpuLoadData) do begin
      rect := fCpuLoadData[i].ButtonRect;
      if fCpuLoadData[i].Enabled then
        fBrush.SetColor(fBgColor2)
      else
        fBrush.SetColor(fBgColor1);
      fGraphic.FillRectangle(fBrush, rect);
      fGraphic.IntersectClip(rect);

      if (i = 0) then
        s := 'Total'
      else
        s := 'CPU #'+IntToStr(i);
      fBrush.SetColor(fFontData.Color);
      fGraphic.DrawString(s, Length(s), fSmallFont, MakePoint(rect.x + 2.0, rect.y + 2.0), fBrush);
      fGraphic.ResetClip;

      fPen.SetWidth(2);
      fPen.SetColor(HSVColor(360*i/Length(fCpuLoadData), 1, 1, 1));
      fGraphic.DrawLine(fPen, rect.X+2, rect.Y+rect.Height-2, rect.X+rect.Width-2, rect.Y+rect.Height-2);
    end;

//CLOSE BUTTON
    if cpusCloseBtDown in fStates then
      fBrush.SetColor(fBgColor2)
    else
      fBrush.SetColor(fBgColor1);
    fGraphic.FillRectangle(fBrush, MakeRect(fSizeMax.x - fontHeight - 4.0, 4.0, fontHeight, fontHeight));
    fPen.SetColor(fColor);
    fPen.SetWidth(3);
    fGraphic.DrawLine(fPen, fCloseButtonRect.X, fCloseButtonRect.Y, fCloseButtonRect.Width, fCloseButtonRect.Height);
    fGraphic.DrawLine(fPen, fCloseButtonRect.X, fCloseButtonRect.Height, fCloseButtonRect.Width, fCloseButtonRect.Y);
    fGraphic.SetSmoothingMode(SmoothingModeDefault);
    fGraphic.ResetClip;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TCPU.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;

  function IsInRect(const aPoint: TPoint; const aRect: TGPRectF): Boolean;
  begin
    result :=
      (aPoint.x >= aRect.x) and
      (aPoint.x <= aRect.x + aRect.Width) and
      (aPoint.y >= aRect.y) and
      (aPoint.y <= aRect.y + aRect.Height);
  end;

var
  i: Integer;
begin
  result := inherited SendTouchReport(aPoint, aPressure, aMode);
  case aMode of
    tmPenDown: begin
      fIsKeyDown := true;
      if not fIsSmall then begin
        if (aPoint.x >= fCloseButtonRect.x) and (aPoint.x <= fCloseButtonRect.Width) and
           (aPoint.y >= fCloseButtonRect.y) and (aPoint.y <= fCloseButtonRect.Height) then begin
          fStates := fStates + [cpusCloseBtDown];
        end;
        for i := 0 to High(fCpuLoadData) do begin
          if IsInRect(aPoint, fCpuLoadData[i].ButtonRect) then
            fStates := fStates + [TcpuState(Integer(cpusChartButtonX) + i)];
        end
      end;
    end;

    tmPenMove: if fIsKeyDown then begin
      if (aPoint.x >= fCloseButtonRect.x) and (aPoint.x <= fCloseButtonRect.Width) and
         (aPoint.y >= fCloseButtonRect.y) and (aPoint.y <= fCloseButtonRect.Height) then
        fStates := fStates + [cpusCloseBtDown]
      else
        fStates := fStates - [cpusCloseBtDown];
    end;

    tmPenUp: if fIsKeyDown then begin
      if fIsSmall then begin
        fIsKeyDown := false;
        result     := true;
      end else begin
        //BUTTONS IM LARGE MODE BEHANDELN
        fIsKeyDown := false;
        if cpusCloseBtDown in fStates then begin
          fStates := fStates - [cpusCloseBtDown];
          result := true;
        end;
        for i := 0 to High(fCpuLoadData) do begin
          if TcpuState(Integer(cpusChartButtonX)+i) in fStates then begin
            fCpuLoadData[i].Enabled := not fCpuLoadData[i].Enabled;
            fStates := fStates - [TcpuState(Integer(cpusChartButtonX) + i)];
          end;
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCPU.SetSettings(const aData: PSettingsItem);

  procedure CheckColor(var aColor: Cardinal);
  begin
    if aColor shr 24 >= $FF then
      aColor := $FE000000 or (aColor and $FFFFFF);
  end;

var
  needDataUpdate: Boolean;
  i: Integer;
begin
  needDataUpdate := PByte(PSettingsItem(aData + 5)^.Data)^ <> fDataDist;
  inherited SetSettings(aData);
  if needDataUpdate then
    InitCpuLoadData;

  CheckColor(fColor);
  CheckColor(fBgColor1);
  CheckColor(fBgColor2);
  CheckColor(fFontData.Color);

  CreateFonts;
  Resize(fIsSmall, fSizeMin.x, fSizeMin.y, fSizeMax.x, fSizeMax.y);
  for i := 0 to High(fCpuLoadData) do
    fCpuLoadData[i].ButtonRect := MakeRect(
      4.0, fDataRect.Y+i*(CHART_BUTTON_HEIGHT+4.0),
      CHART_BUTTON_WIDTH, CHART_BUTTON_HEIGHT);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TCPU.Create;
begin
  inherited Create;

  fDataDist    := 3;
  fDataCount   := 0;
  fDataIndex   := 0;
  fStates      := [];
  fStrechData  := false;
  fIsSmall     := true;
  fFirstUpdate := true;
  fIsKeyDown   := false;
  fHandle      := CreateCompatibleDC(GetDC(0));
  fBitmap      := CreateBitmap(1, 1, 1, 32, nil);
  SelectObject(fHandle, fBitmap);

  fFontData.Name  := 'Arial';
  fFontData.Style := FontStyleBold;
  fFontData.Size  := 12;
  fFontData.Color := $FEFFFFFF;

  fColor    := $FEFFFFFF;
  fBgColor1 := $60000000;
  fBgColor2 := $60FFFFFF;

  fBrush   := TGPSolidBrush.Create(0);
  fPen     := TGPPen.Create($FEFFFFFF, 3);
  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);
  CreateFonts;
  CreateSettings;

  fWMIThread := TwmiCPUThread.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TCPU.Destroy;
begin
  FreeSettings;
  fWMIThread.Free;

  fFont.Free;
  fPen.Free;
  fBrush.Free;
  fGraphic.Free;

  DeleteObject(fBitmap);
  DeleteDC(fHandle);
  inherited Destroy;
end;

end.

