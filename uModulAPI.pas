unit uModulAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, syncobjs, GDIPOBJ, uUSBDisplay;

type
  TModulInfo = packed record
    Name, Autor, eMail, Version: PChar;
    Year: Integer;
  end;

  TDrawResult = packed record
    Handle: HDC;
    Width: Integer;
    Height: Integer;
    Transparent: Boolean;
  end;

  TModulFontData = packed record
    Name: PChar;
    Style: Integer;
    Color: Cardinal;
    Size: Integer;
  end;
  PModulFontData = ^TModulFontData;

  TModulFontDataEx = packed record
    Name: String;
    Style: Integer;
    Color: Cardinal;
    Size: Integer;
  end;
  PModulFontDataEx = ^TModulFontDataEx;
  TModulFontStyle = (FontStyleBold = 1, FontStyleItalic = 2, FontStyleUnderline = 4, FontStyleStrikeout = 8);

  TDataType = (dtInt32, dtFloat32, dtBool, dtString, dtFile, dtPicture, dtColor, dtFont, dtByte);
  TSettingsItem = packed record
    Name: PChar;
    Min, Max: Single;
    Data: Pointer;
    DataType: TDataType;
  end;
  PSettingsItem = ^TSettingsItem;

  TBasicModul = class(TObject)
  protected
    fSettingsArr: array of TSettingsItem;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); virtual; abstract;
    procedure Update; virtual; abstract;
    function Draw: TDrawResult; virtual; abstract;
    procedure GetSettings(out aCount: Integer; out aData: PSettingsItem); virtual;
    procedure SetSettings(const aData: PSettingsItem); virtual;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean; virtual;
  end;

  TCycleThread = class(TThread)
  private
    fIdleEvent: TEvent;
    fCanWorkEvent: TEvent;
  public
    procedure Execute; override;
    procedure DoUpdate; virtual;
    procedure Init; virtual;
    procedure Update; virtual;
    procedure Finish; virtual;
    procedure Terminate;

    constructor Create;
    destructor Destroy; override;
  end;

  TInitLibDataProc = procedure(); stdcall;
  TFreeLibDataProc = procedure(); stdcall;
  TModulInfoProc = function(): TModulInfo; stdcall;
  TCreateModulProc = function(): Pointer; stdcall;
  TFreeModulProc = procedure(aHandle: Pointer); stdcall;
  TResizeProc = procedure(aHandle: Pointer; aSmall: Boolean; aSmallW, aSmallH, aLargeW, aLargeH: Integer); stdcall;
  TUpdateProc = procedure(aHandle: Pointer); stdcall;
  TDrawProc = function(aHandle: Pointer): TDrawResult; stdcall;
  TGetSettingsProc = procedure(aHandle: Pointer; out aCount: Integer; out aData: PSettingsItem); stdcall;
  TSetSettingsProc = procedure(aHandle: Pointer; aData: PSettingsItem); stdcall;
  TSendTouchReportProc = function(aHandle: Pointer; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode): Boolean; stdcall;

  function GetTextMetric(const aHandle: HDC; const aFont: TGPFont; const aGraphic: TGPGraphics): TTEXTMETRIC;
  function HSVColor(H, S, V, A: Single): Cardinal;

implementation

uses
  Math;

function GetTextMetric(const aHandle: HDC; const aFont: TGPFont; const aGraphic: TGPGraphics): TTEXTMETRIC;
var
  LogFont: TLOGFONTA;
  h: HDC;
  f: HFONT;
begin
  if not Assigned(aFont) then
    exit;
  aFont.GetLogFontA(aGraphic, LogFont);
  h := CreateCompatibleDC(aHandle);
  try
    f := CreateFontIndirect(LogFont);
    try
      SelectObject(h, f);
      GetTextMetrics(h, result);
    finally
      DeleteObject(f);
    end;
  finally
    DeleteDC(h);
  end;
end;

function HSVColor(H, S, V, A: Single): Cardinal;
var
  _h: Integer;
  f, p, q, t: Single;
  rL, gL, bL, aL: Byte;
begin
//H normieren
  while (H < 0) do
    H := H + 360;
  while (H > 360) do
    H := H - 360;

  _h := Floor(H / 60);
  f := H/60 - _h;
  p := V * (1 - S);
  q := V * (1 - S * f);
  t := V * (1 - S * (1 - f));
  case _h of
    1: begin
      rL := Round(q*255);
      gL := Round(V*255);
      bL := Round(p*255);
      aL := Round(A*255);
    end;
    2: begin
      rL := Round(p*255);
      gL := Round(V*255);
      bL := Round(t*255);
      aL := Round(A*255);
    end;
    3: begin
      rL := Round(p*255);
      gL := Round(q*255);
      bL := Round(V*255);
      aL := Round(A*255);
    end;
    4: begin
      rL := Round(t*255);
      gL := Round(p*255);
      bL := Round(V*255);
      aL := Round(A*255);
    end;
    5: begin
      rL := Round(V*255);
      gL := Round(p*255);
      bL := Round(q*255);
      aL := Round(A*255);
    end
  else
    rL := Round(V*255);
    gL := Round(t*255);
    bL := Round(p*255);
    aL := Round(A*255);
  end;
  result := (aL shl 24) or (rL shl 16) or (gL shl 8) or bL;
end;

procedure TBasicModul.GetSettings(out aCount: Integer; out aData: PSettingsItem);
begin
  aCount := Length(fSettingsArr);
  if aCount > 0 then
    aData := @fSettingsArr[0]
  else
    aData := nil;
end;

procedure TBasicModul.SetSettings(const aData: PSettingsItem);
var
  i: Integer;
  p: PSettingsItem;
begin
  p := aData;
  for i := 0 to High(fSettingsArr) do with fSettingsArr[i] do begin
    case DataType of
      dtInt32:
        PInteger(Data)^ := PInteger(p^.Data)^;
      dtFloat32:
        PSingle(Data)^ := PSingle(p^.Data)^;
      dtBool:
        PBoolean(Data)^ := PBoolean(p^.Data)^;
      dtString, dtFile, dtPicture:
        PChar(Data) := PChar(p^.Data);
      dtColor:
        PCardinal(Data)^ := PCardinal(p^.Data)^;
      dtFont:
        PModulFontData(Data)^ := PModulFontData(p^.Data)^;
      dTByte:
        PByte(Data)^ := PByte(p^.Data)^;
    end;
    inc(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TBasicModul.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;
begin
  result := false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.Execute;
begin
  try
    Init;
  finally
    fCanWorkEvent.WaitFor(INFINITE);
  end;
  while not Terminated do begin
    try
      Update;
    finally
      fCanWorkEvent.ResetEvent;
      fIdleEvent.SetEvent;
    end;
    fCanWorkEvent.WaitFor(INFINITE);
  end;
  Finish;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.DoUpdate;
begin
  fIdleEvent.ResetEvent;
  fCanWorkEvent.SetEvent;
  fIdleEvent.WaitFor(INFINITE);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.Init;
begin
  //DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.Update;
begin
  //DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.Finish;
begin
  //DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCycleThread.Terminate;
begin
  inherited Terminate;
  fCanWorkEvent.SetEvent;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TCycleThread.Create;
begin
  inherited Create(false);
  fIdleEvent    := TEvent.Create(nil, true, true, '');
  fCanWorkEvent := TEvent.Create(nil, true, false, '');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TCycleThread.Destroy;
begin
  Terminate;
  fIdleEvent.Free;
  fCanWorkEvent.Free;
  inherited Destroy;
end;

end.

