unit uModulAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, syncobjs, GDIPOBJ, uUSBDisplay, fgl;

const
  SETTINGS_FLAG_NONE        =  0;
  SETTINGS_FLAG_FILE        =  1;
  SETTINGS_FLAG_PICTURE     =  2;
  SETTINGS_FLAG_IDENT_STR   =  4;
  SETTINGS_FLAG_COLOR       =  8;
  SETTINGS_FLAG_FONT_DATA   = 16;
  SETTINGS_FLAG_DISP_CHANGE = 32;
  SETTINGS_FLAG_HEX         = 64;

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
    Name: String;
    Style: Integer;
    Color: Cardinal;
    Size: Single;
  end;
  PModulFontData = ^TModulFontData;

  TDataType = (dtUnknown = 0, dtByte = 1, dtInt32 = 2, dtFloat32 = 3, dtBool = 4, dtString = 5);
  TSettingsItemRec = packed record
    IdentStr: PChar;
    DisplayStr: PChar;
    Data: Pointer;
    DataType: TDataType;
    Flags: Cardinal;
  end;
  PSettingsItemRec = ^TSettingsItemRec;

  { TSettingsItem }

  TSettingsItem = class
  private
    fPointerBased: Boolean;
    fDataType: TDataType;
    procedure SetDataType(const aValue: TDataType);
    procedure CreateData;
    procedure FreeData;
  public
    IdentStr: String;
    DisplayStr: String;
    Data: Pointer;
    Flags: Cardinal;
    property DataType: TDataType read fDataType write SetDataType;
    procedure DataToRecord(var aDataRec: TSettingsItemRec);
    constructor Create(const aPointerBased: Boolean = true);
    destructor Destroy; override;
  end;
  TSettingItemList = specialize TFPGObjectList<TSettingsItem>;

  { TBasicModul }

  TBasicModul = class(TObject)
  protected
    fSettingItems: TSettingItemList;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); virtual; abstract;
    procedure Update; virtual;
    function Draw: TDrawResult; virtual; abstract;
    function GetSettingCount: Integer; virtual;
    procedure GetSettings(const aData: PSettingsItemRec); virtual;
    procedure SetSettings(const aData: PSettingsItemRec); virtual;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean; virtual;

    constructor Create;
    destructor Destroy; override;
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
  TGetSettingCountProc = function(aHandle: Pointer): Integer; stdcall;
  TGetSettingsProc = procedure(aHandle: Pointer; aData: PSettingsItemRec); stdcall;
  TSetSettingsProc = procedure(aHandle: Pointer; aData: PSettingsItemRec); stdcall;
  TSendTouchReportProc = function(aHandle: Pointer; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode): Boolean; stdcall;

  function GetTextMetric(const aHandle: HDC; const aFont: TGPFont; const aGraphic: TGPGraphics): TTEXTMETRIC;
  function CreateSettingsItem(aIdentStr, aDisplayStr: String; aFlags: Cardinal;
    aDataType: TDataType; aData: Pointer): TSettingsItem;
  procedure CreateFontSettingItems(const aSettingsItems: TSettingItemList;
    const aIdent, aDisplay: String; const aFontData: PModulFontData);
  function HSVColor(H, S, V, A: Single): Cardinal;

implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
      GetTextMetrics(h, result{%H-});
    finally
      DeleteObject(f);
    end;
  finally
    DeleteDC(h);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function CreateSettingsItem(aIdentStr, aDisplayStr: String; aFlags: Cardinal;
  aDataType: TDataType; aData: Pointer): TSettingsItem;
begin
  result := TSettingsItem.Create;
  result.IdentStr   := aIdentStr;
  result.DisplayStr := aDisplayStr;
  result.Flags      := aFlags;
  result.DataType   := aDataType;
  result.Data       := aData;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure CreateFontSettingItems(const aSettingsItems: TSettingItemList; const aIdent, aDisplay: String; const aFontData: PModulFontData);
begin
  aSettingsItems.Add(CreateSettingsItem(
    aIdent, aDisplay, SETTINGS_FLAG_FONT_DATA, dtUnknown, nil));
  aSettingsItems.Add(CreateSettingsItem(
    aIdent+'/name', 'Name', SETTINGS_FLAG_NONE, dtString, @aFontData^.Name));
  aSettingsItems.Add(CreateSettingsItem(
    aIdent+'/size', 'Größe', SETTINGS_FLAG_NONE, dtFloat32, @aFontData^.Size));
  aSettingsItems.Add(CreateSettingsItem(
    aIdent+'/style', 'Style', SETTINGS_FLAG_NONE, dtInt32, @aFontData^.Style));
  aSettingsItems.Add(CreateSettingsItem(
    aIdent+'/color', 'Farbe', SETTINGS_FLAG_COLOR, dtInt32, @aFontData^.Color));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSettingsItem.SetDataType(const aValue: TDataType);
begin
  FreeData;
  fDataType := aValue;
  CreateData;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSettingsItem.CreateData;
begin
  if not fPointerBased then
    case fDataType of
      dtInt32:
        New(PInteger(Data));
      dtByte:
        New(PByte(Data));
      dtFloat32:
        New(PSingle(Data));
      dtBool:
        New(System.PBoolean(Data));
      dtString:
        New(PString(Data));
    else
      Data := nil;
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSettingsItem.FreeData;
begin
  if not fPointerBased then begin
    case fDataType of
      dtInt32:
        Dispose(PInteger(Data));
      dtByte:
        Dispose(PByte(Data));
      dtFloat32:
        Dispose(PSingle(Data));
      dtBool:
        Dispose(System.PBoolean(Data));
      dtString:
        Dispose(PString(Data));
    end;
    Data := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSettingsItem.DataToRecord(var aDataRec: TSettingsItemRec);
begin
  FillChar(aDataRec, SizeOf(aDataRec), 0);
  aDataRec.IdentStr   := PChar(IdentStr);
  aDataRec.DisplayStr := PChar(DisplayStr);
  aDataRec.DataType   := DataType;
  aDataRec.Flags      := Flags;
  if Assigned(Data) then begin
    case DataType of
      dtBool: begin
        System.PBoolean(aDataRec.Data)^ := System.PBoolean(Data)^;
      end;
      dtFloat32: begin
        PSingle(aDataRec.Data)^ := PSingle(Data)^;
      end;
      dtInt32: begin
        PInteger(aDataRec.Data)^ := PInteger(Data)^;
      end;
      dtString: begin
        aDataRec.Data := PChar(PString(Data)^);
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TSettingsItem.Create(const aPointerBased: Boolean);
begin
  inherited Create;
  fPointerBased := aPointerBased;
  fDataType := dtUnknown;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TSettingsItem.Destroy;
begin
  FreeData;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TBasicModul.Update;
begin
  //DUMMY
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TBasicModul.GetSettingCount(): Integer;
begin
  result := fSettingItems.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TBasicModul.GetSettings(const aData: PSettingsItemRec);
var
  i: Integer;
  p: PSettingsItemRec;
begin
  p := aData;
  for i := 0 to fSettingItems.Count-1 do with fSettingItems[i] do begin
    p^.IdentStr   := PChar(IdentStr);
    p^.DisplayStr := PChar(DisplayStr);
    p^.Flags      := Flags;
    p^.DataType   := DataType;
    case p^.DataType of
      dtString:
        p^.Data := PChar(PString(Data)^);
      else
        p^.Data := Data;
    end;
    inc(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TBasicModul.SetSettings(const aData: PSettingsItemRec);
  {
  procedure CheckColor(var aColor: Cardinal);
  begin
    if (aColor shr 24) >= $FF then
      aColor := (aColor and $FFFFFF) or $FE000000;
  end;
  }
var
  p: PSettingsItemRec;
  i: Integer;
begin
  p := aData;
  for i := 0 to fSettingItems.Count-1 do with fSettingItems[i] do begin
    case p^.DataType of
      dtString: begin
        PString(Data)^ := PChar(p^.Data);
      end;
      dtBool: begin
        System.PBoolean(Data)^ := system.PBoolean(p^.Data)^;
      end;
      dtFloat32: begin
        PSingle(Data)^ := PSingle(p^.Data)^;
      end;
      dtByte: begin
        PByte(Data)^ := PByte(p^.Data)^;
      end;
      dtInt32: begin
        PInteger(Data)^ := PInteger(p^.Data)^;
        //if Flags and SETTINGS_FLAG_COLOR > 0 then
        //  CheckColor(PCardinal(Data)^);
      end;
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
constructor TBasicModul.Create;
begin
  inherited Create;
  fSettingItems := TSettingItemList.Create(true);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TBasicModul.Destroy;
begin
  FreeAndNil(fSettingItems);
  inherited Destroy;
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
