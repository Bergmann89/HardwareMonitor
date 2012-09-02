unit uHardwareMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, uUSBDisplay, syncobjs, Graphics, uUSBD480_API,
  uModulAPI, ExtCtrls, uMCF, contnrs, fgl;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  THardwareMonitorModul = class;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TModulInfoEx = packed record
    libName: String;
    Name, Autor, eMail, Version: String;
    Year: Integer;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  THardwareMonitor = class;
  EHardwareMonitorModul = class(Exception);
  THardwareMonitorModul = class(TObject)
  private
    fOwner: THardwareMonitor;
    fSmallSize: Classes.TPoint;
    fLargeSize: Classes.TPoint;
    fPosition: Classes.TPoint;
    fCurrentSmallSize: Classes.TPoint;
    fCurrentLargeSize: Classes.TPoint;
    fDataCS: TRTLCriticalSection;
    fEventCS: TRTLCriticalSection;
    fIsSmall: Boolean;
    fCurrentIsSmall: Boolean;
    fNeedResize: Boolean;
    fRect: TRect;
    fLibHandle: Cardinal;
    fObjHandle: Pointer;
    fSettingsArr: array of TSettingsItem;
    fModulInfoEx: TModulInfoEx;
    fName: String;

    fInitLibData: TInitLibDataProc;
    fFreeLibData: TFreeLibDataProc;
    fModulInfo: TModulInfoProc;
    fCreateModul: TCreateModulProc;
    fFreeModul: TFreeModulProc;
    fResize: TResizeProc;
    fUpdate: TUpdateProc;
    fDraw: TDrawProc;
    fSetSettings: TSetSettingsProc;
    fGetSettings: TGetSettingsProc;
    fSendTouchReport: TSendTouchReportProc;

    procedure SetSmallSize(const aValue: TPoint);
    procedure SetLargeSize(const aValue: TPoint);
    procedure SetPosition(const aValue: TPoint);
    procedure SetIsSmall(const aValue: Boolean);
    procedure SetNeedResize(const aValue: Boolean);
    procedure SetName(const aValue: String);

    function GetSettingsCount: Integer;
    function GetSettings(const aIndex: Integer): TSettingsItem;
    procedure SetSettings(const aIndex: Integer; aValue: TSettingsItem);

    procedure UpdateRect;
    procedure ClearSettings;
  public
    property IsSmall   : Boolean read fIsSmall    write SetIsSmall;
    property SmallSize : TPoint  read fSmallSize  write SetSmallSize;
    property LargeSize : TPoint  read fLargeSize  write SetLargeSize;
    property Position  : TPoint  read fPosition   write SetPosition;
    property NeedResize: Boolean read fNeedResize write SetNeedResize;
    property Name      : String  read fName       write SetName;

    property ModulInfo       : TModulInfoEx read fModulInfoEx;
    property Rect            : TRect        read fRect;
    property CurrentIsSmall  : Boolean      read fCurrentIsSmall;
    property CurrentSmallSize: TPoint       read fCurrentSmallSize;
    property CurrentLargeSize: TPoint       read fCurrentLargeSize;

    property SettingsCount: Integer read GetSettingsCount;
    property Settings[const aIndex: Integer]: TSettingsItem read GetSettings write SetSettings;

    function Draw: TDrawResult;
    procedure Resize;
    procedure Update;
    procedure GetSettings;
    procedure SetSettings;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;

    procedure SaveToFile(const aSection: TmcfSection);
    procedure LoadFromFile(const aSection: TmcfSection);

    procedure InitLib(const aLibPath: String);
    procedure FreeLib;

    constructor Create(const aOwner: THardwareMonitor);
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TDisplayInfoDynArr = array of TDisplayInfo;
  THardwareMonitorDynArr = array of THardwareMonitor;

  { TDisplays }

  TDisplays = class(TObject)
  private
    fDisplays: TDisplayInfoDynArr;
    fHardwareMonitor: THardwareMonitorDynArr;
    function GetCount: Integer;
    function GetDisplay(const aIndex: Integer): TDisplayInfo;
    function GetHardwareMonitor(const aIndex: Integer): THardwareMonitor;
  public
    property Count: Integer read GetCount;
    property Display[const aIndex: Integer]: TDisplayInfo read GetDisplay; default;
    property HardwareMonitor[const aIndex: Integer]: THardwareMonitor read GetHardwareMonitor;

    function ActivateHardwareMonitor(const aIndex: Integer): THardwareMonitor;
    procedure DeactivateHardwareMonitor(const aIndex: Integer);

    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TModulesInfoExDynArr = array of TModulInfoEx;
  TModulesInfo = class(TObject)
  private
    fSearchSubFolders: Boolean;
    fModulPath: String;
    fModules: TModulesInfoExDynArr;
    procedure UpdateModules;

    function GetCount: Integer;
    function GetModul(const aIndex: Integer): TModulInfoEx;

    procedure SetSearchSubFolders(const aValue: Boolean);
    procedure SetModulPath(const aValue: String);
  public
    property SearchSubFolders: Boolean read fSearchSubFolders write SetSearchSubFolders;
    property ModulPath       : String  read fModulPath        write SetModulPath;
    property Count: Integer read GetCount;
    property Module[const aIndex: Integer]: TModulInfoEx read GetModul; default;
    constructor Create;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{.$DEFINE COMPILE_HM}
{$IFDEF COMPILE_HM}
  EHardwareMonitor = class(Exception);
  THardwareMonitor = class(TThread)
  private
    fModulPath: String;
    fActiveDisplay: TUSBDisplay;
    fBackground: TBitmap;
    fFrameBuffer: TBitmap;
    fIdleEvent: TEvent;
    fCanWorkEvent: TEvent;
    fSuspended: Boolean;
    fSleepCount: Integer;
    fLargeModul: THardwareMonitorModul;
    fCanWork: TEvent;
    fTimer: TTimer;

    fFrameBufferCS: TRTLCriticalSection;
    fActiveModulesCS: TRTLCriticalSection;
    fOnUpdate: TNotifyEvent;

    procedure SetSuspended(const aValue: Boolean);
    procedure SetModulPath(const aValue: String);
    procedure SetUpdateTime(const aValue: Cardinal);

    function GetModules(const aIndex: Integer): TModulInfoEx;
    function GetModulCount: Integer;
    function GetActiveModul(const aIndex: Integer): THardwareMonitorModul;
    function GetActiveModulCount: Integer;
    function GetDisplays(const aIndex: Integer): TDisplayInfo;
    function GetDisplayCount: Integer;
    function GetUpdateTime: Cardinal;

    procedure FreeModules;
    procedure FreeDisplays;
    procedure DoUpdate;

    procedure OnTouch(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode);
    procedure OnTimer(aSender: TObject);
  protected
    procedure UpdateDisplay;
    procedure Execute; override;
  public
    property Modulpath    : String      read fModulPath    write SetModulPath;
    property UpdateTime   : Cardinal    read GetUpdateTime write SetUpdateTime;
    property Suspended    : Boolean     read fSuspended    write SetSuspended;
    property ActiveDisplay: TUSBDisplay read fActiveDisplay;
    property FrameBuffer  : TBitmap     read fFrameBuffer;

    property Modules[const aIndex: Integer]: TModulInfoEx read GetModules;
    property ModulCount: Integer read GetModulCount;
    property ActiveModules[const aIndex: Integer]: THardwareMonitorModul read GetActiveModul;
    property ActiveModulCount: Integer read GetActiveModulCount;
    property Displays[const aIndex: Integer]: TDisplayInfo read GetDisplays;
    property DisplayCount: Integer read GetDisplayCount;
    property LargeModul: THardwareMonitorModul read fLargeModul;

    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;

    procedure Init;
    procedure EnableModul(const aIndex: Integer);
    procedure DisableModul(const aIndex: Integer);
    procedure ActivateDisplay(const aIndex: Integer);
    procedure SetBackground(const aGraphic: TGraphic);
    procedure EmulateTouch(const aPoint: TPoint);
    function GetActiveModul(const aName: String): THardwareMonitorModul;

    procedure LockFrameBuffer;
    procedure UnlockFrameBuffer;

    procedure SaveToFile(const aSection: TmcfSection);
    procedure LoadFromFile(const aSection: TmcfSection);

    procedure Resume;
    procedure Suspend;
    procedure Terminate;

    constructor Create;
    destructor Destroy; override;
  end;
{$ELSE}
  TModules = specialize TFPGObjectList<THardwareMonitorModul>;

  { TUpdateThread }

  TUpdateThread = class(TThread)
  private
    fModules: TModules;
    fCritSec: TRTLCriticalSection;
    fCycleEvent: TEvent;
    fUpdateDoneEvent: TNotifyEvent;
    procedure DoUpdateDone;
  protected
    procedure Execute; override;
  public
    procedure DoUpdate;
    procedure Terminate;

    constructor Create(const aModules: TModules; const aCritSec: TRTLCriticalSection; const aUpdateDoneEvent: TNotifyEvent);
    destructor Destroy; override;
  end;

  { THardwareMonitor }

  THardwareMonitor = class(TUSBDisplay)
  private
    fFrameBufferCS: TRTLCriticalSection;
    fModulesCS: TRTLCriticalSection;
    fModules: TModules;
    fBackground: TBitmap;
    fLargeModul: THardwareMonitorModul;
    fUpdateThread: TUpdateThread;
    fTimer: TTimer;

    function GetModulCount: Integer;
    function GetModul(const aIndex: Integer): THardwareMonitorModul;
    function GetUpdateRate: Cardinal;
    procedure SetUpdateRate(const aValue: Cardinal);
    procedure ThreadUpdateDone(aSender: TObject);
    procedure TimerTick(aSender: TObject);
  protected
    procedure TouchEvent(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode); override;
  public
    property ModulCount: Integer read GetModulCount;
    property Modules[const aIndex: Integer]: THardwareMonitorModul read GetModul;
    property UpdateRate: Cardinal read GetUpdateRate write SetUpdateRate;
    property LargeModul: THardwareMonitorModul read fLargeModul;
    property Background: TBitmap read fBackground;

    function AddModul(const aLibFile: String): THardwareMonitorModul;
    function GetModulByName(const aName: String): THardwareMonitorModul;
    procedure DelModul(const aIndex: Integer);

    procedure UpdateFrameBuffer;
    procedure Update(const aUpdateFrameBuffer: Boolean = false);
    procedure SetBackground(const aGraphic: TGraphic);
    procedure SetSize(AWidth, AHeight: integer); override;
    procedure EmulateTouch(const aPoint: TPoint);

    procedure LockFrameBuffer;
    procedure UnlockFrameBuffer;

    procedure SaveToFile(const aSection: TmcfSection);
    procedure LoadFromFile(const aSection: TmcfSection; const aModules: TModulesInfo);

    constructor Create(const aDisplayID: Integer);
    destructor Destroy; override;
  end;
{$ENDIF}

implementation

function GdiAlphaBlend(hdcDest: HDC; xDest, yDest, wDest, hDest: Integer;
  hdcSrc: HDC; xSrc, ySrc, wSrc, hSrc: Integer; ftn: TBlendFunction): Boolean; stdcall; external 'gdi32.dll';

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUpdateThread.DoUpdateDone;
begin
  if Assigned(fUpdateDoneEvent) then
    fUpdateDoneEvent(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUpdateThread.Execute;
var
  i: Integer;
begin
  while not Terminated do begin
    if not Assigned(fCycleEvent) then
      exit;
    fCycleEvent.WaitFor(INFINITE);
    if not Assigned(fCycleEvent) then
      exit;
    fCycleEvent.ResetEvent;
    EnterCriticalSection(fCritSec);
    try
      for i := 0 to fModules.Count-1 do begin
        fModules[i].Resize;
        fModules[i].Update;
      end;
    finally
      LeaveCriticalSection(fCritSec);
    end;
    if Assigned(fUpdateDoneEvent) then
      Synchronize(@DoUpdateDone);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUpdateThread.DoUpdate;
begin
  if Assigned(fCycleEvent) then
    fCycleEvent.SetEvent;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUpdateThread.Terminate;
begin
  inherited Terminate;
  DoUpdate;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TUpdateThread.Create(const aModules: TModules; const aCritSec: TRTLCriticalSection; const aUpdateDoneEvent: TNotifyEvent);
begin
  inherited Create(true);
  fCycleEvent := TEvent.Create(nil, true, false, '');
  fModules := aModules;
  fCritSec := aCritSec;
  fUpdateDoneEvent := aUpdateDoneEvent;
  Resume;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TUpdateThread.Destroy;
begin
  fCycleEvent.SetEvent;
  FreeAndNil(fCycleEvent);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//THardwareMonitorModul/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetSmallSize(const aValue: TPoint);
begin
  EnterCriticalSection(fDataCS);
  try
    fSmallSize := aValue;
    if fSmallSize.x < 20 then
      fSmallSize.x := 20;
    if fSmallSize.y < 20 then
      fSmallSize.y := 20;
    NeedResize := true;
  finally
    LeaveCriticalSection(fDataCS);
  end;
  UpdateRect;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetLargeSize(const aValue: TPoint);
begin
  EnterCriticalSection(fDataCS);
  try
    fLargeSize := aValue;
    if fLargeSize.x < 20 then
      fLargeSize.x := 20;
    if fLargeSize.y < 20 then
      fLargeSize.y := 20;
    NeedResize := true;
  finally
    LeaveCriticalSection(fDataCS);
  end;
  UpdateRect;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetPosition(const aValue: TPoint);
begin
  EnterCriticalSection(fDataCS);
  try
    fPosition := aValue;
    if Assigned(fOwner) then
      fOwner.Update(true);
  finally
    LeaveCriticalSection(fDataCS);
  end;
  UpdateRect;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetIsSmall(const aValue: Boolean);
begin
  EnterCriticalSection(fDataCS);
  try
    if fIsSmall <> aValue then begin
      fIsSmall := aValue;
      NeedResize := true;
    end;
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetNeedResize(const aValue: Boolean);
begin
  if fNeedResize <> aValue then begin
    EnterCriticalSection(fDataCS);
    try
      fNeedResize := aValue;
      if Assigned(fOwner) then
        fOwner.Update(true);
    finally
      LeaveCriticalSection(fDataCS);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetName(const aValue: String);
begin
  EnterCriticalSection(fDataCS);
  try
    if Assigned(fOwner.GetModulByName(aValue)) then
      raise EHardwareMonitorModul.Create('Name already taken!');
    fName := aValue;
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitorModul.GetSettingsCount: Integer;
begin
  EnterCriticalSection(fDataCS);
  try
    result := Length(fSettingsArr);
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitorModul.GetSettings(const aIndex: Integer): TSettingsItem;
begin
  EnterCriticalSection(fDataCS);
  try
    if (aIndex >= 0) and (aIndex < Length(fSettingsArr)) then
      result := fSettingsArr[aIndex]
    else
      raise EHardwareMonitorModul.Create(format('GetSettings: index out of bounds (%d)', [aIndex]));
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetSettings(const aIndex: Integer; aValue: TSettingsItem);
begin
  EnterCriticalSection(fDataCS);
  try
    if (aIndex >= 0) and (aIndex < Length(fSettingsArr)) then
      fSettingsArr[aIndex] := aValue
    else
      raise EHardwareMonitorModul.Create(format('SetSettings: index out of bounds (%d)', [aIndex]));
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.UpdateRect;
begin
  EnterCriticalSection(fDataCS);
  try
    fRect := Classes.Rect(
      fPosition.x, fPosition.y,
      fPosition.x + fSmallSize.x,
      fPosition.y + fSmallSize.y);
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.ClearSettings;
var
  i: Integer;
begin
  EnterCriticalSection(fDataCS);
  try
    for i := 0 to High(fSettingsArr) do with fSettingsArr[i] do begin
      case DataType of
        dtInt32:
          Dispose(PInteger(Data));
        dtFloat32:
          Dispose(PSingle(Data));
        dtBool:
          Dispose(PBoolean(Data));
        dtString, dtFile, dtPicture, dtIdentStr:
          Dispose(PString(Data));
        dtColor:
          Dispose(PCardinal(Data));
        dtFont:
          Dispose(PModulFontDataEx(Data));
        dtByte:
          Dispose(PByte(Data));
      end;
    end;
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitorModul.Draw: TDrawResult;
var
  small: Boolean;
begin
  EnterCriticalSection(fDataCS);
  try
    small := fIsSmall;
  finally
    LeaveCriticalSection(fDataCS);
  end;

  EnterCriticalSection(fEventCS);
  try
    FillChar(result, SizeOf(result), 0);
    if Assigned(fDraw) then
      result := fDraw(fObjHandle);
  finally
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.Resize;
begin
  if not fNeedResize then
    exit;

  EnterCriticalSection(fDataCS);
  try
    fCurrentIsSmall   := fIsSmall;
    fCurrentSmallSize := fSmallSize;
    fCurrentLargeSize := fLargeSize;
  finally
    LeaveCriticalSection(fDataCS);
  end;

  EnterCriticalSection(fEventCS);
  try
    if Assigned(fResize) then begin
      fResize(fObjHandle, fCurrentIsSmall,
        fCurrentSmallSize.x, fCurrentSmallSize.y,
        fCurrentLargeSize.x, fCurrentLargeSize.y);
      fNeedResize := false;
    end;
  finally
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.Update;
begin
  EnterCriticalSection(fEventCS);
  try
    if Assigned(fUpdate) then
      fUpdate(fObjHandle);
  finally
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.GetSettings;
var
  i: Integer;
  c: Integer;
  p: PSettingsItem;
begin
  if not Assigned(fGetSettings) then
    exit;
  ClearSettings;
  EnterCriticalSection(fDataCS);
  EnterCriticalSection(fEventCS);
  try
    fGetSettings(fObjHandle, c, p);
    SetLength(fSettingsArr, c);
    for i := 0 to High(fSettingsArr) do with fSettingsArr[i] do begin
      Name     := p^.Name;
      Min      := p^.Min;
      Max      := p^.Max;
      DataType := p^.DataType;
      case DataType of
        dtInt32: begin
          New(PInteger(Data));
          PInteger(Data)^ := PInteger(p^.Data)^;
        end;
        dtFloat32: begin
          New(PSingle(Data));
          PSingle(Data)^ := PSingle(p^.Data)^;
        end;
        dtBool: begin
          New(PBoolean(Data));
          PBoolean(Data)^ := PBoolean(p^.Data)^;
        end;
        dtString, dtFile, dtPicture, dtIdentStr: begin
          New(PString(Data));
          PString(Data)^ := PChar(p^.Data);
        end;
        dtColor: begin
          New(PCardinal(Data));
          PCardinal(Data)^ := PCardinal(p^.Data)^;
        end;
        dtFont: begin
          New(PModulFontDataEx(Data));
          PModulFontDataEx(Data)^.Color := PModulFontData(p^.Data)^.Color;
          PModulFontDataEx(Data)^.Name  := PModulFontData(p^.Data)^.Name;
          PModulFontDataEx(Data)^.Style := PModulFontData(p^.Data)^.Style;
          PModulFontDataEx(Data)^.Size  := PModulFontData(p^.Data)^.Size;
        end;
        dtByte: begin
          New(PByte(Data));
          PByte(Data)^ := PByte(p^.Data)^;
        end;
      end;
      inc(p);
    end;
  finally
    LeaveCriticalSection(fDataCS);
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SetSettings;
var
  arr: array of TSettingsItem;

  p: PSettingsItem;
  i: Integer;
begin
  if not Assigned(fSetSettings) then
    exit;
  EnterCriticalSection(fEventCS);
  try
    SetLength(arr, Length(fSettingsArr));
    p := @fSettingsArr[0];
    for i := 0 to High(arr) do with arr[i] do begin
      Name     := p^.Name;
      Min      := p^.Min;
      Max      := p^.Max;
      DataType := p^.DataType;
      case DataType of
        dtInt32: begin
          New(PInteger(Data));
          PInteger(Data)^ := PInteger(p^.Data)^;
        end;
        dtFloat32: begin
          New(PSingle(Data));
          PSingle(Data)^ := PSingle(p^.Data)^;
        end;
        dtBool: begin
          New(PBoolean(Data));
          PBoolean(Data)^ := PBoolean(p^.Data)^;
        end;
        dtString, dtFile, dtPicture, dtIdentStr: begin
          PChar(Data) := PChar(PString(p^.Data)^);
        end;
        dtColor: begin
          New(PCardinal(Data));
          PCardinal(Data)^ := PCardinal(p^.Data)^;
        end;
        dtFont: begin
          New(PModulFontData(Data));
          PModulFontData(Data)^.Color := PModulFontDataEx(p^.Data)^.Color;
          PModulFontData(Data)^.Name  := PChar(PModulFontDataEx(p^.Data)^.Name);
          PModulFontData(Data)^.Style := PModulFontDataEx(p^.Data)^.Style;
          PModulFontData(Data)^.Size  := PModulFontDataEx(p^.Data)^.Size;
        end;
        dtByte: begin
          New(PByte(Data));
          PByte(Data)^ := PByte(p^.Data)^;
        end;
      end;
      inc(p);
    end;
    fSetSettings(fObjHandle, @arr[0]);
    for i := 0 to High(arr) do with arr[i] do begin
      case DataType of
        dtInt32:
          Dispose(PInteger(Data));
        dtFloat32:
          Dispose(PSingle(Data));
        dtBool:
          Dispose(PBoolean(Data));
        dtString, dtFile, dtPicture, dtIdentStr:
          PChar(Data) := '';
        dtColor:
          Dispose(PCardinal(Data));
        dtFont:
          Dispose(PModulFontData(Data));
        dtByte:
          Dispose(PByte(Data));
      end;
    end;
    SetLength(arr, 0);
  finally
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitorModul.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;
begin
  EnterCriticalSection(fEventCS);
  try
    result := false;
    if Assigned(fSendTouchReport) then
      result := fSendTouchReport(fObjHandle, aPoint, aPressure, aMode);
  finally
    LeaveCriticalSection(fEventCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.SaveToFile(const aSection: TmcfSection);

  procedure SaveFont(const aSection: TmcfSection; const aFont: TModulFontDataEx);
  begin
    with aSection do begin
      SetString('Name', aFont.Name);
      SetInt('Color', aFont.Color);
      SetInt('Size', aFont.Size);
      SetInt('Style', aFont.Style)
    end;
  end;

var
  i: Integer;
begin
  EnterCriticalSection(fDataCS);
  try
    aSection.SetString('Name', fName);
    aSection.SetString('LibName', fModulInfoEx.libName);
    aSection.SetString('LibVersion', fModulInfoEx.Version);
    with aSection.Sections['Position'] do begin
      SetInt('x', Position.x);
      SetInt('y', Position.y);
    end;
    with aSection.Sections['SmallSize'] do begin
      SetInt('x', SmallSize.x);
      SetInt('y', SmallSize.y);
    end;
    with aSection.Sections['Modul'] do begin
      GetSettings;
      for i := 0 to High(fSettingsArr) do begin
        case fSettingsArr[i].DataType of
          dtInt32, dtByte:
            SetInt(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'), PInteger(fSettingsArr[i].Data)^);
          dtFloat32:
            SetFloat(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'), PSingle(fSettingsArr[i].Data)^);
          dtBool:
            SetBool(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'), Boolean(PBoolean(fSettingsArr[i].Data)^));
          dtString, dtFile, dtPicture, dtIdentStr:
            SetString(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'), PString(fSettingsArr[i].Data)^);
          dtColor:
            SetInt(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'), PCardinal(fSettingsArr[i].Data)^);
          dtFont:
            SaveFont(Sections[mcfEscapeSpecChars(fSettingsArr[i].Name, '_')], PModulFontDataEx(fSettingsArr[i].Data)^);
        end;
      end;
    end;
  finally
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.LoadFromFile(const aSection: TmcfSection);
var
  lib: String;
  i: Integer;
begin
  EnterCriticalSection(fDataCS);
  EnterCriticalSection(fEventCS);
  try
    fName := aSection.GetString('Name', fName);
    with aSection.Sections['Position'] do begin
      fPosition.x := GetInt('x', fPosition.x);
      fPosition.y := GetInt('y', fPosition.y);
    end;
    with aSection.Sections['SmallSize'] do begin
      fSmallSize.x := GetInt('x', fSmallSize.x);
      fSmallSize.y := GetInt('y', fSmallSize.y);
    end;
    with aSection.Sections['Modul'] do begin
      for i := 0 to High(fSettingsArr) do begin
        case fSettingsArr[i].DataType of
          dtInt32: begin
            PInteger(fSettingsArr[i].Data)^ := GetInt(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              PInteger(fSettingsArr[i].Data)^);
            if (fSettingsArr[i].Min <> 0) or (fSettingsArr[i].Max <> 0) then begin
              if (PInteger(fSettingsArr[i].Data)^ < fSettingsArr[i].Min) then
                PInteger(fSettingsArr[i].Data)^ := Round(fSettingsArr[i].Min);
              if (PInteger(fSettingsArr[i].Data)^ > fSettingsArr[i].Max) then
                PInteger(fSettingsArr[i].Data)^ := Round(fSettingsArr[i].Max);
            end;
          end;
          dtByte: begin
            PByte(fSettingsArr[i].Data)^ := GetInt(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              PByte(fSettingsArr[i].Data)^);
            if (fSettingsArr[i].Min <> 0) or (fSettingsArr[i].Max <> 0) then begin
              if (PByte(fSettingsArr[i].Data)^ < fSettingsArr[i].Min) then
                PByte(fSettingsArr[i].Data)^ := Round(fSettingsArr[i].Min);
              if (PByte(fSettingsArr[i].Data)^ > fSettingsArr[i].Max) then
                PByte(fSettingsArr[i].Data)^ := Round(fSettingsArr[i].Max);
            end;
          end;
          dtFloat32: begin
            PSingle(fSettingsArr[i].Data)^ := GetFloat(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              PSingle(fSettingsArr[i].Data)^);
            if (fSettingsArr[i].Min <> 0) or (fSettingsArr[i].Max <> 0) then begin
              if (PSingle(fSettingsArr[i].Data)^ < fSettingsArr[i].Min) then
                PSingle(fSettingsArr[i].Data)^ := fSettingsArr[i].Min;
              if (PSingle(fSettingsArr[i].Data)^ > fSettingsArr[i].Max) then
                PSingle(fSettingsArr[i].Data)^ := fSettingsArr[i].Max;
            end;
          end;
          dtBool: begin
            PBoolean(fSettingsArr[i].Data)^ := Byte(GetBool(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              Boolean(PBoolean(fSettingsArr[i].Data)^)));
          end;
          dtString, dtFile, dtPicture, dtIdentStr:
            PString(fSettingsArr[i].Data)^ := GetString(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              PString(fSettingsArr[i].Data)^);
          dtColor:
            PCardinal(fSettingsArr[i].Data)^ := GetInt(mcfEscapeSpecChars(fSettingsArr[i].Name, '_'),
              PCardinal(fSettingsArr[i].Data)^);
          dtFont: begin

          end;
        end;
      end;
    end;
  finally
    LeaveCriticalSection(fDataCS);
    LeaveCriticalSection(fEventCS);
  end;
  SetSettings;
  NeedResize := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.InitLib(const aLibPath: String);

  function LoadProcedure(const aName: String): Pointer;
  begin
    result := GetProcAddress(fLibHandle, PChar(aName));
    if result = nil then
      raise EHardwareMonitorModul.Create(format('unable to load procedure from lib: ''%s'' error code: %d', [aName, GetLastError]));
  end;

  procedure CheckName(var aName: String);
  var
    o: THardwareMonitorModul;
    i: Integer;
  begin
    o := fOwner.GetModulByName(aName);
    if Assigned(o) then begin
      i := 0;
      while Assigned(o) do begin
        inc(i);
        o := fOwner.GetModulByName(aName + ' ' + IntToStr(i));
      end;
      aName := aName + ' ' + IntToStr(i);
    end;
  end;

var
  info: TModulInfo;
  mName: String;
begin
  EnterCriticalSection(fDataCS);
  EnterCriticalSection(fEventCS);
  try
    fLibHandle := LoadLibrary(PChar(aLibPath));
    try
      if fLibHandle > 0 then begin
        fInitLibData     := TInitLibDataProc(LoadProcedure('InitLibData'));
        fFreeLibData     := TFreeLibDataProc(LoadProcedure('FreeLibData'));
        fModulInfo       := TModulInfoProc(LoadProcedure('ModulInfo'));
        fCreateModul     := TCreateModulProc(LoadProcedure('CreateModul'));
        fFreeModul       := TFreeModulProc(LoadProcedure('FreeModul'));
        fResize          := TResizeProc(LoadProcedure('Resize'));
        fUpdate          := TUpdateProc(LoadProcedure('Update'));
        fDraw            := TDrawProc(LoadProcedure('Draw'));
        fSetSettings     := TSetSettingsProc(LoadProcedure('SetSettings'));
        fGetSettings     := TGetSettingsProc(LoadProcedure('GetSettings'));
        fSendTouchReport := TSendTouchReportProc(LoadProcedure('SendTouchReport'));

        info := fModulInfo();
        with fModulInfoEx do begin
          libName := ExtractFileName(aLibPath);
          Name    := info.Name;
          Autor   := info.Autor;
          eMail   := info.eMail;
          Version := info.Version;
          Year    := info.Year;
        end;
        mName := fModulInfoEx.Name + ' ' + fModulInfoEx.Version;
        if Assigned(fOwner) then
          CheckName(mName);
        Name := mName;

        fInitLibData();
        fObjHandle := fCreateModul();
        if fObjHandle = nil then
          raise EHardwareMonitorModul.Create('unable to create lib object');
        fNeedResize := true;
      end else
        raise EHardwareMonitorModul.Create(format('unable to load lib: ''%s'' error code: %d', [aLibPath, GetLastError]));
    except
      fInitLibData     := nil;
      fFreeLibData     := nil;
      fCreateModul     := nil;
      fFreeModul       := nil;
      fResize          := nil;
      fUpdate          := nil;
      fDraw            := nil;
      fSetSettings     := nil;
      fGetSettings     := nil;
      fSendTouchReport := nil;
      if fLibHandle > 0 then
        FreeLibrary(fLibHandle);
    end;
  finally
    LeaveCriticalSection(fDataCS);
    LeaveCriticalSection(fEventCS);
  end;
  if Assigned(fObjHandle) then begin
    GetSettings;
    Resize;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitorModul.FreeLib;
begin
  EnterCriticalSection(fEventCS);
  EnterCriticalSection(fDataCS);
  try
    if fLibHandle > 0 then begin
      if Assigned(fObjHandle) then
        fFreeModul(fObjHandle);
      fObjHandle := nil;
      fFreeLibData();
      FreeLibrary(fLibHandle);
    end;
    fInitLibData     := nil;
    fFreeLibData     := nil;
    fCreateModul     := nil;
    fFreeModul       := nil;
    fResize          := nil;
    fUpdate          := nil;
    fDraw            := nil;
    fSetSettings     := nil;
    fGetSettings     := nil;
    fSendTouchReport := nil;
  finally
    LeaveCriticalSection(fEventCS);
    LeaveCriticalSection(fDataCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor THardwareMonitorModul.Create(const aOwner: THardwareMonitor);
begin
  inherited Create;
  fOwner := aOwner;
  InitCriticalSection(fDataCS);
  InitCriticalSection(fEventCS);
  SetPosition(Classes.Point(0, 0));
  SetSmallSize(Classes.Point(480, 200));
  SetLargeSize(Classes.Point(480, 272));
  SetLength(fSettingsArr, 0);
  fIsSmall := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor THardwareMonitorModul.Destroy;
begin
  ClearSettings;
  FreeLib;
  DoneCriticalsection(fDataCS);
  DoneCriticalsection(fEventCS);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TModulesInfo//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TModulesInfo.UpdateModules;
var
  dllList: TStringList;

  procedure SearchPath(const aPath: String);
  var
    sr: TSearchRec;
    Found: Integer;
  begin
    Found := FindFirst(aPath+'*.*', faAnyFile, sr);
    while Found = 0 do begin
      if (sr.Name <> '.') and (sr.Name <> '..') then begin
        if LowerCase(ExtractFileExt(sr.Name)) = '.dll' then
          dllList.Add(aPath+sr.Name);
        if DirectoryExists(aPath + sr.Name) then
          SearchPath(aPath + sr.Name + '\');
      end;
      Found := FindNext(sr);
    end;
    SysUtils.FindClose(sr);
  end;

var
  i: Integer;
  lib: Cardinal;
  p: TModulInfoProc;
  mInfo: TModulInfo;
begin
  dllList := TStringList.Create;
  try
    SearchPath(fModulPath);
    SetLength(fModules, dllList.Count);
    for i := 0 to High(fModules) do begin
      lib := LoadLibrary(PAnsiChar(dllList[i]));
      if (lib <> 0) then begin
        p := TModulInfoProc(GetProcAddress(lib, 'ModulInfo'));
        if Assigned(p) then begin
          mInfo := p();
          fModules[i].libName := dllList[i];
          fModules[i].Name    := mInfo.Name;
          fModules[i].Autor   := mInfo.Autor;
          fModules[i].eMail   := mInfo.eMail;
          fModules[i].Version := mInfo.Version;
          fModules[i].Year    := mInfo.Year;
        end else
          raise Exception.Create(format(
            'unable to load procedure ''ModulInfo'' from lib ''%s'' error code: %d',
            [dllList[i], GetLastError]));
      end else
        raise Exception.Create(format(
          'unable to load lib: ''%s'' error code: %d',
          [dllList[i], GetLastError]));
    end;
  finally
    FreeAndNil(dllList);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TModulesInfo.GetCount: Integer;
begin
  result := Length(fModules);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TModulesInfo.GetModul(const aIndex: Integer): TModulInfoEx;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := fModules[aIndex]
  else
    raise Exception.Create(format('TModules.GetModul: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TModulesInfo.SetSearchSubFolders(const aValue: Boolean);
begin
  fSearchSubFolders := aValue;
  UpdateModules;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TModulesInfo.SetModulPath(const aValue: String);
begin
  fModulPath := aValue;
  if (length(fModulPath) > 0) and (fModulPath[Length(fModulPath)] <> '\') then
    fModulPath := fModulPath + '\';
  UpdateModules;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TModulesInfo.Create;
begin
  inherited Create;
  fModulPath := '';
  fSearchSubFolders := true;
  SetLength(fModules, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TDisplays/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDisplays.GetCount: Integer;
begin
  result := Length(fDisplays);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDisplays.GetDisplay(const aIndex: Integer): TDisplayInfo;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := fDisplays[aIndex]
  else
    raise Exception.Create(format('TDisplays.GetDisplay: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDisplays.GetHardwareMonitor(const aIndex: Integer): THardwareMonitor;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := fHardwareMonitor[aIndex]
  else
    raise Exception.Create(format('TDisplays.GetHardwareMonitor: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDisplays.ActivateHardwareMonitor(const aIndex: Integer): THardwareMonitor;
begin
  if (aIndex >= 0) and (aIndex < Count) then begin
    if not Assigned(fHardwareMonitor[aIndex]) then
      fHardwareMonitor[aIndex] := THardwareMonitor.Create(aIndex-1);
    result := fHardwareMonitor[aIndex];
  end else
    raise Exception.Create(format('TDisplays.ActivateHardwareMonitor: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDisplays.DeactivateHardwareMonitor(const aIndex: Integer);
begin
  if (aIndex >= 0) and (aIndex < Count) then begin
    if Assigned(fHardwareMonitor[aIndex]) then
      FreeAndNil(fHardwareMonitor[aIndex]);
  end else
    raise Exception.Create(format('TDisplays.ActivateHardwareMonitor: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TDisplays.Create;
var
  i, c: Integer;
begin
  inherited Create;
  USBD480_Init();
  c := USBD480_GetNumberOfDisplays();
  SetLength(fDisplays, c+1);
  SetLength(fHardwareMonitor, c+1);
  FillChar(fDisplays[0],        SizeOf(fDisplays[0])        * Length(fDisplays), #0);
  FillChar(fHardwareMonitor[0], SizeOf(fHardwareMonitor[0]) * Length(fDisplays), #0);
  with fDisplays[0] do begin
    Name         := 'Dummy Display';
    Width        := 480;
    Height       := 272;
    SerialNumber := '0123456789D';
  end;
  for i := 1 to c do begin
    USBD480_GetDisplayConfiguration(i-1, @fDisplays[i]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TDisplays.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fHardwareMonitor) do
    if Assigned(fHardwareMonitor[i]) then
      FreeAndNil(fHardwareMonitor[i]);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//THardwareMonitor//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{$IFDEF COMPILE_HM}
procedure THardwareMonitor.SetSuspended(const aValue: Boolean);
begin
  if fSuspended = aValue then
    exit;

  if fSuspended then begin
    fCanWorkEvent.SetEvent;
    fSuspended := false;
  end else begin
    fCanWorkEvent.ResetEvent;
    if fIdleEvent.WaitFor(5000) <> wrSignaled then
      raise EHardwareMonitor.Create('timeout while waiting for thread');
    fSuspended := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetModulPath(const aValue: String);
var
  dllList: TStringList;
  sr: TSearchRec;
  found, i: Integer;
  lHandle: Cardinal;
  p: TModulInfoProc;
  s: Boolean;
  mInfo: TModulInfo;
begin
  if fModulPath = aValue then
    exit;

  s := Suspended;
  if not s then
    Suspend;
  try
    fModulPath := aValue;
    if (Length(fModulPath) > 0) and (fModulPath[Length(fModulPath)] <> '\') then
      fModulPath := fModulPath + '\';
    dllList := TStringList.Create;
    try
      found := SysUtils.FindFirst(fModulPath+'*.*', faAnyFile, sr);
      while found = 0 do begin
        if (sr.Name <> '.') and (sr.Name <> '..') and
            FileExists(fModulPath + sr.Name) and
            (LowerCase(ExtractFileExt(sr.Name)) = '.dll') then
          dllList.Add(sr.Name);
        found := SysUtils.FindNext(sr);
      end;
      SysUtils.FindClose(sr);

      FreeModules;
      SetLength(fModules, dllList.Count);
      if Length(fModules) > 0 then begin
        FillChar(fModules[0], SizeOf(fModules[0])*Length(fModules), 0);
        for i := 0 to High(fModules) do begin
          lHandle := LoadLibrary(PChar(fModulPath + dllList[i]));
          if lHandle > 0 then begin
            try
              p := TModulInfoProc(GetProcAddress(lHandle, 'ModulInfo'));
              if Assigned(p) then begin
                mInfo := p();
                fModules[i].libName := dllList[i];
                fModules[i].Name    := mInfo.Name;
                fModules[i].Autor   := mInfo.Autor;
                fModules[i].eMail   := mInfo.eMail;
                fModules[i].Version := mInfo.Version;
                fModules[i].Year    := mInfo.Year;
              end else
                raise EHardwareMonitor.Create(format(
                  'unable to load procedure ''ModulInfo'' from lib ''%s'' error code: %d',
                  [dllList[i], GetLastError]));
            finally
              FreeLibrary(lHandle);
            end;
          end else
            raise EHardwareMonitor.Create(
              format('unable to load lib: ''%s'' error code: %d',
                [dllList[i], GetLastError]));
        end;
      end;
    finally
      dllList.Free;
    end;
  finally
    if not s then
      Resume;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetUpdateTime(const aValue: Cardinal);
begin
  fTimer.Interval := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetModules(const aIndex: Integer): TModulInfoEx;
begin
  if (aIndex >= 0) and (aIndex < Length(fModules)) then
    result := fModules[aIndex]
  else
    raise EHardwareMonitor.Create(format('modul index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetModulCount: Integer;
begin
  result := Length(fModules);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetActiveModul(const aIndex: Integer): THardwareMonitorModul;
begin
  if (aIndex >= 0) and (aIndex < fActiveModules.Count) then
    result := (fActiveModules[aIndex] as THardwareMonitorModul)
  else
    raise EHardwareMonitor.Create(format('active modul index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetActiveModulCount: Integer;
begin
  result := fActiveModules.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetDisplays(const aIndex: Integer): TDisplayInfo;
begin
  if (aIndex >= 0) and (aIndex < Length(fDisplays)) then
    result := fDisplays[aIndex].dInfo
  else
    raise EHardwareMonitor.Create(format('display index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetDisplayCount: Integer;
begin
  result := Length(fDisplays);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetUpdateTime: Cardinal;
begin
  result := fTimer.Interval;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.FreeModules;
var
  i: Integer;
begin
  while fActiveModules.Count > 0 do
    DisableModul(i);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.FreeDisplays;
var
  i: Integer;
begin
  for i := Low(fDisplays) to High(fDisplays) do
    if Assigned(fDisplays[i].Obj) then
      FreeAndNil(fDisplays[i].Obj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.DoUpdate;
begin
  if Assigned(fOnUpdate) then
    fOnUpdate(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.OnTouch(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode);
var
  p: TPoint;
  i: Integer;
  obj: THardwareMonitorModul;
begin
  if Assigned(fLargeModul) then begin
    if fLargeModul.SendTouchReport(aPoint, aPressure, aMode) then begin
      fLargeModul.IsSmall := true;
      fLargeModul := nil;
    end;
  end else begin
    EnterCriticalSection(fActiveModulesCS);
    try
      for i := fActiveModules.Count-1 downto 0 do begin
        obj := (fActiveModules[i] as THardwareMonitorModul);
        with obj  do begin
          if (aPoint.X >= Position.x) and
             (aPoint.Y >= Position.y) and
             (aPoint.X <= Position.x + SmallSize.x) and
             (aPoint.Y <= Position.y + SmallSize.y) then begin
            p := aPoint;
            p.X := aPoint.X - Position.x;
            p.Y := aPoint.Y - Position.y;
            if SendTouchReport(p, aPressure, aMode) then begin
              fLargeModul := obj;
              fLargeModul.IsSmall := false;
              exit;
            end;
            Continue;
          end;
        end;
      end;
    finally
      LeaveCriticalSection(fActiveModulesCS);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.OnTimer(aSender: TObject);
begin
  Resume;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.UpdateDisplay;
var
  i: Integer;
  o: THardwareMonitorModul;
  f: TBlendFunction;
  bmp: TBitmap;
  dRes: TDrawResult;

  procedure DrawModul(const aModul: THardwareMonitorModul);
  var
    p, s: Point;
  begin
    dRes := aModul.Draw;
    if dRes.Transparent then
      f.AlphaFormat := AC_SRC_ALPHA
    else
      f.AlphaFormat := 0;
    if aModul.CurrentIsSmall then
      p := aModul.Position
    else
      p := Classes.Point(0, 0);
    if aModul.CurrentIsSmall then
      s := aModul.CurrentSmallSize
    else
      s := aModul.CurrentLargeSize;
    GdiAlphaBlend(fActiveDisplay.Canvas.Handle, p.x, p.y, s.x, s.y,
      dRes.Handle, 0, 0, dRes.Width, dRes.Height, f);
  end;

begin
  if not Assigned(fActiveDisplay) then
    exit;

  fActiveDisplay.Canvas.StretchDraw(Classes.Rect(0, 0,
    fBackground.Width, fBackground.Height), fBackground);

  f.AlphaFormat         := 0;//AC_SRC_ALPHA;
  f.BlendFlags          := 0;
  f.BlendOp             := AC_SRC_OVER;
  f.SourceConstantAlpha := 255;

//UPDATE
  EnterCriticalSection(fActiveModulesCS);
  try
    for i := 0 to fActiveModules.Count-1 do
      with (fActiveModules[i] as THardwareMonitorModul) do begin
        Resize;
        Update;
      end;
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;

//DRAW
  EnterCriticalSection(fActiveModulesCS);
  try
    if Assigned(fLargeModul) then begin
      DrawModul(fLargeModul);
    end else
      for i := 0 to fActiveModules.Count-1 do
        DrawModul(fActiveModules[i] as THardwareMonitorModul);
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;

  LockFrameBuffer;
  try
    fFrameBuffer.Canvas.Draw(0, 0, fActiveDisplay);
  finally
    UnlockFrameBuffer;
  end;
  fActiveDisplay.Update;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Execute;
{var
  t: Cardinal;
  d: Integer;  }
begin
  fCanWorkEvent.WaitFor(INFINITE);
  while not Terminated do begin
    fCanWorkEvent.ResetEvent;
    fIdleEvent.ResetEvent;
    try
      {t := GetTickCount;
      UpdateDisplay;
      d := fUpdateTime - (GetTickCount - t);}
      UpdateDisplay;
    finally
      fSuspended := true;
      fIdleEvent.SetEvent;
    end;
    if Assigned(fOnUpdate) then
      Synchronize(@DoUpdate);
    {if d > 0 then
      Sleep(d);}
    if not Terminated then
      fCanWorkEvent.WaitFor(INFINITE);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Init;
var
  c, i: Integer;
begin
  USBD480_Init;
  c := USBD480_GetNumberOfDisplays();
  FreeDisplays;
  SetLength(fDisplays, c+1);
  FillChar(fDisplays[0], SizeOf(fDisplays[0]) * Length(fDisplays), #0);
  fDisplays[0].dInfo.Name := 'Dummy Display';
  fDisplays[0].dInfo.Width := 480;
  fDisplays[0].dInfo.Height := 272;
  for i := 0 to c-1 do
    USBD480_GetDisplayConfiguration(i, @fDisplays[i+1].dInfo);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.EnableModul(const aIndex: Integer);
var
  obj: THardwareMonitorModul;
begin
  EnterCriticalSection(fActiveModulesCS);
  try
    if (aIndex >= 0) and (aIndex < Length(fModules)) then begin
      obj := THardwareMonitorModul.Create(self);
      obj.InitLib(fModulPath + fModules[aIndex].libName);
      fActiveModules.Add(obj);
      Resume;
    end else
      raise EHardwareMonitor.Create(format('modul index out of bounds (%d)', [aIndex]));
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.DisableModul(const aIndex: Integer);
begin
  EnterCriticalSection(fActiveModulesCS);
  try
    if (aIndex >= 0) and (aIndex < fActiveModules.Count) then begin
      if fActiveModules[aIndex] = fLargeModul then
        fLargeModul := nil;
      fActiveModules.Delete(aIndex);
      Resume;
    end else
      raise EHardwareMonitor.Create(format('modul index out of bounds (%d)', [aIndex]));
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.ActivateDisplay(const aIndex: Integer);
var
  i: Integer;
begin
  FreeDisplays;
  fActiveDisplay := nil;
  if (aIndex >= 0) and (aIndex < Length(fDisplays)) then begin
    fDisplays[aIndex].obj := TUSBDisplay.Create;
    fDisplays[aIndex].obj.Open(aIndex-1);

    EnterCriticalSection(fActiveModulesCS);
    try
      for i := 0 to fActiveModules.Count-1 do
       (fActiveModules[i] as THardwareMonitorModul).LargeSize := Classes.Point(
          fDisplays[aIndex].obj.Width,
          fDisplays[aIndex].obj.Height);
    finally
      LeaveCriticalSection(fActiveModulesCS);
    end;

    fActiveDisplay := fDisplays[aIndex].obj;
    fActiveDisplay.TouchInterval := 100;
    fActiveDisplay.OnTouch := @OnTouch;
    fFrameBuffer.SetSize(fActiveDisplay.Width, fActiveDisplay.Height);
    Resume;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetBackground(const aGraphic: TGraphic);
begin
  fBackground.SetSize(aGraphic.Width, aGraphic.Height);
  fBackground.Canvas.StretchDraw(Classes.Rect(0, 0, aGraphic.Width, aGraphic.Height), aGraphic);
  Resume;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.EmulateTouch(const aPoint: TPoint);
begin
  OnTouch(self, aPoint, 1, tmPenDown);
  OnTouch(self, aPoint, 1, tmPenUp);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetActiveModul(const aName: String): THardwareMonitorModul;
var
  i: Integer;
  obj: THardwareMonitorModul;
begin
  EnterCriticalSection(fActiveModulesCS);
  try
    result := nil;
    for i := 0 to fActiveModules.Count-1 do begin
      obj := fActiveModules[i] as THardwareMonitorModul;
      if (obj.Name = aName) then begin
        result := obj;
        exit;
      end;
    end;
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.LockFrameBuffer;
begin
  EnterCriticalSection(fFrameBufferCS);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.UnlockFrameBuffer;
begin
  LeaveCriticalSection(fFrameBufferCS);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SaveToFile(const aSection: TmcfSection);

  function ToStr(i: Integer): String;
  begin
    result := IntToStr(i);
    while Length(result) < 3 do
      result := '0' + result;
  end;

var
  i: Integer;
begin
  EnterCriticalSection(fActiveModulesCS);
  try
    aSection.SetInt('RefreshRate', fTimer.Interval);
    if Assigned(fActiveDisplay) then
      aSection.SetString('DisplaySerial', fActiveDisplay.DisplayInfo.SerialNumber);
    for i := 0 to fActiveModules.Count-1 do
      (fActiveModules[i] as THardwareMonitorModul).SaveToFile(aSection.Sections['Modul'+ToStr(i)]);
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.LoadFromFile(const aSection: TmcfSection);

  function ToStr(i: Integer): String;
  begin
    result := IntToStr(i);
    while Length(result) < 3 do
      result := '0' + result;
  end;

var
  s: array[0..31] of Char;
  sec: TmcfSection;
  i: Integer;
  obj: THardwareMonitorModul;
begin
  EnterCriticalSection(fActiveModulesCS);
  try
    fActiveModules.Clear;
    fTimer.Interval := aSection.GetInt('RefreshRate', 250);
    s := aSection.GetString('DisplaySerial', '');
    if s <> '' then begin
      for i := 0 to High(fDisplays) do
        if fDisplays[i].dInfo.SerialNumber = s then begin
          ActivateDisplay(i);
          break;
        end;
    end;
    i := 0;
    while aSection.Sections.IndexOf('Modul'+ToStr(i)) >= 0 do begin
      obj := THardwareMonitorModul.Create(self);
      fActiveModules.Add(obj);
      obj.LoadFromFile(aSection.Sections['Modul'+ToStr(i)]);
      inc(i);
    end;
  finally
    LeaveCriticalSection(fActiveModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Resume;
begin
  dec(fSleepCount);
  if fSleepCount <= 0 then begin
    Suspended := false;
    fSleepCount := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Suspend;
begin
  Suspended := true;
  inc(fSleepCount);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Terminate;
begin
  inherited Terminate;
  fCanWorkEvent.SetEvent;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor THardwareMonitor.Create;
begin
  inherited Create(false);

  fBackground  := TBitmap.Create;
  fFrameBuffer := TBitmap.Create;

  SetLength(fModules, 0);
  SetLength(fDisplays, 0);

  fIdleEvent    := TEvent.Create(nil, true, true, '');
  fCanWorkEvent := TEvent.Create(nil, true, false, '');
  fIdleEvent.SetEvent;
  fCanWorkEvent.ResetEvent;
  fSuspended := true;
  InitCriticalSection(fFrameBufferCS);
  InitCriticalSection(fActiveModulesCS);
  fTimer := TTimer.Create(nil);
  fTimer.Interval := 1000;
  fTimer.Enabled := true;
  fTimer.OnTimer := @OnTimer;
  fActiveModules := TObjectList.create(true);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor THardwareMonitor.Destroy;
begin
  fActiveModules.Free;
  fTimer.Free;
  Terminate;
  inherited Destroy;
  FreeDisplays;
  fIdleEvent.Free;
  fCanWorkEvent.Free;
  fBackground.Free;
  fFrameBuffer.Free;
  DoneCriticalsection(fFrameBufferCS);
  DoneCriticalsection(fActiveModulesCS);
end;
{$ELSE}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//THardwareMonitor//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetModulCount: Integer;
begin
  result := fModules.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetModul(const aIndex: Integer): THardwareMonitorModul;
begin
  if (aIndex >= 0) and (aIndex < fModules.Count) then
    result := fModules[aIndex]
  else
    raise Exception.Create(format('THardwareMonitor.GetModul: index out of bounds (%d)', [aIndex]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetUpdateRate: Cardinal;
begin
  result := fTimer.Interval;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetUpdateRate(const aValue: Cardinal);
begin
  fTimer.Enabled := (aValue <> 0);
  fTimer.Interval := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.ThreadUpdateDone(aSender: TObject);
begin
  UpdateFrameBuffer;
  inherited Update;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.TimerTick(aSender: TObject);
begin
  Update(true);
end;

procedure THardwareMonitor.TouchEvent(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode);
var
  p: TPoint;
  i: Integer;
  obj: THardwareMonitorModul;
begin
  inherited TouchEvent(aSender, aPoint, aPressure, aMode);

  if Assigned(fLargeModul) then begin
    if fLargeModul.SendTouchReport(aPoint, aPressure, aMode) then begin
      fLargeModul.IsSmall := true;
      fLargeModul := nil;
    end;
  end else begin
    EnterCriticalSection(fModulesCS);
    try
      for i := fModules.Count-1 downto 0 do begin
        obj := fModules[i];
        with obj  do begin
          if (aPoint.X >= Position.x) and
             (aPoint.Y >= Position.y) and
             (aPoint.X <= Position.x + SmallSize.x) and
             (aPoint.Y <= Position.y + SmallSize.y) then begin
            p := aPoint;
            p.X := aPoint.X - Position.x;
            p.Y := aPoint.Y - Position.y;
            if SendTouchReport(p, aPressure, aMode) then begin
              fLargeModul := obj;
              fLargeModul.IsSmall := false;
              UpdateFrameBuffer;
              inherited Update;
              exit;
            end;
            Continue;
          end;
        end;
      end;
    finally
      LeaveCriticalSection(fModulesCS);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.AddModul(const aLibFile: String): THardwareMonitorModul;
begin
  EnterCriticalSection(fModulesCS);
  try
    result := THardwareMonitorModul.Create(self);
    try
      result.InitLib(aLibFile);
      fModules.Add(result);
      Update(true);
    except
        on E: Exception do begin
        if fModules.Remove(result) < 0 then
          FreeAndNil(result);
        raise e;
      end;
    end;
  finally
    LeaveCriticalSection(fModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function THardwareMonitor.GetModulByName(const aName: String): THardwareMonitorModul;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to fModules.Count-1 do begin
    if (fModules[i].Name = aName) then begin
      result := fModules[i];
      break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.DelModul(const aIndex: Integer);
begin
  EnterCriticalSection(fModulesCS);
  try
    if (aIndex >= 0) and (aIndex < fModules.Count) then begin
      fModules.Delete(aIndex);
      Update(true);
    end else
      raise Exception.Create(format('THardwareMonitor.DelModul: index out of bounds (%d)', [aIndex]));
  finally
    LeaveCriticalSection(fModulesCS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.UpdateFrameBuffer;
var
  i: Integer;
  o: THardwareMonitorModul;
  f: TBlendFunction;
  bmp: TBitmap;
  dRes: TDrawResult;

  procedure DrawModul(const aModul: THardwareMonitorModul);
  var
    p, s: Point;
  begin
    dRes := aModul.Draw;
    if dRes.Transparent then
      f.AlphaFormat := AC_SRC_ALPHA
    else
      f.AlphaFormat := 0;
    if aModul.CurrentIsSmall then
      p := aModul.Position
    else
      p := Classes.Point(0, 0);
    if aModul.CurrentIsSmall then
      s := aModul.CurrentSmallSize
    else
      s := aModul.CurrentLargeSize;
    GdiAlphaBlend(Canvas.Handle, p.x, p.y, s.x, s.y,
      dRes.Handle, 0, 0, dRes.Width, dRes.Height, f);
  end;

begin
  LockFrameBuffer;
  try
    Canvas.StretchDraw(Classes.Rect(0, 0,
      fBackground.Width, fBackground.Height), fBackground);

    f.AlphaFormat         := 0;//AC_SRC_ALPHA;
    f.BlendFlags          := 0;
    f.BlendOp             := AC_SRC_OVER;
    f.SourceConstantAlpha := 255;

//DRAW
    EnterCriticalSection(fModulesCS);
    try
      if Assigned(fLargeModul) then begin
        DrawModul(fLargeModul);
      end else
        for i := 0 to fModules.Count-1 do
          DrawModul(fModules[i]);
    finally
      LeaveCriticalSection(fModulesCS);
    end;
  finally
    UnlockFrameBuffer;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.LockFrameBuffer;
begin
  EnterCriticalSection(fFrameBufferCS);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.UnlockFrameBuffer;
begin
  LeaveCriticalSection(fFrameBufferCS);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SaveToFile(const aSection: TmcfSection);
var
  i: Integer;
begin
  with aSection.Sections['DisplayInfo'] do begin
    SetInt('Width',       fDisplayInfo.Width);
    SetInt('Height',      fDisplayInfo.Height);
    SetInt('PixelFormat', fDisplayInfo.PixelFormat);
    SetInt('Handle',      fDisplayInfo.Handle);
    SetInt('Version',     fDisplayInfo.Version);
    SetString('Name',     fDisplayInfo.Name);
    SetString('Serial',   fDisplayInfo.SerialNumber);
  end;
  aSection.SetInt('UpdateRate', UpdateRate);
  aSection.SetInt('ModulCount', fModules.Count);
  for i := 0 to fModules.Count-1 do
    with aSection.Sections['Modules'] do
      fModules[i].SaveToFile(Sections[IntToStr(i)]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.LoadFromFile(const aSection: TmcfSection; const aModules: TModulesInfo);

  function FindModul(const aLibFile, aVersion: String): Integer;
  var
    i: Integer;
  begin
    for i := 0 to aModules.Count-1 do
      if (ExtractFileName(aModules[i].libName) = aLibFile) and
         (aModules[i].Version = aVersion) then begin
        result := i;
        exit;
      end;
    result := -1;
  end;

var
  i, c, id: Integer;
begin
  with aSection do begin
    if FileExists(GetString('Background', '')) then
      fBackground.LoadFromFile(GetString('Background', ''));
    UpdateRate := GetInt('UpdateRate', 250);
    c := GetInt('ModulCount', 0);
    with Sections['Modules'] do
      for i := 0 to c-1 do begin
        with Sections[IntToStr(i)] do begin
          id := FindModul(GetString('LibName', ''), GetString('LibVersion', ''));
          if (id >= 0) then begin
            AddModul(aModules[id].libName).LoadFromFile(aSection.Sections['Modules'].Sections[IntToStr(i)]);
          end;
        end;
      end;
  end;
  Update(true);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.Update(const aUpdateFrameBuffer: Boolean = false);
begin
  if not aUpdateFrameBuffer then
    inherited Update
  else
    fUpdateThread.DoUpdate;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetBackground(const aGraphic: TGraphic);
begin
  fBackground.Canvas.StretchDraw(Classes.Rect(0, 0, fBackground.Width, fBackground.Height), aGraphic);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.SetSize(AWidth, AHeight: integer);
begin
  inherited SetSize(AWidth, AHeight);
  fBackground.SetSize(AWidth, AHeight);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure THardwareMonitor.EmulateTouch(const aPoint: TPoint);
begin
  TouchEvent(self, aPoint, 1, tmPenDown);
  TouchEvent(self, aPoint, 1, tmPenUp);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor THardwareMonitor.Create(const aDisplayID: Integer);
begin
  inherited Create;

  InitCriticalSection(fFrameBufferCS);
  InitCriticalSection(fModulesCS);

  fBackground := TBitmap.Create;
  fModules := TModules.Create(true);
  Open(aDisplayID);
  fUpdateThread := TUpdateThread.Create(fModules, fModulesCS, @ThreadUpdateDone);
  fTimer := TTimer.Create(nil);
  fTimer.OnTimer := @TimerTick;
  fTimer.Enabled := true;
  fTimer.Interval := 250;
  TouchInterval := 100;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor THardwareMonitor.Destroy;
begin
  FreeAndNil(fTimer);
  FreeAndNil(fUpdateThread);
  Close;
  FreeAndNil(fBackground);
  FreeAndNil(fModules);

  DoneCriticalSection(fFrameBufferCS);
  DoneCriticalSection(fModulesCS);
  inherited Destroy;
end;
{$ENDIF}

end.

