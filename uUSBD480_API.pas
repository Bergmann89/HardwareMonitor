unit uUSBD480_API;

{$mode objfpc}{$H+}
{$DEFINE WORK_THREAD_SAVE}

interface

uses
  Classes, SysUtils, windows;

type
  TPixelFormat = (
    pf1BPP = 0,
    pf1BPP_DS = 1,
    pf2BPP = 2,
    pf4BPP = 3,
    pf8BPP = 4,
    pf16BBP_RGB565 = 5,
    pf16BPP_BGR565 = 6,
    pf32BPP = 7);

  TDisplayInfo = packed record
    Width: Cardinal;
    Height: Cardinal;
    PixelFormat: Cardinal;
    Name: array[0..31] of Char;
    SerialNumber: array[0..31] of Char;
    Handle: Cardinal;
    Version: Cardinal;
  end;
  PDisplayInfo = ^TDisplayInfo;

  TTouchReport = packed record
    x: Word;
    y: Word;
    z1: Word;
    z2: Word;
    PenDown: Boolean;
    Pressure: Byte;
    Reserved: array[0..5] of Byte;
  end;
  PTouchReport = ^TTouchReport;

  EUSBD480 = class(Exception);

  TUSBD480_GetNumberOfDisplays = function(): Integer; stdcall;
  TUSBD480_GetDisplayConfiguration = function(aIndex: Cardinal; aDisplayInfo: PDisplayInfo): Integer; stdcall;
  TUSBD480_Open = function(aDisplayInfo: PDisplayInfo; aFlags: Cardinal): Integer; stdcall;
  TUSBD480_Close = function(aDisplayInfo: PDisplayInfo): Integer; stdcall;
  TUSBD480_DrawFullScreen = function(aDisplayInfo: PDisplayInfo; aBuffer: PByte): Integer; stdcall;
  TUSBD480_DrawFullScreenRGBA32 = function(aDisplayInfo: PDisplayInfo; aBuffer: PCardinal): Integer; stdcall;
  TUSBD480_DrawFullScreenBGRA32 = function(aDisplayInfo: PDisplayInfo; aBuffer: PCardinal): Integer; stdcall;
  TUSBD480_SetBrightness = function(aDisplayInfo: PDisplayInfo; aBrightness: Cardinal): Integer; stdcall;
  TUSBD480_SetTouchMode = function(aDisplayInfo: PDisplayInfo; aMode: Cardinal): Integer; stdcall;
  TUSBD480_SetAddress = function(aDisplayInfo: PDisplayInfo; aAddress: Cardinal): Integer; stdcall;
  TUSBD480_SetFrameStartAddress = function(aDisplayInfo: PDisplayInfo; aAddress: Cardinal): Integer; stdcall;
  TUSBD480_DrawFromBuffer = function(aDisplayInfo: PDisplayInfo; aBuffer: PByte; aSize: Cardinal): Integer; stdcall;
  TUSBD480_GetTouchReport = function(aDisplayInfo: PDisplayInfo; aTouchReport: PTouchReport): Integer; stdcall;
  TUSBD480_GetTouchPosition = function(aDisplayInfo: PDisplayInfo; x: PCardinal; y: PCardinal): Integer; stdcall;

  TUSBD480_SetStartupImage = function(aDisplayInfo: PDisplayInfo; aBuffer: PByte; aSize: Cardinal): Integer; stdcall;
  TUSBD480_UpdateFPGA = function(aDisplayInfo: PDisplayInfo): Integer; stdcall;
  TUSBD480_UpdateFW = function(aDisplayInfo: PDisplayInfo): Integer; stdcall;

{$IFDEF WORK_THREAD_SAVE}
 function USBD480_GetNumberOfDisplays(): Integer;
 function USBD480_GetDisplayConfiguration(const aIndex: Cardinal; const  aDisplayInfo: PDisplayInfo): Integer;
 function USBD480_Open(const aDisplayInfo: PDisplayInfo; const aFlags: Cardinal): Integer;
 function USBD480_Close(const aDisplayInfo: PDisplayInfo): Integer;
 function USBD480_DrawFullScreen(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte): Integer;
 function USBD480_DrawFullScreenRGBA32(const aDisplayInfo: PDisplayInfo; const aBuffer: PCardinal): Integer;
 function USBD480_DrawFullScreenBGRA32(const aDisplayInfo: PDisplayInfo; const aBuffer: PCardinal): Integer;
 function USBD480_SetBrightness(const aDisplayInfo: PDisplayInfo; const aBrightness: Cardinal): Integer;
 function USBD480_SetTouchMode(const aDisplayInfo: PDisplayInfo; const aMode: Cardinal): Integer;
 function USBD480_SetAddress(const aDisplayInfo: PDisplayInfo; const aAddress: Cardinal): Integer;
 function USBD480_SetFrameStartAddress(const aDisplayInfo: PDisplayInfo; const aAddress: Cardinal): Integer;
 function USBD480_DrawFromBuffer(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte; const aSize: Cardinal): Integer;
 function USBD480_GetTouchReport(const aDisplayInfo: PDisplayInfo; const aTouchReport: PTouchReport): Integer;
 function USBD480_GetTouchPosition(const aDisplayInfo: PDisplayInfo; const x, y: PCardinal): Integer;

 function USBD480_SetStartupImage(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte; const aSize: Cardinal): Integer;
 function USBD480_UpdateFPGA(const aDisplayInfo: PDisplayInfo): Integer;
 function USBD480_UpdateFW(const aDisplayInfo: PDisplayInfo): Integer;
{$ELSE}
var
  USBD480_GetNumberOfDisplays: TUSBD480_GetNumberOfDisplays;
  USBD480_GetDisplayConfiguration: TUSBD480_GetDisplayConfiguration;
  USBD480_Open: TUSBD480_Open;
  USBD480_Close: TUSBD480_Close;
  USBD480_DrawFullScreen: TUSBD480_DrawFullScreen;
  USBD480_DrawFullScreenRGBA32: TUSBD480_DrawFullScreenRGBA32;
  USBD480_DrawFullScreenBGRA32: TUSBD480_DrawFullScreenBGRA32;
  USBD480_SetBrightness: TUSBD480_SetBrightness;
  USBD480_SetTouchMode: TUSBD480_SetTouchMode;
  USBD480_SetAddress: TUSBD480_SetAddress;
  USBD480_SetFrameStartAddress: TUSBD480_SetFrameStartAddress;
  USBD480_DrawFromBuffer: TUSBD480_DrawFromBuffer;
  USBD480_GetTouchReport: TUSBD480_GetTouchReport;
  USBD480_GetTouchPosition: TUSBD480_GetTouchPosition;

  USBD480_SetStartupImage: TUSBD480_SetStartupImage;
  USBD480_UpdateFPGA: TUSBD480_UpdateFPGA;
  USBD480_UpdateFW: TUSBD480_UpdateFW;
{$ENDIF}

procedure USBD480_Init(aLibName: String = ''; const aReInit: Boolean = false);
procedure USBD480_Finish;

implementation

var
  LibHandle: Cardinal;
  Init: Boolean;
  CritSec: TRTLCriticalSection;

{$IFDEF WORK_THREAD_SAVE}
  _USBD480_GetNumberOfDisplays: TUSBD480_GetNumberOfDisplays;
  _USBD480_GetDisplayConfiguration: TUSBD480_GetDisplayConfiguration;
  _USBD480_Open: TUSBD480_Open;
  _USBD480_Close: TUSBD480_Close;
  _USBD480_DrawFullScreen: TUSBD480_DrawFullScreen;
  _USBD480_DrawFullScreenRGBA32: TUSBD480_DrawFullScreenRGBA32;
  _USBD480_DrawFullScreenBGRA32: TUSBD480_DrawFullScreenBGRA32;
  _USBD480_SetBrightness: TUSBD480_SetBrightness;
  _USBD480_SetTouchMode: TUSBD480_SetTouchMode;
  _USBD480_SetAddress: TUSBD480_SetAddress;
  _USBD480_SetFrameStartAddress: TUSBD480_SetFrameStartAddress;
  _USBD480_DrawFromBuffer: TUSBD480_DrawFromBuffer;
  _USBD480_GetTouchReport: TUSBD480_GetTouchReport;
  _USBD480_GetTouchPosition: TUSBD480_GetTouchPosition;

  _USBD480_SetStartupImage: TUSBD480_SetStartupImage;
  _USBD480_UpdateFPGA: TUSBD480_UpdateFPGA;
  _USBD480_UpdateFW: TUSBD480_UpdateFW;
{$ENDIF}

{$IFDEF WORK_THREAD_SAVE}
function USBD480_GetNumberOfDisplays(): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_GetNumberOfDisplays();
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_GetDisplayConfiguration(const aIndex: Cardinal; const  aDisplayInfo: PDisplayInfo): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_GetDisplayConfiguration(aIndex, aDisplayInfo);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_Open(const aDisplayInfo: PDisplayInfo; const aFlags: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_Open(aDisplayInfo, aFlags);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_Close(const aDisplayInfo: PDisplayInfo): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_Close(aDisplayInfo);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_DrawFullScreen(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_DrawFullScreen(aDisplayInfo, aBuffer);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_DrawFullScreenRGBA32(const aDisplayInfo: PDisplayInfo; const aBuffer: PCardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_DrawFullScreenRGBA32(aDisplayInfo, aBuffer);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_DrawFullScreenBGRA32(const aDisplayInfo: PDisplayInfo; const aBuffer: PCardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_DrawFullScreenBGRA32(aDisplayInfo, aBuffer);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_SetBrightness(const aDisplayInfo: PDisplayInfo; const aBrightness: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_SetBrightness(aDisplayInfo, aBrightness);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_SetTouchMode(const aDisplayInfo: PDisplayInfo; const aMode: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_SetTouchMode(aDisplayInfo, aMode);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_SetAddress(const aDisplayInfo: PDisplayInfo; const aAddress: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_SetAddress(aDisplayInfo, aAddress);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_SetFrameStartAddress(const aDisplayInfo: PDisplayInfo; const aAddress: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_SetFrameStartAddress(aDisplayInfo, aAddress);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_DrawFromBuffer(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte; const aSize: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_DrawFromBuffer(aDisplayInfo, aBuffer, aSize);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_GetTouchReport(const aDisplayInfo: PDisplayInfo; const aTouchReport: PTouchReport): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_GetTouchReport(aDisplayInfo, aTouchReport);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_GetTouchPosition(const aDisplayInfo: PDisplayInfo; const x, y: PCardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_GetTouchPosition(aDisplayInfo, x, y);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_SetStartupImage(const aDisplayInfo: PDisplayInfo; const aBuffer: PByte; const aSize: Cardinal): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_SetStartupImage(aDisplayInfo, aBuffer, aSize);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_UpdateFPGA(const aDisplayInfo: PDisplayInfo): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_UpdateFPGA(aDisplayInfo);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;

function USBD480_UpdateFW(const aDisplayInfo: PDisplayInfo): Integer;
begin
  EnterCriticalSection(CritSec);
  try
    result := _USBD480_UpdateFW(aDisplayInfo);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;
{$ENDIF}

procedure USBD480_Init(aLibName: String = ''; const aReInit: Boolean = false);
var
  error: Cardinal;

  function LoadProc(const aName: String): Pointer; overload;
  begin
    result := GetProcAddress(LibHandle, PChar(aName));
    if result = nil then begin
      error := GetLastError;
      raise EUSBD480.Create(format('can''t get proc adress of ''%s''. error code %d: %s', [aName, error, SysErrorMessage(error)]));
    end;
  end;

  function LoadProc(const aOrdinal: Cardinal): Pointer; overload;
  begin
    result := GetProcAddress(LibHandle, {%H-}PChar(aOrdinal));
    if result = nil then begin
      error := GetLastError;
      raise EUSBD480.Create(format('can''t get proc adress of ordinal value (%d). error code %d: %s', [aOrdinal, error, SysErrorMessage(error)]));
    end;
  end;

begin
  if Init and not aReInit then
    exit;

  if aLibName = '' then
    aLibName := 'USBD480_lib.dll';
  LibHandle := LoadLibrary(PChar(aLibName));
  if LibHandle <> 0 then begin
    {$IFDEF WORK_THREAD_SAVE}
    _USBD480_Close := TUSBD480_Close(LoadProc('_USBD480_Close@4'));
    _USBD480_DrawFromBuffer := TUSBD480_DrawFromBuffer(LoadProc('_USBD480_DrawFromBuffer@12'));
    _USBD480_DrawFullScreen := TUSBD480_DrawFullScreen(LoadProc('_USBD480_DrawFullScreen@8'));
    _USBD480_DrawFullScreenBGRA32 := TUSBD480_DrawFullScreenBGRA32(LoadProc('_USBD480_DrawFullScreenBGRA32@8'));
    _USBD480_DrawFullScreenRGBA32 := TUSBD480_DrawFullScreenRGBA32(LoadProc('_USBD480_DrawFullScreenRGBA32@8'));
    _USBD480_GetDisplayConfiguration := TUSBD480_GetDisplayConfiguration(LoadProc('_USBD480_GetDisplayConfiguration@8'));
    _USBD480_GetNumberOfDisplays := TUSBD480_GetNumberOfDisplays(LoadProc('_USBD480_GetNumberOfDisplays@0'));
    _USBD480_GetTouchPosition := TUSBD480_GetTouchPosition(LoadProc('_USBD480_GetTouchPosition@12'));
    _USBD480_GetTouchReport := TUSBD480_GetTouchReport(LoadProc('_USBD480_GetTouchReport@8'));
    _USBD480_Open := TUSBD480_Open(LoadProc('_USBD480_Open@8'));
    _USBD480_SetAddress := TUSBD480_SetAddress(LoadProc('_USBD480_SetAddress@8'));
    _USBD480_SetBrightness := TUSBD480_SetBrightness(LoadProc('_USBD480_SetBrightness@8'));
    _USBD480_SetFrameStartAddress := TUSBD480_SetFrameStartAddress(LoadProc('_USBD480_SetFrameStartAddress@8'));
    _USBD480_SetTouchMode := TUSBD480_SetTouchMode(LoadProc('_USBD480_SetTouchMode@8'));

    _USBD480_SetStartupImage := TUSBD480_SetStartupImage(LoadProc(1));
    _USBD480_UpdateFPGA := TUSBD480_UpdateFPGA(LoadProc(2));
    _USBD480_UpdateFW := TUSBD480_UpdateFW  (LoadProc(3));
    {$ELSE}
    USBD480_Close := TUSBD480_Close(LoadProc('_USBD480_Close@4'));
    USBD480_DrawFromBuffer := TUSBD480_DrawFromBuffer(LoadProc('_USBD480_DrawFromBuffer@12'));
    USBD480_DrawFullScreen := TUSBD480_DrawFullScreen(LoadProc('_USBD480_DrawFullScreen@8'));
    USBD480_DrawFullScreenBGRA32 := TUSBD480_DrawFullScreenBGRA32(LoadProc('_USBD480_DrawFullScreenBGRA32@8'));
    USBD480_DrawFullScreenRGBA32 := TUSBD480_DrawFullScreenRGBA32(LoadProc('_USBD480_DrawFullScreenRGBA32@8'));
    USBD480_GetDisplayConfiguration := TUSBD480_GetDisplayConfiguration(LoadProc('_USBD480_GetDisplayConfiguration@8'));
    USBD480_GetNumberOfDisplays := TUSBD480_GetNumberOfDisplays(LoadProc('_USBD480_GetNumberOfDisplays@0'));
    USBD480_GetTouchPosition := TUSBD480_GetTouchPosition(LoadProc('_USBD480_GetTouchPosition@12'));
    USBD480_GetTouchReport := TUSBD480_GetTouchReport(LoadProc('_USBD480_GetTouchReport@8'));
    USBD480_Open := TUSBD480_Open(LoadProc('_USBD480_Open@8'));
    USBD480_SetAddress := TUSBD480_SetAddress(LoadProc('_USBD480_SetAddress@8'));
    USBD480_SetBrightness := TUSBD480_SetBrightness(LoadProc('_USBD480_SetBrightness@8'));
    USBD480_SetFrameStartAddress := TUSBD480_SetFrameStartAddress(LoadProc('_USBD480_SetFrameStartAddress@8'));
    USBD480_SetTouchMode := TUSBD480_SetTouchMode(LoadProc('_USBD480_SetTouchMode@8'));

    USBD480_SetStartupImage := TUSBD480_SetStartupImage(LoadProc(1));
    USBD480_UpdateFPGA := TUSBD480_UpdateFPGA(LoadProc(2));
    USBD480_UpdateFW := TUSBD480_UpdateFW  (LoadProc(3));
    {$ENDIF}
    Init := True;
  end else begin
    error := GetLastError;
    raise EUSBD480.Create(format('invalid lib handle. error code %d: %s', [error, SysErrorMessage(error)]));
  end;
end;

procedure USBD480_Finish;
begin
  Init := False;
  if LibHandle <> 0 then
    if not FreeLibrary(LibHandle) then
      raise EUSBD480.Create('can''t free lib handle');
end;

{$IFDEF WORK_THREAD_SAVE}
initialization
  InitCriticalSection(CritSec);
{$ENDIF}

finalization
  {$IFDEF WORK_THREAD_SAVE}
  DoneCriticalSection(CritSec);
  {$ENDIF}
  USBD480_Finish;

end.

