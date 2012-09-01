unit uUSBDisplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uUSBD480_API, IntfGraphics, LCLType, uUtils, ExtCtrls;

type
  EUSBDisplay = class(Exception);
  TTouchMode = (tmPenDown, tmPenMove, tmPenUp);
  TTouchEvent = procedure(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode) of object;

  { TUSBDisplay }

  TUSBDisplay = class(TBitmap)
  private
    fDisplayID: Integer;
    fDisplayInfo: TDisplayInfo;
    fBuffer: TRGB565Arr;
    fTouchInterval: Word;
    fTouchTimer: TTimer;
    fLastTouchData: TTouchReport;

    fOnTouch: TTouchEvent;
    fOnUpdate: TNotifyEvent;

    procedure SetTouchInterval(const aValue: Cardinal);
    procedure SetOnTouch(const aValue: TTouchEvent);
    function GetTouchInterval: Cardinal;
    function GetBufferData: Pointer;
    function GetBufferSize: Integer;
    procedure TouchTimerTick(aSender: TObject);
  protected
    procedure TouchEvent(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode); virtual;
  public
    property DisplayID: Integer read fDisplayID;
    property DisplayInfo: TDisplayInfo read fDisplayInfo;
    property TouchInterval: Cardinal read GetTouchInterval write SetTouchInterval;
    property BufferData: Pointer read GetBufferData;
    property BufferSize: Integer read GetBufferSize;

    property OnTouch : TTouchEvent  read fOnTouch  write SetOnTouch;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;

    procedure Open(const aDisplayID: Cardinal);
    procedure Close;
    procedure Update;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  Windows;

const
  TOUCH_MODE = 3;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.SetTouchInterval(const aValue: Cardinal);
begin
  fTouchTimer.Enabled := (aValue <> 0);
  fTouchTimer.Interval := aValue;
  if fDisplayID >= 0 then begin
    if fTouchTimer.Enabled then
      USBD480_SetTouchMode(@fDisplayInfo, TOUCH_MODE)
    else
      USBD480_SetTouchMode(@fDisplayInfo, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.SetOnTouch(const aValue: TTouchEvent);
begin
  fOnTouch := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUSBDisplay.GetTouchInterval: Cardinal;
begin
  result := fTouchTimer.Interval;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUSBDisplay.GetBufferData: Pointer;
begin
  result := @fBuffer[0];
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUSBDisplay.GetBufferSize: Integer;
begin
  result := 2*Length(fBuffer);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.TouchTimerTick(aSender: TObject);
var
  touch: TTouchReport;
  time: Cardinal;
  d: Integer;
begin
  time := GetTickCount;
  if (USBD480_GetTouchReport(@fDisplayInfo, @touch) = 0) then
    exit;

  //ACHTUNG PenDown ist invertiert!!!
  if fLastTouchData.PenDown and not touch.PenDown then begin
    TouchEvent(self, Classes.Point(touch.x, touch.y), touch.Pressure, tmPenDown);
  end else if not fLastTouchData.PenDown and not touch.PenDown then begin
    TouchEvent(self, Classes.Point(touch.x, touch.y), touch.Pressure, tmPenMove);
  end else if not fLastTouchData.PenDown and touch.PenDown then begin
    TouchEvent(self, Classes.Point(fLastTouchData.x, fLastTouchData.y), touch.Pressure, tmPenUp);
  end;
  fLastTouchData := touch;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.TouchEvent(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode);
begin
  if Assigned(fOnTouch) then
    fOnTouch(self, aPoint, aPressure, aMode);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.Open(const aDisplayID: Cardinal);
var
  c: Integer;
begin
  if aDisplayID < High(Cardinal) then begin
    USBD480_Init;
    c := USBD480_GetNumberOfDisplays();
    if (c < aDisplayID) then
      raise EUSBDisplay.Create('invalid display id');
    if USBD480_GetDisplayConfiguration(aDisplayID, @fDisplayInfo) = 0 then
      raise EUSBDisplay.Create('unable to receive display info');
    if USBD480_Open(@fDisplayInfo, 0) = 0 then
      raise EUSBDisplay.Create('unable to open display');
  end else begin
    fDisplayInfo.Name := 'Dummy Display';
    fDisplayInfo.Width := 480;
    fDisplayInfo.Height := 272;
    fDisplayInfo.SerialNumber := '0123456789D';
  end;
  fDisplayID := aDisplayID;
  SetSize(fDisplayInfo.Width, fDisplayInfo.Height);
  SetLength(fBuffer, Width * Height);
  FillChar(fBuffer[0], 2*Length(fBuffer), #0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.Close;
begin
  if (fDisplayID >= 0) then begin
    USBD480_Close(@fDisplayInfo);
    FillChar(fDisplayInfo, SizeOf(fDisplayInfo), #0);
    fDisplayID := -1;
  end;
  SetLength(fBuffer, 0);
  TouchInterval := 0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.Update;
begin
  if fDisplayID < 0 then
    exit;
  BitmapTo565(self, fBuffer);
  USBD480_DrawFullScreen(@fDisplayInfo, @fBuffer[0]);
  if Assigned(fOnUpdate) then
    fOnUpdate(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TUSBDisplay.Create;
begin
  inherited Create;
  PixelFormat := pf16Bit;
  fDisplayID := -1;
  fTouchTimer := TTimer.Create(nil);
  fTouchTimer.Enabled := false;
  fTouchTimer.OnTimer := @TouchTimerTick;
  SetLength(fBuffer, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TUSBDisplay.Destroy;
begin
  Close;
  FreeAndNil(fTouchTimer);
  inherited Destroy;
end;

end.

