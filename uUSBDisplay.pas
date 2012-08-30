unit uUSBDisplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uUSBD480_API, IntfGraphics, LCLType, uUtils;

type
  EUSBDisplay = class(Exception);
  TTouchMode = (tmPenDown, tmPenMove, tmPenUp);
  TTouchEvent = procedure(aSender: TObject; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode) of object;
  TUSBDisplay = class(TBitmap)
  private
    fDisplayID: Integer;
    fDisplayInfo: TDisplayInfo;
    fBuffer: TRGB565Arr;
    fTouchInterval: Word;
    fTouchThread: TThread;

    fOnTouch: TTouchEvent;

    procedure SetTouchInterval(const aValue: Word);
    procedure SetOnTouch(const aValue: TTouchEvent);
    function GetBufferData: Pointer;
    function GetBufferSize: Integer;
    procedure CreateTouchThread;
  public
    property DisplayID: Integer read fDisplayID;
    property DisplayInfo: TDisplayInfo read fDisplayInfo;
    property TouchInterval: Word read fTouchInterval write SetTouchInterval;
    property BufferData: Pointer read GetBufferData;
    property BufferSize: Integer read GetBufferSize;

    property OnTouch: TTouchEvent read fOnTouch write SetOnTouch;

    procedure Open(const aDisplayID: Cardinal);
    procedure Close;
    procedure Update;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  Windows;

type
  TTouchThread = class(TThread)
  private
    fOwner: TUSBDisplay;
    fDisplayInfo: TDisplayInfo;
    fLastTouchData: TTouchReport;
    fOnTouch: TTouchEvent;
    fInterval: Word; //in ms

    fTouchPoint: TPoint;
    fTouchMode: TTouchMode;
    fTouchPressure: Byte;
  protected
    procedure DoTouch;
    procedure Execute; override;
  public
    constructor Create(const aOwner: TUSBDisplay; const aDisplayInfo: TDisplayInfo;
      const aTouchEvent: TTouchEvent; const aInterval: Word);
  end;

const
  TOUCH_MODE = 3;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TTouchThread.DoTouch;
begin
  if Assigned(fOnTouch) then
    fOnTouch(fOwner, fTouchPoint, fTouchPressure, fTouchMode);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TTouchThread.Execute;
var
  touch: TTouchReport;
  time: Cardinal;
  d: Integer;
begin
  if not Assigned(fOnTouch) then
    exit;

  USBD480_SetTouchMode(@fDisplayInfo, TOUCH_MODE);
  while not Terminated do begin
    time := GetTickCount;
    if (USBD480_GetTouchReport(@fDisplayInfo, @touch) = 0) then
      exit;

    //ACHTUNG PenDown ist invertiert!!!
    if fLastTouchData.PenDown and not touch.PenDown then begin
      fTouchMode     := tmPenDown;
      fTouchPressure := touch.Pressure;
      fTouchPoint    := Classes.Point(touch.x, touch.y);
      Synchronize(@DoTouch);
    end else if not fLastTouchData.PenDown and not touch.PenDown then begin
      fTouchMode     := tmPenMove;
      fTouchPressure := touch.Pressure;
      fTouchPoint    := Classes.Point(touch.x, touch.y);
      Synchronize(@DoTouch);
    end else if not fLastTouchData.PenDown and touch.PenDown then begin
      fTouchMode     := tmPenUp;
      fTouchPressure := touch.Pressure;
      fTouchPoint    := Classes.Point(fLastTouchData.x, fLastTouchData.y);
      Synchronize(@DoTouch);
    end;
    fLastTouchData := touch;

    d := fInterval - (GetTickCount - time);
    if d > 0 then
      sleep(d);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TTouchThread.Create(const aOwner: TUSBDisplay; const aDisplayInfo: TDisplayInfo;
  const aTouchEvent: TTouchEvent; const aInterval: Word);
begin
  inherited Create(false);
  FreeOnTerminate := True;
  fOwner       := aOwner;
  fDisplayInfo := aDisplayInfo;
  fOnTouch     := aTouchEvent;
  fInterval    := aInterval;
  FillChar(fLastTouchData, SizeOf(fLastTouchData), 0);
  fLastTouchData.PenDown := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.SetTouchInterval(const aValue: Word);
begin
  if fDisplayID < 0 then
    exit;
  if (fTouchInterval = aValue) then
    exit;
  if fDisplayID < 0 then
    raise EUSBDisplay.Create('no display attached');
  fTouchInterval := aValue;
  if fTouchInterval > 0 then begin
    USBD480_SetTouchMode(@fDisplayInfo, TOUCH_MODE);
    CreateTouchThread;
  end else begin
    if Assigned(fTouchThread) then
      fTouchThread.Terminate;
    USBD480_SetTouchMode(@fDisplayInfo, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.SetOnTouch(const aValue: TTouchEvent);
begin
  if fDisplayID < 0 then
    exit;
  if Assigned(fTouchThread) then
    fTouchThread.Terminate;
  fOnTouch := aValue;
  CreateTouchThread;
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
procedure TUSBDisplay.CreateTouchThread;
begin
  if fDisplayID < 0 then
    exit;
  if Assigned(fTouchThread) then
    fTouchThread.Terminate;
  if (fTouchInterval > 0) and Assigned(fOnTouch) then
    fTouchThread := TTouchThread.Create(self, fDisplayInfo, fOnTouch, fTouchInterval);
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
  if Assigned(fTouchThread) then
    fTouchThread.Terminate;
  fTouchInterval := 0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUSBDisplay.Update;
begin
  if fDisplayID < 0 then
    exit;
  BitmapTo565(self, fBuffer);
  USBD480_DrawFullScreen(@fDisplayInfo, @fBuffer[0]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TUSBDisplay.Create;
begin
  inherited Create;
  PixelFormat := pf16Bit;
  fDisplayID := -1;
  SetLength(fBuffer, 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TUSBDisplay.Destroy;
begin
  Close;
  inherited Destroy;
end;

end.

