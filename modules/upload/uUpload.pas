unit uUpload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, Windows, ActiveX, ComObj, Variants,
  GDIPOBJ, GDIPAPI, uUSBDisplay, uWmiModul;

type

  { TUpload }

  TUpload = class(TwmiGraphModul)
  private
    function PrepareLine(aFormat: String): String;
  protected
    function GetData: Single; override;
    function GetLargeDisplayString: String; override;
    function GetSmallDisplayString: String; override;
  public
    constructor Create;
  end;

  TUploadData = packed record
    Currently: Cardinal;
    StartUp: Cardinal;
    DeltaTime: Cardinal;
  end;
  PUploadData = ^TUploadData;

  TUploadWmiThread = class(TwmiThread)
    TimeStamp: Cardinal;
    procedure Update; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TUploadWmiThread.Update;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
  StartUpOld: Cardinal;
  c: Cardinal;
begin
  StartUpOld := PUploadData(Data)^.StartUp;
  FillChar(PUploadData(Data)^, SizeOf(PUploadData(Data)^), 0);
  FWbemObjectSet := WMIService.ExecQuery('SELECT BytesSentPersec FROM Win32_PerfRawData_Tcpip_NetworkInterface', 'WQL', wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, nil) = 0 do begin
    c := FWbemObject.Properties_.Item('BytesSentPersec').Value;
    PUploadData(Data)^.StartUp := PUploadData(Data)^.StartUp + c;
  end;
  PUploadData(Data)^.Currently := PUploadData(Data)^.StartUp - StartUpOld;
  PUploadData(Data)^.DeltaTime := GetTickCount - TimeStamp;
  TimeStamp := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TUploadWmiThread.Create;
begin
  inherited Create;
  New(PUploadData(Data));
  TimeStamp := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TUploadWmiThread.Destroy;
begin
  Dispose(PUploadData(Data));
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUpload.PrepareLine(aFormat: String): String;
begin
  result := '';
  if not Assigned(fUpdateThread) then
    exit;
  with PUploadData((fUpdateThread as TUploadWmiThread).Data)^ do begin
    result := StringReplace(aFormat, '%nl', sLineBreak, [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%d1', FormatByte(StartUp), [rfIgnoreCase, rfReplaceAll]);
    if DeltaTime > 0 then
      result := StringReplace(result, '%d2', FormatByte(Currently / DeltaTime * 1000), [rfIgnoreCase, rfReplaceAll])
    else
      result := StringReplace(result, '%d2', '', [rfIgnoreCase, rfReplaceAll]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUpload.GetData: Single;
begin
  if not Assigned(fUpdateThread) then
    exit;
  with PUploadData((fUpdateThread as TUploadWmiThread).Data)^ do begin
    if DeltaTime > 0 then
      Result := Currently / DeltaTime
    else
      Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUpload.GetLargeDisplayString: String;
begin
  result := PrepareLine(fFirstLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TUpload.GetSmallDisplayString: String;
begin
  result := PrepareLine(fSecondLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TUpload.Create;
begin
  fFirstLineFormat := 'Upload';
  fSecondLineFormat := '%d1%nl%d2';
  inherited Create(TUploadWmiThread);
end;

end.

