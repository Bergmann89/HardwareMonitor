unit uDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, Windows, ActiveX, ComObj, Variants,
  GDIPOBJ, GDIPAPI, uUSBDisplay, uWmiModul;

type

  { TDownload }

  TDownload = class(TwmiGraphModul)
  private
    function PrepareLine(aFormat: String): String;
  protected
    function GetData: Single; override;
    function GetLargeDisplayString: String; override;
    function GetSmallDisplayString: String; override;
  public
    constructor Create;
  end;

  TDownloadData = packed record
    Currently: Cardinal;
    StartUp: Cardinal;
    DeltaTime: Cardinal;
  end;
  PDownloadData = ^TDownloadData;

  { TDownloadWmiThread }

  TDownloadWmiThread = class(TwmiThread)
    TimeStamp: Cardinal;
    procedure Update; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDownloadWmiThread.Update;
var
  FWbemObjectSet: OLEVariant;
  FWbemObject   : Variant;
  oEnum         : IEnumvariant;
  StartUpOld: Cardinal;
  c: Cardinal;
begin
  StartUpOld := PDownloadData(Data)^.StartUp;
  FillChar(PDownloadData(Data)^, SizeOf(PDownloadData(Data)^), 0);
  FWbemObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_PerfRawData_Tcpip_NetworkInterface', 'WQL', wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, nil) = 0 do begin
    c := FWbemObject.Properties_.Item('BytesReceivedPersec').Value;
    PDownloadData(Data)^.StartUp := PDownloadData(Data)^.StartUp + c;
  end;
  PDownloadData(Data)^.Currently := PDownloadData(Data)^.StartUp - StartUpOld;
  PDownloadData(Data)^.DeltaTime := GetTickCount - TimeStamp;
  TimeStamp := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TDownloadWmiThread.Create;
begin
  inherited Create;
  New(PDownloadData(Data));
  TimeStamp := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TDownloadWmiThread.Destroy;
begin
  Dispose(PDownloadData(Data));
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.PrepareLine(aFormat: String): String;
begin
  result := '';
  if not Assigned(fUpdateThread) or not Assigned((fUpdateThread as TDownloadWmiThread).Data) then
    exit;
  with PDownloadData((fUpdateThread as TDownloadWmiThread).Data)^ do begin
    result := StringReplace(aFormat, '%nl', sLineBreak, [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%d1', FormatByte(StartUp), [rfIgnoreCase, rfReplaceAll]);
    if DeltaTime > 0 then
      result := StringReplace(result, '%d2', FormatByte(Currently / DeltaTime * 1000), [rfIgnoreCase, rfReplaceAll])
    else
      result := StringReplace(result, '%d2', '', [rfIgnoreCase, rfReplaceAll])
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.GetData: Single;
begin
  if not Assigned(fUpdateThread) then
    exit;
  with PDownloadData((fUpdateThread as TDownloadWmiThread).Data)^ do begin
    if DeltaTime > 0 then
      Result := Currently / DeltaTime
    else
      Result := 0;  
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.GetLargeDisplayString: String;
begin
  Result := PrepareLine(fFirstLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TDownload.GetSmallDisplayString: String;
begin
  result := PrepareLine(fSecondLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TDownload.Create;
begin
  fFirstLineFormat := 'Download';
  fSecondLineFormat := '%d1%nl%d2';
  inherited Create(TDownloadWmiThread);
end;

end.

