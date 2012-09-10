unit uSensor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWmiModul, uUtils, variants, comobj, ActiveX, uModulAPI;

type

  { TSensor }

  TSensor = class(TwmiGraphModul)
  private
    fMaxValue: Single;
    fMinValue: Single;
    fWarningColor: Cardinal;
    fSensorIdent: String;
    function PrepareLine(aFormat: String): String;
  protected
    function GetData: Single; override;
    function GetLargeDisplayString: String; override;
    function GetSmallDisplayString: String; override;
    function GetFontColor: Cardinal; override;
  public
    procedure SetSettings(const aData: PSettingsItemRec); override;

    constructor Create;
  end;

  TSensorType = (stUnknown = 0, stLoad, stTemp, stClock, stVolt, stFan);
  TSensorData = packed record
    UpdateAll: Boolean;
    Ident: String;
    ParentName: String;
    SensorName: String;
    SensorType: TSensorType;
    Value: Single;
    Min: Single;
    Max: Single;
  end;
  PSensorData = ^TSensorData;

  { TSensorWmiThread }

  TSensorWmiThread = class(TwmiThread)
    procedure Update; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorWmiThread.Update;
var
  WbemObjectSet: OLEVariant;
  WbemObject   : Variant;
  Enum         : IEnumvariant;
  ParentIdent: String;
  str: String;
  s: Variant;
begin
  with PSensorData(Data)^ do begin
    str := 'Value, Max, Min';
    if UpdateAll then
      str := '*';
    s := 'SELECT '+str+' FROM Sensor WHERE Identifier = "'+Ident+'"';
    WbemObjectSet := WMIService.ExecQuery(s, 'WQL', wbemFlagForwardOnly);
    Enum          := IUnknown(WbemObjectSet._NewEnum) as IEnumVariant;
    if Enum.Next(1, WbemObject, nil) = 0 then begin
      Value := VarToFloat(WbemObject.Properties_.Item('Value').Value);
      Min   := VarToFloat(WbemObject.Properties_.Item('Min').Value);
      Max   := VarToFloat(WbemObject.Properties_.Item('Max').Value);
      if UpdateAll then begin
        ParentIdent := VarToStr(WbemObject.Properties_.Item('Parent').Value);
        SensorName  := VarToStr(WbemObject.Properties_.Item('Name').Value);
        str := LowerCase(VarToStr(WbemObject.Properties_.Item('SensorType').Value));
        if str = 'load' then
          SensorType := stLoad
        else if str = 'temperature' then
          SensorType := stTemp
        else if str = 'clock' then
          SensorType := stClock
        else if str = 'voltage' then
          SensorType := stVolt
        else if str = 'fan' then
          SensorType := stFan;

        ParentName := '';
        s := 'SELECT Name FROM Hardware WHERE Identifier = "'+ParentIdent+'"';
        WbemObjectSet := WMIService.ExecQuery(s, 'WQL', wbemFlagForwardOnly);
        Enum          := IUnknown(WbemObjectSet._NewEnum) as IEnumVariant;
        if Enum.Next(1, WbemObject, nil) = 0 then
          ParentName := VarToStr(WbemObject.Properties_.Item('Name').Value);
      end;
    end;
    if UpdateAll then
      UpdateAll := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TSensorWmiThread.Create;
begin
  inherited Create;
  Namespace := 'root\OpenHardwareMonitor';
  New(PSensorData(Data));
  FillChar(PSensorData(Data)^, SizeOf(PSensorData(Data)^), 0);
  PSensorData(Data)^.UpdateAll := true;
  //PSensorData(Data)^.Ident := '/amdcpu/0/load/0';
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TSensorWmiThread.Destroy;
begin
  Dispose(PSensorData(Data));
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TSensor.PrepareLine(aFormat: String): String;

  function DisplayText(const aDataID: Byte): String;
  var
    f: Single;
  begin
    if not Assigned(fUpdateThread) then
      exit;
    with PSensorData((fUpdateThread as TSensorWmiThread).Data)^ do begin
      case aDataID of
        1: f := Value;
        2: f := Min;
        3: f := Max;
      end;
      case SensorType of
        stLoad: result := ToStr(f, -1) +' %';
        stVolt: result := ToStr(f, -3)+' V';
        stTemp: result := ToStr(f, -1)+' °C';
        stFan: result := ToStr(f, 0)+' RPM';
        stClock: result := ToStr(f, 0)+' MHz';
      else
        result := ToStr(f);
      end;
    end;
  end;

begin
  //1 = parent-name
  //2 = sensor-name
  //3 = value
  //4 = min
  //5 = max
  if not Assigned(fUpdateThread) then
    exit;
  with PSensorData((fUpdateThread as TSensorWmiThread).Data)^ do begin
    result := StringReplace(aFormat, '%1', ParentName, [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%2', SensorName, [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%3', DisplayText(1), [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%4', DisplayText(2), [rfIgnoreCase, rfReplaceAll]);
    result := StringReplace(result, '%5', DisplayText(3), [rfIgnoreCase, rfReplaceAll]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TSensor.GetData: Single;
begin
  result := 0;
  if Assigned(fUpdateThread) then
    result := PSensorData((fUpdateThread as TSensorWmiThread).Data)^.Value;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TSensor.GetLargeDisplayString: String;
begin
  result := PrepareLine(fFirstLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TSensor.GetSmallDisplayString: String;
begin
  result := PrepareLine(fSecondLineFormat);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TSensor.GetFontColor: Cardinal;
begin
  Result := inherited GetFontColor;
  if Assigned(fUpdateThread) then
    with PSensorData((fUpdateThread as TSensorWmiThread).Data)^ do begin
      if ((fMaxValue <> 0) and (Value >= fMaxValue)) or
         ((fMinValue <> 0) and (Value <= fMinValue)) then
        result := fWarningColor;
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensor.SetSettings(const aData: PSettingsItemRec);
begin
  inherited SetSettings(aData);
  if not Assigned(fUpdateThread) then
    exit;
  with PSensorData((fUpdateThread as TSensorWmiThread).Data)^ do begin
    if fSensorIdent <> Ident then begin
      Ident := fSensorIdent;
      ClearData;
      UpdateAll := true;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TSensor.Create;
begin
  //1 = parent-name
  //2 = sensor-name
  //3 = value
  //4 = min
  //5 = max
  fFirstLineFormat := '%1 - %2: %3';
  fSecondLineFormat := 'min: %4; max %5';
  fWarningColor := $FEFF0000;
  inherited Create(TSensorWmiThread);

  fSettingItems.Add(CreateSettingsItem(
    'sensor', 'Sensordaten', SETTINGS_FLAG_NONE, dtUnknown, nil));
  fSettingItems.Add(CreateSettingsItem(
    'sensor/ident', 'Sensorident.', SETTINGS_FLAG_IDENT_STR, dtString, @fSensorIdent));
  fSettingItems.Add(CreateSettingsItem(
    'sensor/max_value', 'max. Wert', SETTINGS_FLAG_NONE, dtFloat32, @fMaxValue));
  fSettingItems.Add(CreateSettingsItem(
    'sensor/min_value', 'min. Wert', SETTINGS_FLAG_NONE, dtFloat32, @fMinValue));
  fSettingItems.Add(CreateSettingsItem(
    'sensor/alarm_color', 'Alarmfarbe', SETTINGS_FLAG_COLOR, dtInt32, @fWarningColor));
end;

end.

