unit uSensorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActiveX, ComObj, Variants, fgl;

type
  TNodeDataType = (dtHardware = 1, dtSensor, dtUnknown, dtClock, dtLoad, dtTemp, dtVolt, dtFan);
  TNodeDataTypes = set of TNodeDataType;
  TNodeData = record
    Typ: TNodeDataTypes;
    Identifier: String;
    Name: String;
    Min: Single;
    Max: Single;
    Value: Single;
  end;
  PNodeData = ^TNodeData;

  TSensorDataList = specialize TFPGList<PNodeData>;

  { TSensorForm }

  TSensorForm = class(TForm)
    OKBt: TButton;
    CancelBt: TButton;
    SensorVST: TVirtualStringTree;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtClick(Sender: TObject);
    procedure SensorVSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure SensorVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SensorVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure TimerTimer(Sender: TObject);
  private
    fIdentStr: String;
    fSWbemLocator: OLEVariant;
    fWMIService: OLEVariant;
    fRootNode: PVirtualNode;
    fSensorData: TSensorDataList;

    procedure BuildTree;
    procedure UpdateTree;
  public
    property IdentStr: String read fIdentStr;
  end;

var
  SensorForm: TSensorForm;

implementation

{$R *.lfm}

uses
  uUtils;

const
  WbemUser            ='';
  WbemPassword        ='';
  WbemComputer        ='localhost';
  wbemFlagForwardOnly = $00000020;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.FormCreate(Sender: TObject);
begin
  fSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  fWMIService   := FSWbemLocator.ConnectServer(WbemComputer, 'root\OpenHardwareMonitor', WbemUser, WbemPassword);
  fSensorData   := TSensorDataList.Create;
  SensorVST.NodeDataSize := SizeOf(TNodeData);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.FormDestroy(Sender: TObject);
begin
  SensorVST.Clear;
  FreeAndNil(fSensorData);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.FormShow(Sender: TObject);
begin
  BuildTree;
  Timer.Enabled := True;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.OKBtClick(Sender: TObject);
var
  p: PNodeData;
begin
  fIdentStr := '';
  p := SensorVST.GetNodeData(SensorVST.FocusedNode);
  if Assigned(p) and (dtSensor in p^.Typ) then
    fIdentStr := p^.Identifier;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.SensorVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  p1, p2: PNodeData;
begin
  result := 0;
  p1 := SensorVST.GetNodeData(Node1);
  p2 := SensorVST.GetNodeData(Node2);
  if Assigned(p1) and Assigned(p2) then
    result := CompareStr(p1^.Identifier, p2^.Identifier);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.SensorVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  p: PNodeData;
begin
  p := SensorVST.GetNodeData(Node);
  if Assigned(p) then begin
    p^.Identifier := '';
    p^.Name := '';
    fSensorData.Remove(p);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.SensorVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  data: PNodeData;

  function DisplayText: String;
  var
    f: Single;
  begin
    case Column of
      1: f := data^.Value;
      2: f := data^.Min;
      3: f := data^.Max;
    end;
    if (dtLoad in data^.Typ) then
      result := ToStr(f, -1) +' %'
    else if (dtVolt in data^.Typ) then
      result := ToStr(f, -3)+' V'
    else if (dtTemp in data^.Typ) then
      result := ToStr(f, -1)+' °C'
    else if (dtFan in data^.Typ) then
      result := ToStr(f, 0)+' RPM'
    else if (dtClock in data^.Typ) then
      result := ToStr(f, 0)+' MHz'
    else
      result := ToStr(f);
  end;

begin
  CellText := '';
  data := SensorVST.GetNodeData(Node);
  if Assigned(data) then begin
    case Column of
      0: if data^.Name <> '' then
          CellText := data^.Name
        else begin
          if (dtLoad in data^.Typ) then
            CellText := 'Auslatung'
          else if (dtVolt in data^.Typ) then
            CellText := 'Spannung'
          else if (dtTemp in data^.Typ) then
            CellText := 'Temperatur'
          else if (dtFan in data^.Typ) then
            CellText := 'Lüfter'
          else if (dtClock in data^.Typ) then
            CellText := 'Takt'
          else
            CellText := data^.Identifier;
        end;
      1, 2, 3: if (dtSensor in data^.Typ) then
        CellText := DisplayText;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.TimerTimer(Sender: TObject);
begin
  if not Visible then
    Timer.Enabled := false
  else
    UpdateTree;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.BuildTree;
var
  WbemObjectSet: OLEVariant;
  WbemObject: Variant;
  Enum: IEnumvariant;
  Data: TNodeData;
  p: PNodeData;
  IdentList: TStringList;

  function GetNode(const aNode: PVirtualNode; const aIdent: String): PVirtualNode;
  var
    child: PVirtualNode;
  begin
    child := SensorVST.GetFirstChild(aNode);
    while Assigned(child) do begin
      p := SensorVST.GetNodeData(child);
      if (p^.Identifier = aIdent) then begin
        result := child;
        exit;
      end;
      child := SensorVST.GetNextSibling(child);
    end;
    result := SensorVST.AddChild(aNode);
    p := SensorVST.GetNodeData(result);
    SensorVST.ValidateNode(result, false);
    p^.Identifier := aIdent;
    p^.Typ        := [dtHardware];
  end;

  procedure InsertIntoTree;
  var
    node: PVirtualNode;
    i: Integer;
    s: String;
    sType: TNodeDataType;
    ident: String;
  begin
    IdentList.Text := StringReplace(Data.Identifier, '/', sLineBreak, [rfIgnoreCase, rfReplaceAll]);
    while (IdentList.Count > 0) and (IdentList[0] = '') do
      IdentList.Delete(0);
    while (IdentList.Count > 0) and (IdentList[IdentList.Count-1] = '') do
      IdentList.Delete(IdentList.Count-1);
    node := fRootNode;
    i := 0;
    ident := '';
    sType := dtUnknown;
    while (i < IdentList.Count) do begin
      s := IdentList[i];
      ident := ident + '/' + s;
      node := GetNode(node, ident);
      p := SensorVST.GetNodeData(node);
      if (s = 'clock') then
        sType := dtClock
      else if (s = 'fan') then
        sType := dtFan
      else if (s = 'voltage') then
        sType := dtVolt
      else if (s = 'temperature') then
        sType := dtTemp
      else if (s = 'load') then
        sType := dtLoad;
      if Assigned(p) then
        p^.Typ := p^.Typ + [sType];
      inc(i);
    end;
    p := SensorVST.GetNodeData(node);
    p^ := Data;
    p^.Typ := [dtSensor, sType];
    fSensorData.Add(p);
  end;

  function GetHardwareName(const aIdent: String): String;
  var
    s: Variant;
  begin
    result := '';
    s := 'SELECT Name FROM Hardware WHERE Identifier = "' + aIdent + '"';
    WbemObjectSet := fWMIService.ExecQuery(s, 'WQL', wbemFlagForwardOnly);
    Enum          := IUnknown(WbemObjectSet._NewEnum) as IEnumVariant;
    if Enum.Next(1, WbemObject, nil) = 0 then
      result := VarToStr(WbemObject.Properties_.Item('Name').Value);
  end;

  procedure UpdateHardwareNames(const aNode: PVirtualNode);
  var
    child: PVirtualNode;
  begin
    p := SensorVST.GetNodeData(aNode);
    if Assigned(p) and (dtHardware in p^.Typ) and (dtUnknown in p^.Typ) then
      p^.Name := GetHardwareName(p^.Identifier);
    child := SensorVST.GetFirstChild(aNode);
    while Assigned(child) do begin
      UpdateHardwareNames(child);
      child := SensorVST.GetNextSibling(child);
    end;
  end;

begin
  IdentList := TStringList.Create;
  try
    SensorVST.Clear;
    fSensorData.Clear;
    fRootNode := SensorVST.AddChild(nil, nil);
    p := SensorVST.GetNodeData(fRootNode);
    FillChar(p^, SizeOf(p^), 0);
    p^.Typ  := [dtHardware];
    p^.Name := 'Sensoren';
    p^.Identifier := '/';

    WbemObjectSet := fWMIService.ExecQuery('SELECT * FROM Sensor', 'WQL', wbemFlagForwardOnly);
    Enum          := IUnknown(WbemObjectSet._NewEnum) as IEnumVariant;
    FillChar(Data, SizeOf(Data), 0);
    while Enum.Next(1, WbemObject, nil) = 0 do begin
      Data.Typ := [dtSensor];
      Data.Identifier := VarToStr(WbemObject.Properties_.Item('Identifier').Value);
      Data.Name       := VarToStr(WbemObject.Properties_.Item('Name').Value);
      Data.Min        := VarToFloat(WbemObject.Properties_.Item('Min').Value);
      Data.Max        := VarToFloat(WbemObject.Properties_.Item('Max').Value);
      Data.Value      := VarToFloat(WbemObject.Properties_.Item('Value').Value);
      InsertIntoTree;
    end;

    UpdateHardwareNames(fRootNode);
    SensorVST.SortTree(0, sdAscending);
    SensorVST.FullExpand;
  finally
    IdentList.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TSensorForm.UpdateTree;

  function FindNodeData(const aIdent: String): PNodeData;
  var
    i: Integer;
  begin
    for i := 0 to fSensorData.Count-1 do
      if (fSensorData[i]^.Identifier = aIdent) then begin
        result := fSensorData[i];
        exit;
      end;
    result := nil;
  end;

var
  WbemObjectSet: OLEVariant;
  WbemObject: Variant;
  Enum: IEnumvariant;
  p: PNodeData;
begin
  WbemObjectSet := fWMIService.ExecQuery('SELECT * FROM Sensor', 'WQL', wbemFlagForwardOnly);
  Enum          := IUnknown(WbemObjectSet._NewEnum) as IEnumVariant;
  while Enum.Next(1, WbemObject, nil) = 0 do begin
    p := FindNodeData(VarToStr(WbemObject.Properties_.Item('Identifier').Value));
    if Assigned(p) then begin
      p^.Min   := VarToFloat(WbemObject.Properties_.Item('Min').Value);
      p^.Max   := VarToFloat(WbemObject.Properties_.Item('Max').Value);
      p^.Value := VarToFloat(WbemObject.Properties_.Item('Value').Value);
    end;
  end;
  SensorVST.Repaint;
end;

end.

