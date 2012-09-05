unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, ExtDlgs, VirtualTrees, uHardwareMonitor, uModulAPI,
  uMCF;

type
  TNodeDataFlag = (nfFreeSettings, nfUpdateChildren, nfUpdateParent);
  TNodeDataFlags = set of TNodeDataFlag;

  { TMainForm }

  TMainForm = class(TForm)
    AddModulBt: TButton;
    SaveDialog: TSaveDialog;
    SaveSettingsBt: TButton;
    LoadSettingsBt: TButton;
    DelModulBt: TButton;
    DispHeightLab: TLabel;
    DisplayInfoGroup: TGroupBox;
    DisplaysCLB: TCheckListBox;
    DisplaysGroup: TGroupBox;
    DispNameLab: TLabel;
    DispSerialLab: TLabel;
    DispVersionLab: TLabel;
    DispWidthLab: TLabel;
    FontDialog: TFontDialog;
    ActiveModulesLB: TListBox;
    ModulesLB: TListBox;
    ModulesGroup: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ModAutorLab: TLabel;
    ModFileLab: TLabel;
    ModMailLab: TLabel;
    ModNameLab: TLabel;
    ModulInfoGroup: TGroupBox;
    ModVersionLab: TLabel;
    ModYearLab: TLabel;
    OpenDialog: TOpenDialog;
    OpenPicDialog: TOpenPictureDialog;
    PreviewPanel: TPanel;
    PreviewPB: TPaintBox;
    SettingsGroup: TGroupBox;
    SettingsVST: TVirtualStringTree;
    procedure ActiveModulesLBClick(Sender: TObject);
    procedure ActiveModulesLBDblClick(Sender: TObject);
    procedure AddModulBtClick(Sender: TObject);
    procedure DelModulBtClick(Sender: TObject);
    procedure DisplaysCLBClick(Sender: TObject);
    procedure DisplaysCLBClickCheck(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LoadSettingsBtClick(Sender: TObject);
    procedure ModMailLabClick(Sender: TObject);
    procedure ModulesLBClick(Sender: TObject);
    procedure PreviewPBDblClick(Sender: TObject);
    procedure PreviewPBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPBPaint(Sender: TObject);
    procedure SaveSettingsBtClick(Sender: TObject);
    procedure SettingsVSTDblClick(Sender: TObject);
    procedure SettingsVSTEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure SettingsVSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure SettingsVSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure SettingsVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SettingsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure SettingsVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
  private
    fModules: TModulesInfo;
    fDisplays: TDisplays;
    fActiveMonitor: THardwareMonitor;
    fActiveModul: THardwareMonitorModul;
    fActiveModulID: Integer;
    fMouseDown: Byte;
    fMouseDownPos, fOldModulPos, fOldModulSize: TPoint;
    fIsEditing, fPreviewNeedUpdate: Boolean;
    fFormatSettings: TFormatSettings;

    fBackgroundPic: String;
    fStartupPic: String;
    fUpdateRate: Integer;
    fModulNode: PVirtualNode;

    procedure OnChangeModulSizePos(Sender: TObject);
    procedure OnChangeBackground(Sender: TObject);
    procedure OnChangeStartPic(Sender: TObject);
    procedure OnChangeUpdateRate(Sender: TObject);
    procedure OnChangeSettings(Sender: TObject);

    function IsOnModul(const aModul: THardwareMonitorModul; const X, Y: Integer): Byte;
    procedure MonitorUpdate(aSendeR: TObject);
    procedure SetActiveMonitor(const aMonitor: THardwareMonitor);
    procedure SetActiveModul(const aIndex: Integer);
    procedure UpdateModulesLB;
    procedure UpdateActiveModulesLB;
    procedure UpdateDisplaysCLB;
    procedure RepaintNodes(const aNode: PVirtualNode);
    procedure LoadModulInfo(const ID: Integer);
    procedure SaveModulData(const ID: Integer);
    function CreateNode(aParent: PVirtualNode; aEvent: TNotifyEvent;
      aFlags: TNodeDataFlags; aSettings: TSettingsItem): PVirtualNode;
    procedure BuildModulSettingsTree(aModul: THardwareMonitorModul);
    procedure LoadDisplayInfo(const ID: Integer);
    procedure HardwareMonitorUpdate(aSender: TObject);
    procedure StartOpenHardwareMonitor;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  uUSBD480_API, LCLIntf, LCLType, uUtils, FileCtrl, JwaTlHelp32, windows,
  uSensorForm, uColorForm, GDIPAPI;

const
  MODUL_CURSER: array[0..9] of Integer = (crDefault, crSizeNWSE, crSizeNS, crSizeNESW,
    crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crHandPoint);
  NODE_DATA_UPDATE_CHILDREN = 1;
  NODE_DATA_UPDATE_PARENT   = 2;

type
  TNodeData = packed record
    Event: TNotifyEvent;
    Flags: TNodeDataFlags;
    Settings: TSettingsItem;
  end;
  PNodeData = ^TNodeData;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeModulSizePos(Sender: TObject);
begin
  if Assigned(fActiveModul) then begin
    fActiveModul.NeedResize := true;
    SaveModulData(fActiveModulID);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeBackground(Sender: TObject);
var
  pic: TPicture;
begin
  if not Assigned(fActiveMonitor) then
    exit;

  if FileExists(fBackgroundPic) then begin
    pic := TPicture.Create;
    try
      pic.LoadFromFile(fBackgroundPic);
      fActiveMonitor.SetBackground(pic.Graphic);
      fActiveMonitor.Update(true);
    finally
      pic.Free;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeStartPic(Sender: TObject);
var
  pic: TPicture;
begin
  if not Assigned(fActiveMonitor) then
    exit;

  if FileExists(fStartupPic) then begin
    pic := TPicture.Create;
    try
      pic.LoadFromFile(fStartupPic);
      fActiveMonitor.SetStartupImage(pic.Graphic);
    finally
      pic.Free;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeUpdateRate(Sender: TObject);
begin
  if fUpdateRate < 50 then
    fUpdateRate := 50;
  if Assigned(fActiveMonitor) then
    fActiveMonitor.UpdateRate := fUpdateRate;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeSettings(Sender: TObject);
begin
  if Assigned(fActiveModul) then begin
    fActiveModul.SetSettings;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TMainForm.IsOnModul(const aModul: THardwareMonitorModul; const X, Y: Integer): Byte;

  function Between(const aValue, aMin, aMax: Integer): Boolean;
  begin
    result := (aValue > aMin) and (aValue < aMax);
  end;

const
  C = 5;
  MIN = -1;
  MAX = 1;
  BET = 2;
  NONE = 0;
var
  px, py: Integer;
begin
  if Between(aModul.Position.X - X, -C, C) then
    px := MIN
  else if Between(aModul.Position.X + aModul.SmallSize.X - X, -C, C) then
    px := MAX
  else if Between(X, aModul.Position.X, aModul.Position.X + aModul.SmallSize.X) then
    px := BET
  else
    px := NONE;

  if Between(aModul.Position.Y - Y, -C, C) then
    py := MIN
  else if Between(aModul.Position.Y + aModul.SmallSize.Y - Y, -C, C) then
    py := MAX
  else if Between(Y, aModul.Position.Y, aModul.Position.Y + aModul.SmallSize.Y) then
    py := BET
  else
    py := NONE;

  result := 0;
  case px of
    MIN: begin
      case py of
        MIN: result := 1;
        BET: result := 8;
        MAX: result := 7;
      end;
    end;
    BET: begin
      case py of
        MIN: result := 2;
        BET: result := 9;
        MAX: result := 6;
      end;
    end;
    MAX: begin
      case py of
        MIN: result := 3;
        BET: result := 4;
        MAX: result := 5;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.MonitorUpdate(aSendeR: TObject);
begin
  PreviewPB.Repaint;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SetActiveMonitor(const aMonitor: THardwareMonitor);
begin
  if Assigned(fActiveMonitor) then
    fActiveMonitor.OnUpdate := nil;
  fActiveMonitor := aMonitor;
  AddModulBt.Enabled      := Assigned(fActiveMonitor);
  DelModulBt.Enabled      := Assigned(fActiveMonitor);
  ActiveModulesLB.Enabled := Assigned(fActiveMonitor);
  LoadSettingsBt.Enabled  := Assigned(fActiveMonitor);
  SaveSettingsBt.Enabled  := Assigned(fActiveMonitor);
  SettingsVST.Enabled     := Assigned(fActiveMonitor);
  PreviewPB.Enabled       := Assigned(fActiveMonitor);
  if Assigned(fActiveMonitor) then begin
    fUpdateRate := fActiveMonitor.UpdateRate;
    fActiveMonitor.OnUpdate := @MonitorUpdate;
    fActiveMonitor.Update(true);
    SettingsVST.Repaint;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SetActiveModul(const aIndex: Integer);
begin
  if Assigned(fActiveMonitor) and (aIndex >= 0) and (aIndex < fActiveMonitor.ModulCount) then begin
    fActiveModul   := fActiveMonitor.Modules[aIndex];
    fActiveModulID := aIndex;
  end else begin
    fActiveModul   := nil;
    fActiveModulID := -1;
  end;
  BuildModulSettingsTree(fActiveModul);
  ActiveModulesLB.ItemIndex := fActiveModulID;
  PreviewPB.Repaint;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.UpdateModulesLB;
var
  i: Integer;
  info: TModulInfoEx;
begin
  with ModulesLB.Items do begin
    BeginUpdate;
    Clear;
    for i := 0 to fModules.Count-1 do begin
      info := fModules[i];
      Add(format('%s %s', [info.Name, info.Version]));
    end;
    EndUpdate;
  end;
  if ModulesLB.Count > 0 then begin
    ModulesLB.ItemIndex := ModulesLB.Count-1;
    LoadModulInfo(ModulesLB.ItemIndex);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.UpdateActiveModulesLB;
var
  i: Integer;
begin
  if Assigned(fActiveMonitor) then
    with ActiveModulesLB.Items do begin
      BeginUpdate;
      Clear;
      for i := 0 to fActiveMonitor.ModulCount-1 do
        Add(fActiveMonitor.Modules[i].Name);
      EndUpdate;
    end;
  SetActiveModul(-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.UpdateDisplaysCLB;
var
  i: Integer;
  info: TDisplayInfo;
  s: String;
begin
  with DisplaysCLB.Items do begin
    BeginUpdate;
    Clear;
    for i := 0 to fDisplays.Count-1 do begin
      info := fDisplays[i];
      Add(format('%s', [info.Name]));
      DisplaysCLB.Checked[i] := Assigned(fDisplays.HardwareMonitor[i]);
    end;
    EndUpdate;
  end;
  if DisplaysCLB.Count > 0 then begin
    DisplaysCLB.ItemIndex := DisplaysCLB.Count-1;
    LoadDisplayInfo(DisplaysCLB.ItemIndex);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.RepaintNodes(const aNode: PVirtualNode);
var
  n: PVirtualNode;
begin
  if not Assigned(aNode) then
    exit;
  SettingsVST.RepaintNode(aNode);
  if SettingsVST.Expanded[aNode] then begin
    n := SettingsVST.GetFirstChild(aNode);
    while Assigned(n) do begin
      RepaintNodes(n);
      n := SettingsVST.GetNextSibling(n);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.LoadModulInfo(const ID: Integer);
var
  info: TModulInfoEx;
begin
  if ID < 0 then
    exit;
  info := fModules[ID];
  ModFileLab.Caption    := MiniMizeName(info.libName, ModFileLab.Canvas, ModulesGroup.Width-50);
  ModNameLab.Caption    := info.Name;
  ModVersionLab.Caption := info.Version;
  ModAutorLab.Caption   := info.Autor;
  ModMailLab.Caption    := info.eMail;
  ModYearLab.Caption    := IntToStr(info.Year);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SaveModulData(const ID: Integer);
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TMainForm.CreateNode(aParent: PVirtualNode; aEvent: TNotifyEvent;
  aFlags: TNodeDataFlags; aSettings: TSettingsItem): PVirtualNode;
var
  data: PNodeData;
begin
  result := SettingsVST.AddChild(aParent);
  data := SettingsVST.GetNodeData(result);
  data^.Event    := aEvent;
  data^.Flags    := aFlags;
  data^.Settings := aSettings;
  SettingsVST.ValidateNode(result, false);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.BuildModulSettingsTree(aModul: THardwareMonitorModul);

  function GetNode(const aIdent: String): PVirtualNode;
  var
    sList: TStringList;
    child: PVirtualNode;
    data: PNodeData;
    i: Integer;
    found: Boolean;
    ident: String;
  begin
    sList := TStringList.Create;
    try
      sList.Delimiter := '/';
      sList.DelimitedText := aIdent;

      i := 0;
      result := fModulNode;
      while i < sList.Count do begin
        try
          if Length(ident) > 0 then
            ident := ident + '/';
          ident := ident + sList[i];
          child := SettingsVST.GetFirstChild(result);
          found := false;
          while Assigned(child) do begin
            data := SettingsVST.GetNodeData(child);
            if Assigned(data) and Assigned(data^.Settings) and
               (data^.Settings.IdentStr = ident) then begin
              result := child;
              found := true;
              break;
            end;
            child := SettingsVST.GetNextSibling(child);
          end;
          if not found then
            result := CreateNode(result, nil, [nfFreeSettings],
              CreateSettingsItem(ident, sList[i], SETTINGS_FLAG_NONE, dtUnknown, nil));
        finally
          inc(i);
        end;
      end;
    finally
      sList.Free;
    end;
  end;

  procedure CreateColorTree(const aNode: PVirtualNode; const aSettingsItem: TSettingsItem);
  begin
    with aSettingsItem do begin
      CreateNode(aNode, @OnChangeSettings, [nfFreeSettings, nfUpdateParent],
        CreateSettingsItem(IdentStr+'/r', 'rot', SETTINGS_FLAG_NONE, dtByte, Data+2));
      CreateNode(aNode, @OnChangeSettings, [nfFreeSettings],
        CreateSettingsItem(IdentStr+'/g', 'grün', SETTINGS_FLAG_NONE, dtByte, Data+1));
      CreateNode(aNode, @OnChangeSettings, [nfFreeSettings],
        CreateSettingsItem(IdentStr+'/b', 'blau', SETTINGS_FLAG_NONE, dtByte, Data));
      CreateNode(aNode, @OnChangeSettings, [nfFreeSettings],
        CreateSettingsItem(IdentStr+'/a', 'alpha', SETTINGS_FLAG_NONE, dtByte, Data+3));
    end;
  end;

var
  node: PVirtualNode;
  d: PNodeData;
  i: Integer;
  settings: TSettingsItem;
begin
  SettingsVST.DeleteChildren(fModulNode);
  if not Assigned(aModul) then
    exit;
//Position und Größe
  node := CreateNode(fModulNode, @OnChangeModulSizePos, [nfFreeSettings],
    CreateSettingsItem('size', 'Größe', SETTINGS_FLAG_NONE, dtUnknown, nil));
//x-Position
  CreateNode(node, @OnChangeModulSizePos, [nfFreeSettings],
    CreateSettingsItem('size/pos_x', 'x-Position', SETTINGS_FLAG_NONE, dtInt32, @aModul.Position.x));
//y-Position
  CreateNode(node, @OnChangeModulSizePos, [nfFreeSettings],
    CreateSettingsItem('size/pos_y', 'y-Position', SETTINGS_FLAG_NONE, dtInt32, @aModul.Position.y));
//Breite
  CreateNode(node, @OnChangeModulSizePos, [nfFreeSettings],
    CreateSettingsItem('size/width', 'Breite', SETTINGS_FLAG_NONE, dtInt32, @aModul.SmallSize.x));
//Höhe
  CreateNode(node, @OnChangeModulSizePos, [nfFreeSettings],
    CreateSettingsItem('size/height', 'Höhe', SETTINGS_FLAG_NONE, dtInt32, @aModul.SmallSize.y));

//ModulSettings
  aModul.GetSettings;
  for i := 0 to aModul.SettingsCount-1 do begin
    node := GetNode(aModul.Settings[i].IdentStr);
    d := SettingsVST.GetNodeData(node);
    if Assigned(d) then begin
      if Assigned(d^.Settings) then
        FreeAndNil(d^.Settings);
      d^.Settings := aModul.Settings[i];
      d^.Flags := [];
      d^.Event := @OnChangeSettings;

      if (d^.Settings.Flags and SETTINGS_FLAG_COLOR > 0) and
         (d^.Settings.DataType = dtInt32) then begin
        CreateColorTree(node, d^.Settings);
        d^.Flags := d^.Flags + [nfUpdateChildren];
      end;
    end;
  end;
  SettingsVST.Expanded[fModulNode] := True;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.LoadDisplayInfo(const ID: Integer);
var
  info: TDisplayInfo;
begin
  if ID < 0 then
    exit;
  info := fDisplays[ID];
  DispNameLab.Caption    := info.Name;
  DispVersionLab.Caption := format('%d', [info.Version]);
  DispWidthLab.Caption   := format('%d px', [info.Width]);
  DispHeightLab.Caption  := format('%d px', [info.Height]);
  DispSerialLab.Caption  := info.SerialNumber;
  SetActiveMonitor(fDisplays.HardwareMonitor[ID]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.HardwareMonitorUpdate(aSender: TObject);
begin
  if Visible then
    PreviewPB.Repaint;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.StartOpenHardwareMonitor;

  function PrepareFilename(const aFilename: String): String;
  var
    list: TStringList;
    i: Integer;
  begin
    list := TStringList.Create;
    try
      result := ExtractFilePath(Application.ExeName);
      list.Text := StringReplace(aFilename, '\', sLineBreak, [rfIgnoreCase, rfReplaceAll]);
      for i := 0 to list.Count-1 do begin
        if list[i] = '..' then begin
          if (Length(result) > 0) and (result[Length(result)] = '\') then
            Delete(result, Length(result), 1);
          result := ExtractFilePath(result);
        end else begin
          if (Length(result) > 0) and (result[Length(result)] <> '\') then
            result := result + '\';
          result := result + list[i];
        end;
      end;
    finally
      list.Free;
    end;
  end;

const
  OPEN_HARDWARE_MONITOR_PATH = '..\OpenHardwareMonitor\OpenHardwareMonitor.exe';
var
  procInfo: TProcessEntry32;
  h: THandle;
  procList: TStringList;
  i: Integer;
  filename: String;
begin
  procList := TStringList.Create;
  try
    h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    procInfo.dwSize := SizeOf(procInfo);
    if Process32First(h, procInfo) then begin
      procList.Add(procInfo.szExeFile);
      while Process32Next(h, procInfo) do
        procList.Add(procInfo.szExeFile);
    end;

    filename := LowerCase(ExtractFileName(OPEN_HARDWARE_MONITOR_PATH));
    for i := 0 to procList.Count-1 do
      if (LowerCase(procList[i]) = filename) then
        exit;

    filename := PrepareFilename(OPEN_HARDWARE_MONITOR_PATH);
    ShellExecute(Handle, 'open', PAnsiChar(filename), '', PAnsiChar(ExtractFilePath(filename)), SW_HIDE);
  finally
    procList.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormCreate(Sender: TObject);
var
  node: PVirtualNode;
  data: PNodeData;
begin
  StartOpenHardwareMonitor;

  fFormatSettings.DecimalSeparator := '.';
  SettingsVST.EditDelay := 100;
  SettingsVST.NodeDataSize := SizeOf(TNodeData);

  CreateNode(nil, @OnChangeBackground, [nfFreeSettings],
    CreateSettingsItem('background', 'Hintergrund', SETTINGS_FLAG_PICTURE or SETTINGS_FLAG_DISP_CHANGE,
      dtString, @fBackgroundPic));

  CreateNode(nil, @OnChangeStartPic, [nfFreeSettings],
    CreateSettingsItem('startup', 'Startbild', SETTINGS_FLAG_PICTURE or SETTINGS_FLAG_DISP_CHANGE,
      dtString, @fStartupPic));

  CreateNode(nil, @OnChangeUpdateRate, [nfFreeSettings],
    CreateSettingsItem('update_rate', 'Update-Rate', SETTINGS_FLAG_NONE,
      dtInt32, @fUpdateRate));

  fModulNode := CreateNode(nil, @OnChangeStartPic, [nfFreeSettings],
    CreateSettingsItem('modul_settings', 'Modul Einstellungen', SETTINGS_FLAG_NONE,
      dtUnknown, nil));

  fDisplays := TDisplays.Create;
  fModules := TModulesInfo.Create;
  fModules.ModulPath := ExtractFilePath(Application.ExeName)+'modules';

  SetActiveMonitor(nil);
  fActiveModul   := nil;
  fActiveModulID := -1;

  UpdateDisplaysCLB;
  UpdateModulesLB;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.DelModulBtClick(Sender: TObject);
begin
  if Assigned(fActiveMonitor) and (ActiveModulesLB.ItemIndex >= 0) then begin
    fActiveMonitor.DelModul(ActiveModulesLB.ItemIndex);
    UpdateActiveModulesLB;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.AddModulBtClick(Sender: TObject);
begin
  if Assigned(fActiveMonitor) and (ModulesLB.ItemIndex >= 0) then begin
    fActiveMonitor.AddModul(fModules[ModulesLB.ItemIndex].libName);
    UpdateActiveModulesLB;
    SetActiveModul(fActiveMonitor.ModulCount-1);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ActiveModulesLBClick(Sender: TObject);
begin
  if (ActiveModulesLB.ItemIndex >= 0) and Assigned(fActiveMonitor) then
    SetActiveModul(ActiveModulesLB.ItemIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ActiveModulesLBDblClick(Sender: TObject);
var
  s: String;
  i: Integer;
  obj: THardwareMonitorModul;
begin
  i := ActiveModulesLB.ItemIndex;
  if Assigned(fActiveModul) and (i >= 0) then begin
    obj := fActiveMonitor.Modules[i];
    s := InputBox('Modulname', 'Bitte geben Sie den gewünschten Modulnamen ein:', obj.Name);
    if (s <> obj.Name) then begin
      try
        obj.Name := s;
        ActiveModulesLB.Items[i] := obj.Name;
      except
        MessageDlg('Fehler', 'Es exestiert bereits ein Modul mit diesem Namen!', mtError, [mbOK], 0);
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.DisplaysCLBClick(Sender: TObject);
begin
  LoadDisplayInfo(DisplaysCLB.ItemIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.DisplaysCLBClickCheck(Sender: TObject);
begin
  with DisplaysCLB do
    if (ItemIndex >= 0) then begin
      if (Checked[ItemIndex]) then begin
        SetActiveMonitor(fDisplays.ActivateHardwareMonitor(ItemIndex));
      end else begin
        fDisplays.DeactivateHardwareMonitor(ItemIndex);
        SetActiveMonitor(nil);
      end;
      PreviewPB.Repaint;
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fModules);
  FreeAndNil(fDisplays);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function Move(const p: TPoint; const x, y: Integer): TPoint;
  begin
    result := Classes.Point(p.X + x, p.Y + y);
    Key := 0;
  end;

begin
  if Assigned(fActiveModul) and not fIsEditing then begin
    if (ssShift in Shift) then begin
      case Key of
        VK_UP   : fActiveModul.SmallSize := Move(fActiveModul.SmallSize,  0, -1);
        VK_DOWN : fActiveModul.SmallSize := Move(fActiveModul.SmallSize,  0,  1);
        VK_LEFT : fActiveModul.SmallSize := Move(fActiveModul.SmallSize, -1,  0);
        VK_RIGHT: fActiveModul.SmallSize := Move(fActiveModul.SmallSize,  1,  0);
      end;
    end else begin
      case Key of
        VK_UP   : fActiveModul.Position := Move(fActiveModul.Position,  0, -1);
        VK_DOWN : fActiveModul.Position := Move(fActiveModul.Position,  0,  1);
        VK_LEFT : fActiveModul.Position := Move(fActiveModul.Position, -1,  0);
        VK_RIGHT: fActiveModul.Position := Move(fActiveModul.Position,  1,  0);
      end;
    end;
    RepaintNodes(fModulNode);
    PreviewPB.Repaint;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.LoadSettingsBtClick(Sender: TObject);
var
  mcf: TmcfFile;
begin
  if not Assigned(fActiveMonitor) then
    exit;
  if OpenDialog.Execute then begin
    mcf := TmcfFile.Create;
    try
      mcf.LoadFromFile(OpenDialog.FileName);
      fActiveMonitor.LoadFromFile(mcf, fModules);
      SettingsVST.Repaint;
      UpdateActiveModulesLB;
      UpdateDisplaysCLB;
    finally
      mcf.Free;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ModMailLabClick(Sender: TObject);
begin
  OpenURL('mailto: '+ModMailLab.Caption);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ModulesLBClick(Sender: TObject);
begin
  LoadModulInfo(ModulesLB.ItemIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBDblClick(Sender: TObject);
begin
  if Assigned(fActiveMonitor) then
    fActiveMonitor.EmulateTouch(fMouseDownPos);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDownPos := Classes.Point(X, Y);
  if Assigned(fActiveModul) then begin
    fMouseDown := IsOnModul(fActiveModul, X, Y);
    fOldModulPos  := fActiveModul.Position;
    fOldModulSize := fActiveModul.SmallSize;
  end;
end;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  function P(const aX, aY: Integer): TPoint;
  begin
    result.X := aX;
    result.Y := aY;
  end;

var
  dX, dY: Integer;
begin
  if not Assigned(fActiveMonitor) or
     not Assigned(fActiveModul) or
     fPreviewNeedUpdate or
     Assigned(fActiveMonitor.LargeModul) then
    exit;

  if fMouseDown = 0 then begin
    PreviewPB.Cursor := MODUL_CURSER[IsOnModul(fActiveModul, X, Y)];
    exit;
  end;

  dX := (X - fMouseDownPos.X);
  dY := (Y - fMouseDownPos.Y);
  case fMouseDown of
    1: begin
      fActiveModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y + dY);
      fActiveModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y - dY);
    end;
    2: begin
      fActiveModul.Position  := P(fOldModulPos.X, fOldModulPos.Y + dY);
      fActiveModul.SmallSize := P(fOldModulSize.X, fOldModulSize.Y - dY);
    end;
    3: begin
      fActiveModul.Position  := P(fOldModulPos.X, fOldModulPos.Y + dY);
      fActiveModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y - dY);
    end;
    4: begin
      fActiveModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y);
    end;
    5: begin
      fActiveModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y + dY);
    end;
    6: begin
      fActiveModul.SmallSize := P(fOldModulSize.X, fOldModulSize.Y + dY);
    end;
    7: begin
      fActiveModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y);
      fActiveModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y + dY);
    end;
    8: begin
      fActiveModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y);
      fActiveModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y);
    end;
    9: begin
      fActiveModul.Position := P(fOldModulPos.X + dX, fOldModulPos.Y + dY)
    end;
  end;
  SaveModulData(fActiveModulID);
  RepaintNodes(fModulNode);
  fPreviewNeedUpdate := True;
  PreviewPB.Repaint;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := crDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBPaint(Sender: TObject);
var
  i: Integer;
  obj: THardwareMonitorModul;

  procedure DrawRect;
  begin
    if Assigned(obj) then begin
      PreviewPB.Canvas.Rectangle(
        obj.Position.x, obj.Position.y,
        obj.Position.x + obj.SmallSize.x,
        obj.Position.y + obj.SmallSize.y);
    end;
  end;

begin
  if fPreviewNeedUpdate then
    fPreviewNeedUpdate := false;

  with PreviewPB.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Rectangle(-5, -5, PreviewPB.Width+5, PreviewPB.Height+5);
    if Assigned(fActiveMonitor) then begin
      Draw(0, 0, fActiveMonitor);
      if not Assigned(fActiveMonitor.LargeModul) then begin
        Pen.Color   := clBlack;
        Pen.Width   := 1;
        Brush.Style := bsClear;
        for i := 0 to fActiveMonitor.ModulCount-1 do begin
          obj := fActiveMonitor.Modules[i];
          DrawRect;
        end;
        Pen.Color := clRed;
        obj := fActiveModul;
        DrawRect;
      end;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SaveSettingsBtClick(Sender: TObject);
var
  mcf: TmcfFile;
begin
  if not Assigned(fActiveMonitor) then
    exit;
  mcf := TmcfFile.Create;
  try
    if SaveDialog.Execute then begin
      if FileExists(SaveDialog.FileName) and (MessageDlg('Datei überscheriben?',
        'Die Datei exestiert bereits. Soll die Datei überschreiben werden?', mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
          exit;
      fActiveMonitor.SaveToFile(mcf);
      mcf.SetString('Background', ExtractFileNameWithoutExt(SaveDialog.FileName)+'.bmp');
      fActiveMonitor.Background.SaveToFile(mcf.GetString('Background'));
      mcf.SaveToFile(SaveDialog.FileName);
    end;
  finally
    mcf.Free;
    SetActiveModul(fActiveModulID);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTDblClick(Sender: TObject);
var
  n: PVirtualNode;
  d: PNodeData;

  procedure ChangeFont;
  var
    pName: PString;
    pSize: PSingle;
    pStyle, pColor: PInteger;
    child: PVirtualNode;
    childData: PNodeData;
    baseIdent: String;
  begin
    child := SettingsVST.GetFirstChild(n);
    baseIdent := '';
    if Assigned(d) and Assigned(d^.Settings) then
      baseIdent := d^.Settings.IdentStr;

    pName  := nil;
    pSize  := nil;
    pStyle := nil;
    pColor := nil;
    while Assigned(child) do begin
      childData := SettingsVST.GetNodeData(child);
      if Assigned(childData) and Assigned(childData^.Settings) then
        with childData^.Settings do begin
          if (IdentStr = baseIdent+'/name') and (DataType = dtString) then
            pName := Data;
          if (IdentStr = baseIdent+'/size') and (DataType = dtFloat32) then
            pSize := Data;
          if (IdentStr = baseIdent+'/style') and (DataType = dtInt32) then
            pStyle := Data;
          if (IdentStr = baseIdent+'/color') and (DataType = dtInt32) then
            pColor := Data;
        end;
      child := SettingsVST.GetNextSibling(child);
    end;
    with FontDialog.Font do begin
      if Assigned(pName) then
        Name := pName^;
      if Assigned(pSize) then
        Size := Round(pSize^);
      FontDialog.Font.Style := [];
      if Assigned(pStyle) then begin
        if ((pStyle^ and Integer(FontStyleBold)) > 0) then
          FontDialog.Font.Style := FontDialog.Font.Style + [fsBold];
        if ((pStyle^ and Integer(FontStyleItalic)) > 0) then
          FontDialog.Font.Style := FontDialog.Font.Style + [fsItalic];
        if ((pStyle^ and Integer(FontStyleStrikeout)) > 0) then
          FontDialog.Font.Style := FontDialog.Font.Style + [fsStrikeOut];
        if ((pStyle^ and Integer(FontStyleUnderline)) > 0) then
          FontDialog.Font.Style := FontDialog.Font.Style + [fsUnderline];
      end;
      if Assigned(pColor) then begin
        Color :=
          ((pColor^ shr  16) and $FF) or
          ((pColor^ and $FF) shl  16) or
          ( pColor^ and $FF00);
      end else
        Color := clBlack;
      if FontDialog.Execute then  begin
        if Assigned(pName) then
          pName^ := Name;
        if Assigned(pSize) then
          pSize^ := Size;
        if Assigned(pStyle) then begin
          pStyle^ := 0;
          if fsBold in Style then
            pStyle^ := pStyle^ or Integer(FontStyleBold);
          if fsItalic in Style then
            pStyle^ := pStyle^ or Integer(FontStyleItalic);
          if fsStrikeOut in Style then
            pStyle^ := pStyle^ or Integer(FontStyleStrikeout);
          if fsUnderline in Style then
            pStyle^ := pStyle^ or Integer(FontStyleUnderline);
        end;
        if Assigned(pColor) then
          pColor^ :=
            (pColor^ and $FF000000) or
            ((Color shr  16) and $FF) or
            ((Color and $FF) shl  16) or
            ( Color and $FF00);
        if Assigned(d^.Event) then
          d^.Event(self);
      end;
    end;
  end;

begin
  n := SettingsVST.FocusedNode;
  if Assigned(n) then begin
    d := SettingsVST.GetNodeData(n);
    if Assigned(d) and Assigned(d^.Settings) then with (d^.Settings) do begin
      case DataType of
        dtUnknown: begin
          if (SETTINGS_FLAG_FONT_DATA and Flags > 0) then begin
            ChangeFont;
          end;
        end;
        dtString: if Assigned(Data) then begin
        //Picture Dialog
          if (SETTINGS_FLAG_PICTURE and Flags > 0) then begin
            if OpenDialog.Execute then begin
              PString(Data)^ := OpenDialog.FileName;
              if Assigned(d^.Event) then
                d^.Event(self);
            end;
        //File Dialog
          end else if (SETTINGS_FLAG_FILE and Flags > 0) then begin
            if OpenPicDialog.Execute then begin
              PString(Data)^ := OpenPicDialog.FileName;
              if Assigned(d^.Event) then
                d^.Event(self);
            end;
        //Sensor Dialog
          end else if (SETTINGS_FLAG_IDENT_STR and Flags > 0) then begin
            if SensorForm.ShowModal = mrOK then begin
              PString(Data)^ := SensorForm.IdentStr;
              if Assigned(d^.Event) then
                d^.Event(self);
            end;
        //normal String
          end else begin
            if n^.ChildCount <= 0 then
              SettingsVST.EditNode(n, 1);
          end;
        end;
        dtInt32: if Assigned(Data) then begin
          if (Flags and SETTINGS_FLAG_COLOR > 0) then begin
            if ColorForm.ShowModalColor(PCardinal(Data)^) = mrOK then begin
              PCardinal(Data)^ := ColorForm.Color.c;
              if Assigned(d^.Event) then
                d^.Event(self);
            end;
          end else if n^.ChildCount <= 0 then
            SettingsVST.EditNode(n, 1);
        end;
        dtBool: if Assigned(Data) then begin
          System.PBoolean(Data)^ := not System.PBoolean(Data)^;
          if Assigned(d^.Event) then
            d^.Event(self);
        end;
      else
        if n^.ChildCount <= 0 then
          SettingsVST.EditNode(n, 1);
      end;

      if (nfUpdateChildren in d^.Flags) then
        RepaintNodes(n);
      if (nfUpdateParent in d^.Flags) then
        SettingsVST.RepaintNode(n^.Parent);
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  fIsEditing := false;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  fIsEditing := false;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  d: PNodeData;
begin
  d := SettingsVST.GetNodeData(Node);
  Allowed := Assigned(Node) and (SettingsVST.ChildCount[Node] <= 0) and
    Assigned(d) and Assigned(d^.Settings) and Assigned(d^.Settings.Data) and (Column = 1);
  fIsEditing := Allowed;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  d: PNodeData;
begin
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) then begin
    if (nfFreeSettings in d^.Flags) then
      FreeAndNil(d^.Settings);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  d: PNodeData;
begin
  CellText := '';
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) and Assigned(d^.Settings) then begin
    case Column of
      0: CellText := d^.Settings.DisplayStr;
      1: begin
        if Assigned(d^.Settings.Data) then with (d^.Settings) do begin
          if (Flags and SETTINGS_FLAG_DISP_CHANGE > 0) then
            CellText := '[ändern]'
          else
            case DataType of
              dtInt32: begin
                if (Flags and (SETTINGS_FLAG_COLOR or SETTINGS_FLAG_HEX) > 0) then
                  CellText := IntToHex(PCardinal(Data)^, 8)
                else
                  CellText := IntToStr(PInteger(Data)^);
              end;
              dtByte: begin
                if (Flags and (SETTINGS_FLAG_COLOR or SETTINGS_FLAG_HEX) > 0) then
                  CellText := IntToHex(PByte(Data)^, 8)
                else
                  CellText := IntToStr(PByte(Data)^);
              end;
              dtFloat32: begin
                CellText := ToStr(PSingle(Data)^, -3);
              end;
              dtBool: begin
                if System.PBoolean(Data)^ then
                  CellText := 'true'
                else
                  CellText := 'false';
              end;
              dtString: begin
                CellText := PString(Data)^;
              end;
            end;
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  d: PNodeData;
  i: Integer;
  f: Single;
  callEvent: Boolean;
begin
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) and Assigned(d^.Settings) and Assigned(d^.Settings.Data) then begin
    callEvent := false;
    with d^.Settings do begin
      case DataType of
        dtString: begin
          PString(Data)^ := NewText;
          callEvent := true;
        end;
        dtInt32, dtByte: begin
          if (Flags and (SETTINGS_FLAG_COLOR or SETTINGS_FLAG_HEX) > 0) then
            callEvent := TryHexToInt(NewText, Cardinal(i))
          else
            callEvent := TryStrToInt(NewText, i);
          if not callEvent then begin
            MessageDlg('Error', Format('''%s'' ist keine gültige Zahl!', [NewText]), mtError, [mbOK], 0)
          end else begin
            case DataType of
              dtInt32:
                PInteger(Data)^ := i;
              dtByte: begin
                if i > 255 then
                  i := 255;
                if i < 0 then
                  i := 0;
                PByte(Data)^ := i;
              end;
            end;
          end;
        end;
        dtFloat32: begin
          callEvent := TryStrToFloat(StringReplace(NewText, ',', '.', [rfIgnoreCase, rfReplaceAll]), f, fFormatSettings);
          if not callEvent then begin
            MessageDlg('Error', Format('''%s'' ist keine gültige Fließkommazahl!', [NewText]), mtError, [mbOK], 0)
          end else begin
            PSingle(Data)^ := f
          end;
        end;
      end;
    end;
    if callEvent and Assigned(d^.Event) then
      d^.Event(self);

    if (nfUpdateChildren in d^.Flags) then
        RepaintNodes(Node);
    if (nfUpdateParent in d^.Flags) then
      SettingsVST.RepaintNode(Node^.Parent);
  end;
end;

end.

