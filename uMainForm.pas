unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, ExtDlgs, VirtualTrees, uHardwareMonitor, uModulAPI,
  uMCF;

type
  TSettingsItemEx = packed record
    Event: TNotifyEvent;
    Tag: PtrInt;
    i: TSettingsItem;
  end;
  PSettingsItemEx = ^TSettingsItemEx;

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
    procedure Button1Click(Sender: TObject);
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
    fHardwareMonitor: THardwareMonitor;
    fCurrentModul: THardwareMonitorModul;
    fCurrentModulID: Integer;
    fMouseDown: Byte;
    fMouseDownPos, fOldModulPos, fOldModulSize: TPoint;
    fIsEditing: Boolean;
    fFormatSettings: TFormatSettings;

    fBgNode: PVirtualNode;
    fBgFilename: String;
    fUpdateRate: Integer;
    fModulNode: PVirtualNode;

    procedure OnChangeModulSizePos(Sender: TObject);
    procedure OnChangeBackground(Sender: TObject);
    procedure OnChangeUpdateRate(Sender: TObject);
    procedure OnChangeSettings(Sender: TObject);

    function IsOnModul(const aModul: THardwareMonitorModul; const X, Y: Integer): Byte;
    procedure UpdateModulesLB;
    procedure UpdateActiveModulesLB;
    procedure UpdateDisplaysCLB;
    procedure RepaintNodes(const aNode: PVirtualNode);
    procedure LoadModulInfo(const ID: Integer);
    procedure SaveModulData(const aID: Integer);
    procedure BuildModulSettingsTree(aModul: THardwareMonitorModul);
    procedure LoadDisplayInfo(const ID: Integer);
    procedure HardwareMonitorUpdate(aSender: TObject);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  uUSBD480_API, LCLIntf, LCLType, uUtils, FileCtrl, uColorForm;

const
  MODUL_CURSER: array[0..9] of Integer = (crDefault, crSizeNWSE, crSizeNS, crSizeNESW,
    crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crHandPoint);
  NODE_DATA_UPDATE_CHILDREN = 1;
  NODE_DATA_UPDATE_PARENT   = 2;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeModulSizePos(Sender: TObject);
begin
  if Assigned(fCurrentModul) then begin
    fCurrentModul.NeedResize := true;
    SaveModulData(fCurrentModulID);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeBackground(Sender: TObject);
var
  d: PSettingsItemEx;
  pic: TPicture;
begin
  d := SettingsVST.GetNodeData(fBgNode);
  if Assigned(d) and (d^.i.DataType = dtPicture) and FileExists(PString(d^.i.Data)^) then begin
    pic := TPicture.Create;
    try
      pic.LoadFromFile(PString(d^.i.Data)^);
      fHardwareMonitor.SetBackground(pic.Graphic);
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
  fHardwareMonitor.UpdateTime := fUpdateRate;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.OnChangeSettings(Sender: TObject);
begin
  if Assigned(fCurrentModul) then begin
    fCurrentModul.SetSettings;
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
procedure TMainForm.UpdateModulesLB;
var
  i: Integer;
  info: TModulInfoEx;
begin
  with ModulesLB.Items do begin
    BeginUpdate;
    Clear;
    for i := 0 to fHardwareMonitor.ModulCount-1 do begin
      info := fHardwareMonitor.Modules[i];
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
  with ActiveModulesLB.Items do begin
    BeginUpdate;
    Clear;
    for i := 0 to fHardwareMonitor.ActiveModulCount-1 do begin
      Add(fHardwareMonitor.ActiveModules[i].Name);
    end;
    EndUpdate;
  end;
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
    for i := 0 to fHardwareMonitor.DisplayCount-1 do begin
      info := fHardwareMonitor.Displays[i];
      Add(format('%s', [info.Name]));
    end;
    EndUpdate;
  end;
  if Assigned(fHardwareMonitor.ActiveDisplay) then
    for i := 0 to fHardwareMonitor.DisplayCount-1 do
      DisplaysCLB.Checked[i] :=
        (fHardwareMonitor.ActiveDisplay.DisplayInfo.SerialNumber =
         fHardwareMonitor.Displays[i].SerialNumber);
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
  n := SettingsVST.GetFirstChild(aNode);
  while Assigned(n) do begin
    RepaintNodes(n);
    n := SettingsVST.GetNextSibling(n);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.LoadModulInfo(const ID: Integer);
var
  info: TModulInfoEx;
begin
  if ID < 0 then
    exit;
  info := fHardwareMonitor.Modules[ID];
  ModFileLab.Caption    := info.libName;
  ModNameLab.Caption    := info.Name;
  ModVersionLab.Caption := info.Version;
  ModAutorLab.Caption   := info.Autor;
  ModMailLab.Caption    := info.eMail;
  ModYearLab.Caption    := IntToStr(info.Year);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SaveModulData(const aID: Integer);
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.BuildModulSettingsTree(aModul: THardwareMonitorModul);
var
  n1: PVirtualNode;
  d: PSettingsItemEx;
  i: Integer;
  settings: TSettingsItem;

  //////////////////////////////////////////////////////////////////////////////
  function InitData(const aNode: PVirtualNode; const aName: String): PSettingsItemEx;
  begin
    result := SettingsVST.GetNodeData(aNode);
    FillChar(result^, SizeOf(result^), 0);
    result^.i.Name := PChar(aName);
  end;

  //////////////////////////////////////////////////////////////////////////////
  procedure InitDataEx(const aNode: PVirtualNode; const aName: String; aDataType: TDataType;
    const aData: Pointer; const aEvent: TNotifyEvent; const aTag: PtrInt);
  var
    d: PSettingsItemEx;
  begin
    d := InitData(aNode, aName);
    d^.i.DataType := aDataType;
    d^.i.Data := aData;
    d^.Event  := aEvent;
    d^.Tag    := aTag;
  end;

  //////////////////////////////////////////////////////////////////////////////
  procedure CreateColorNode(const aNode: PVirtualNode);
  var
    oldD, newD: PSettingsItemEx;
    newNode: PVirtualNode;
  begin
    oldD := SettingsVST.GetNodeData(aNode);
    oldD^.Tag := NODE_DATA_UPDATE_CHILDREN;
    if Assigned(aNode) and Assigned(oldD) then begin
      InitDataEx(SettingsVST.AddChild(aNode), 'r', dtByte, oldD^.i.Data+2, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      InitDataEx(SettingsVST.AddChild(aNode), 'g', dtByte, oldD^.i.Data+1, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      InitDataEx(SettingsVST.AddChild(aNode), 'b', dtByte, oldD^.i.Data+0, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      InitDataEx(SettingsVST.AddChild(aNode), 'a', dtByte, oldD^.i.Data+3, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
    end;
  end;

  //////////////////////////////////////////////////////////////////////////////
  procedure CreateFontNode(const aNode: PVirtualNode);
  var
    oldD: PSettingsItemEx;
    n: PVirtualNode;
  begin
    oldD := SettingsVST.GetNodeData(aNode);
    if Assigned(aNode) and Assigned(oldD) then begin
      InitDataEx(SettingsVST.AddChild(aNode), 'Name', dtString, @PModulFontDataEx(oldD^.i.Data)^.Name, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      n := SettingsVST.AddChild(aNode);
      InitDataEx(n, 'Farbe', dtColor, @PModulFontDataEx(oldD^.i.Data)^.Color, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      CreateColorNode(n);
      InitDataEx(SettingsVST.AddChild(aNode), 'Style', dtInt32, @PModulFontDataEx(oldD^.i.Data)^.Style, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
      InitDataEx(SettingsVST.AddChild(aNode), 'Größe', dtInt32, @PModulFontDataEx(oldD^.i.Data)^.Size, @OnChangeSettings, NODE_DATA_UPDATE_PARENT);
    end;
  end;

begin
  SettingsVST.DeleteChildren(fModulNode);
  if not Assigned(aModul) then
    exit;
//Position und Größe
  n1 := SettingsVST.AddChild(fModulNode);
  InitData(n1, 'Größe');
//x-Position
  d := InitData(SettingsVST.AddChild(n1), 'x-Position');
  d^.i.DataType := dtInt32;
  d^.i.Data := @aModul.Position.x;
  d^.Event := @OnChangeModulSizePos;
//y-Position
  d := InitData(SettingsVST.AddChild(n1), 'y-Position');
  d^.i.DataType := dtInt32;
  d^.i.Data := @aModul.Position.y;
  d^.Event := @OnChangeModulSizePos;
//Breite
  d := InitData(SettingsVST.AddChild(n1), 'Breite');
  d^.i.DataType := dtInt32;
  d^.i.Data := @aModul.SmallSize.x;
  d^.Event := @OnChangeModulSizePos;
//Höhe
  d := InitData(SettingsVST.AddChild(n1), 'Höhe');
  d^.i.DataType := dtInt32;
  d^.i.Data := @aModul.SmallSize.y;
  d^.Event := @OnChangeModulSizePos;
//ModulSettings
  aModul.GetSettings;
  for i := 0 to aModul.SettingsCount-1 do begin
    settings := aModul.Settings[i];
    n1 := SettingsVST.AddChild(fModulNode);
    d := SettingsVST.GetNodeData(n1);
    d^.i := settings;
    d^.Event := @OnChangeSettings;
    if d^.i.DataType = dtColor then
      CreateColorNode(n1)
    else if d^.i.DataType = dtFont then
      CreateFontNode(n1);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.LoadDisplayInfo(const ID: Integer);
var
  info: TDisplayInfo;
begin
  if ID < 0 then
    exit;
  info := fHardwareMonitor.Displays[ID];
  DispNameLab.Caption    := info.Name;
  DispVersionLab.Caption := format('%d', [info.Version]);
  DispWidthLab.Caption   := format('%d px', [info.Width]);
  DispHeightLab.Caption  := format('%d px', [info.Height]);
  DispSerialLab.Caption  := info.SerialNumber;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.HardwareMonitorUpdate(aSender: TObject);
begin
  if Visible then
    PreviewPB.Repaint;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormCreate(Sender: TObject);
var
  n: PVirtualNode;
  d: PSettingsItemEx;
begin
  fFormatSettings.DecimalSeparator := '.';
  SettingsVST.EditDelay := 100;
  SettingsVST.NodeDataSize := SizeOf(TSettingsItemEx);

  fBgNode := SettingsVST.AddChild(nil);
  d := SettingsVST.GetNodeData(fBgNode);
  FillChar(d^, SizeOf(d^), 0);
  d^.i.Name := 'Hintergrund';
  fBgFilename := '';
  d^.i.DataType := dtPicture;
  d^.i.Data := @fBgFilename;
  d^.Event := @OnChangeBackground;

  n := SettingsVST.AddChild(nil);
  d := SettingsVST.GetNodeData(n);
  FillChar(d^, SizeOf(d^), 0);
  d^.i.Name := 'Update-Rate';
  fUpdateRate := 250;
  d^.i.DataType := dtInt32;
  d^.i.Data := @fUpdateRate;
  d^.Event := @OnChangeUpdateRate;

  fModulNode := SettingsVST.AddChild(nil);
  d := SettingsVST.GetNodeData(fModulNode);
  FillChar(d^, SizeOf(d^), 0);
  d^.i.Name := 'Modul';

////////////////////////////////////////////////////////////////////////////////
  fCurrentModul   := nil;
  fCurrentModulID := -1;
  fHardwareMonitor := THardwareMonitor.Create;
  try
    fHardwareMonitor.OnUpdate := @HardwareMonitorUpdate;
    fHardwareMonitor.Modulpath := ExtractFilePath(Application.ExeName)+'modules';
    fHardwareMonitor.Init;
    fHardwareMonitor.UpdateTime := fUpdateRate;
    UpdateDisplaysCLB;
    UpdateModulesLB;
    fHardwareMonitor.Resume;
  except
    on e: Exception do begin
      ShowMessage(format('Fehler beim Initialisieren des Prgramms: %s - %s',
        [e.ClassName, e.Message]));
      Application.Terminate;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.Button1Click(Sender: TObject);
begin
  ColorForm.ShowModal;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.DelModulBtClick(Sender: TObject);
begin
  if (ActiveModulesLB.ItemIndex >= 0) then begin
    if (fCurrentModulID = ActiveModulesLB.ItemIndex) then begin
      fCurrentModulID := -1;
      fCurrentModul   := nil;
    end;
    fHardwareMonitor.DisableModul(ActiveModulesLB.ItemIndex);
    UpdateActiveModulesLB;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.AddModulBtClick(Sender: TObject);
begin
  if (ModulesLB.ItemIndex >= 0) then begin
    fHardwareMonitor.EnableModul(ModulesLB.ItemIndex);
    UpdateActiveModulesLB;
    ActiveModulesLB.ItemIndex := fHardwareMonitor.ActiveModulCount-1;
    fCurrentModulID := ActiveModulesLB.ItemIndex;
    fCurrentModul   := fHardwareMonitor.ActiveModules[fCurrentModulID];
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ActiveModulesLBClick(Sender: TObject);
begin
  fCurrentModulID := ActiveModulesLB.ItemIndex;
  if fCurrentModulID >= 0 then begin
    fCurrentModul := fHardwareMonitor.ActiveModules[fCurrentModulID];
    BuildModulSettingsTree(fCurrentModul);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.ActiveModulesLBDblClick(Sender: TObject);
var
  s: String;
  i: Integer;
  obj: THardwareMonitorModul;
begin
  i := ActiveModulesLB.ItemIndex;
  if (i >= 0) then begin
    obj := fHardwareMonitor.ActiveModules[i];
    s := InputBox('Modulname', 'Bitte geben Sie den gewünschten Modulnamen ein:', obj.Name);
    if (s <> fHardwareMonitor.ActiveModules[i].Name) then begin
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
var
  i: Integer;
begin
  for i := 0 to DisplaysCLB.Count-1 do begin
    if (i <> DisplaysCLB.ItemIndex) then
      DisplaysCLB.Checked[i] := false;
  end;
  if (DisplaysCLB.Checked[DisplaysCLB.ItemIndex]) then
    fHardwareMonitor.ActivateDisplay(DisplaysCLB.ItemIndex)
  else
    fHardwareMonitor.ActivateDisplay(-1);
  PreviewPB.Repaint;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fHardwareMonitor.Suspend;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fHardwareMonitor.Free;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function Move(const p: TPoint; const x, y: Integer): TPoint;
  begin
    result := Point(p.X + x, p.Y + y);
    Key := 0;
  end;

begin
  if Assigned(fCurrentModul) and not fIsEditing then begin
    if (ssShift in Shift) then begin
      case Key of
        VK_UP   : fCurrentModul.SmallSize := Move(fCurrentModul.SmallSize,  0, -1);
        VK_DOWN : fCurrentModul.SmallSize := Move(fCurrentModul.SmallSize,  0,  1);
        VK_LEFT : fCurrentModul.SmallSize := Move(fCurrentModul.SmallSize, -1,  0);
        VK_RIGHT: fCurrentModul.SmallSize := Move(fCurrentModul.SmallSize,  1,  0);
      end;
    end else begin
      case Key of
        VK_UP   : fCurrentModul.Position := Move(fCurrentModul.Position,  0, -1);
        VK_DOWN : fCurrentModul.Position := Move(fCurrentModul.Position,  0,  1);
        VK_LEFT : fCurrentModul.Position := Move(fCurrentModul.Position, -1,  0);
        VK_RIGHT: fCurrentModul.Position := Move(fCurrentModul.Position,  1,  0);
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
  if OpenDialog.Execute then begin
    mcf := TmcfFile.Create;
    try
      BuildModulSettingsTree(nil);
      mcf.LoadFromFile(OpenDialog.FileName);
      fHardwareMonitor.LoadFromFile(mcf);
      fBgFilename := mcf.GetString('Background', '');
      if FileExists(fBgFilename) then
        OnChangeBackground(self);
      fUpdateRate := fHardwareMonitor.UpdateTime;
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
  fHardwareMonitor.EmulateTouch(fMouseDownPos);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PreviewPBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fCurrentModul) then begin
    fMouseDown := IsOnModul(fCurrentModul, X, Y);
    fMouseDownPos := Point(X, Y);
    fOldModulPos  := fCurrentModul.Position;
    fOldModulSize := fCurrentModul.SmallSize;
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
  if not Assigned(fCurrentModul) then
    exit;

  if fMouseDown = 0 then begin
    PreviewPB.Cursor := MODUL_CURSER[IsOnModul(fCurrentModul, X, Y)];
    exit;
  end;

  dX := (X - fMouseDownPos.X);
  dY := (Y - fMouseDownPos.Y);
  case fMouseDown of
    1: begin
      fCurrentModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y + dY);
      fCurrentModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y - dY);
    end;
    2: begin
      fCurrentModul.Position  := P(fOldModulPos.X, fOldModulPos.Y + dY);
      fCurrentModul.SmallSize := P(fOldModulSize.X, fOldModulSize.Y - dY);
    end;
    3: begin
      fCurrentModul.Position  := P(fOldModulPos.X, fOldModulPos.Y + dY);
      fCurrentModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y - dY);
    end;
    4: begin
      fCurrentModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y);
    end;
    5: begin
      fCurrentModul.SmallSize := P(fOldModulSize.X + dX, fOldModulSize.Y + dY);
    end;
    6: begin
      fCurrentModul.SmallSize := P(fOldModulSize.X, fOldModulSize.Y + dY);
    end;
    7: begin
      fCurrentModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y);
      fCurrentModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y + dY);
    end;
    8: begin
      fCurrentModul.Position  := P(fOldModulPos.X + dX, fOldModulPos.Y);
      fCurrentModul.SmallSize := P(fOldModulSize.X - dX, fOldModulSize.Y);
    end;
    9: begin
      fCurrentModul.Position := P(fOldModulPos.X + dX, fOldModulPos.Y + dY)
    end;
  end;
  SaveModulData(fCurrentModulID);
  RepaintNodes(fModulNode);
  fHardwareMonitor.Resume;
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
  with PreviewPB.Canvas do begin
    Rectangle(-5, -5, PreviewPB.Width+5, PreviewPB.Height+5);
    with fHardwareMonitor do begin
      LockFrameBuffer;
      try
        StretchDraw(Classes.Rect(0, 0,
          fHardwareMonitor.FrameBuffer.Width,
          fHardwareMonitor.FrameBuffer.Height),
          fHardwareMonitor.FrameBuffer);
      finally
        UnlockFrameBuffer;
      end;
    end;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Brush.Style := bsClear;
    if not Assigned(fHardwareMonitor.LargeModul) then begin
      for i := 0 to fHardwareMonitor.ActiveModulCount-1 do begin
        obj := fHardwareMonitor.ActiveModules[i];
        DrawRect;
      end;
      Pen.Color := clRed;
      obj := fCurrentModul;
      DrawRect;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SaveSettingsBtClick(Sender: TObject);
var
  mcf: TmcfFile;
begin
  mcf := TmcfFile.Create;
  try
    if SaveDialog.Execute then begin
      mcf.SetString('Background', fBgFilename);
      fHardwareMonitor.SaveToFile(mcf);
      mcf.SaveToFile(SaveDialog.FileName);
    end;
  finally
    mcf.Free;
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTDblClick(Sender: TObject);
var
  n: PVirtualNode;
  d: PSettingsItemEx;
begin
  n := SettingsVST.FocusedNode;
  if Assigned(n) then begin
    d := SettingsVST.GetNodeData(n);
    if Assigned(d) and Assigned(d^.i.Data) then begin
      case d^.i.DataType of
        dtFont: begin
          FontDialog.Font.Style := [];
          if ((PModulFontDataEx(d^.i.Data)^.Style and Integer(FontStyleBold)) > 0) then
            FontDialog.Font.Style := FontDialog.Font.Style + [fsBold];
          if ((PModulFontDataEx(d^.i.Data)^.Style and Integer(FontStyleItalic)) > 0) then
            FontDialog.Font.Style := FontDialog.Font.Style + [fsItalic];
          if ((PModulFontDataEx(d^.i.Data)^.Style and Integer(FontStyleStrikeout)) > 0) then
            FontDialog.Font.Style := FontDialog.Font.Style + [fsStrikeOut];
          if ((PModulFontDataEx(d^.i.Data)^.Style and Integer(FontStyleUnderline)) > 0) then
            FontDialog.Font.Style := FontDialog.Font.Style + [fsUnderline];
          FontDialog.Font.Name  := PModulFontDataEx(d^.i.Data)^.Name;
          FontDialog.Font.Size  := PModulFontDataEx(d^.i.Data)^.Size;
          FontDialog.Font.Color :=
              ((PModulFontDataEx(d^.i.Data)^.Color shr  16) and $FF) or
              ((PModulFontDataEx(d^.i.Data)^.Color and $FF) shl  16) or
              ( PModulFontDataEx(d^.i.Data)^.Color and $FF00);
          if FontDialog.Execute then begin
            PModulFontDataEx(d^.i.Data)^.Name  := FontDialog.Font.Name;
            PModulFontDataEx(d^.i.Data)^.Style := 0;
            if fsBold in FontDialog.Font.Style then
              PModulFontDataEx(d^.i.Data)^.Style := PModulFontDataEx(d^.i.Data)^.Style or Integer(FontStyleBold);
            if fsItalic in FontDialog.Font.Style then
              PModulFontDataEx(d^.i.Data)^.Style := PModulFontDataEx(d^.i.Data)^.Style or Integer(FontStyleItalic);
            if fsStrikeOut in FontDialog.Font.Style then
              PModulFontDataEx(d^.i.Data)^.Style := PModulFontDataEx(d^.i.Data)^.Style or Integer(FontStyleStrikeout);
            if fsUnderline in FontDialog.Font.Style then
              PModulFontDataEx(d^.i.Data)^.Style := PModulFontDataEx(d^.i.Data)^.Style or Integer(FontStyleUnderline);
            PModulFontDataEx(d^.i.Data)^.Size := FontDialog.Font.Size;
            PModulFontDataEx(d^.i.Data)^.Color :=
              (PModulFontDataEx(d^.i.Data)^.Color and $FF000000) or
              ((FontDialog.Font.Color shr  16) and $FF) or
              ((FontDialog.Font.Color and $FF) shl  16) or
              ( FontDialog.Font.Color and $FF00);
            if Assigned(d^.Event) then
              d^.Event(self);
          end;
        end;
        dtFile: begin
          if OpenDialog.Execute then begin
            PString(d^.i.Data)^ := OpenDialog.FileName;
            if Assigned(d^.Event) then
              d^.Event(self);
          end;
        end;
        dtPicture: begin
          if OpenPicDialog.Execute then begin
            PString(d^.i.Data)^ := OpenPicDialog.FileName;
            if Assigned(d^.Event) then
              d^.Event(self);
          end;
        end;
        dtColor: begin
          if (ColorForm.ShowModalColor(PCardinal(d^.i.Data)^) = mrOK) then begin
            PCardinal(d^.i.Data)^ := ColorForm.Color.c;
            if Assigned(d^.Event) then
              d^.Event(self);
          end;
        end;
        dtBool: begin
          PBoolean(d^.i.Data)^ := not PBoolean(d^.i.Data)^;
          if Assigned(d^.Event) then
            d^.Event(self);
        end
      else
        if n^.ChildCount <= 0 then
          SettingsVST.EditNode(n, 1);
      end;
      case d^.Tag of
        NODE_DATA_UPDATE_PARENT:
          SettingsVST.RepaintNode(n^.Parent);
        NODE_DATA_UPDATE_CHILDREN:
          RepaintNodes(n);
      end;
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
  d: PSettingsItemEx;
begin
  d := SettingsVST.GetNodeData(Node);
  Allowed := Assigned(Node) and (SettingsVST.ChildCount[Node] <= 0) and
    Assigned(d) and Assigned(d^.i.Data) and (Column = 1);
  fIsEditing := Allowed;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  d: PSettingsItemEx;
begin
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) then begin
    d^.i.Name := '';
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  d: PSettingsItemEx;
begin
  CellText := '';
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) then begin
    case Column of
      0: CellText := d^.i.Name;
      1: begin
        case d^.i.DataType of
          dtFile, dtPicture: if Assigned(d^.i.Data) and (PString(d^.i.Data)^ <> '') then
              CellText := MiniMizeName(PString(d^.i.Data)^, SettingsVST.Canvas, SettingsVST.Header.Columns[1].Width)
            else
              CellText := '[leer]';
          dtString: if Assigned(d^.i.Data) then
              CellText := PString(d^.i.Data)^
            else
              CellText := '[leer]';
          dtBool: if Assigned(d^.i.Data) and PBoolean(d^.i.Data)^ then
              CellText := 'true'
            else
              CellText := 'false';
          dtInt32: if Assigned(d^.i.Data) then
            CellText := IntToStr(PInteger(d^.i.Data)^);
          dtFloat32: if Assigned(d^.i.Data) then
            CellText := ToStr(PSingle(d^.i.Data)^, -3);
          dtColor: if Assigned(d^.i.Data) then
            CellText := IntToHex(PCardinal(d^.i.Data)^, 8);
          dtByte: if Assigned(d^.i.Data) then
            CellText := IntToStr(PByte(d^.i.Data)^);
        else
          CellText := '';
        end;
      end
    else
      CellText := '';
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.SettingsVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  d: PSettingsItemEx;
  i: Integer;
begin
  d := SettingsVST.GetNodeData(Node);
  if Assigned(d) and Assigned(d^.i.Data) then begin
    case d^.i.DataType of
      dtInt32: begin
        if not TryStrToInt(NewText, PInteger(d^.i.Data)^) then
          MessageDlg('Error', Format('''%s'' ist keine gültige Zahl!', [NewText]), mtError, [mbOK], 0)
        else begin
          if (d^.i.Min <> 0) or (d^.i.Max <> 0) then begin
            if (i < d^.i.Min) then
              i := Trunc(d^.i.Min);
            if (i > d^.i.Max) then
              i := Trunc(d^.i.Max);
          end;
          if Assigned(d^.Event) then
            d^.Event(self);
        end;
      end;
      dtByte: begin
        if not TryStrToInt(NewText, i) then
          MessageDlg('Error', Format('''%s'' ist keine gültige Zahl!', [NewText]), mtError, [mbOK], 0)
        else begin
          if (i < 0) then
            i := 0;
          if (i > 255) then
            i := 255;
          if (d^.i.Min <> 0) or (d^.i.Max <> 0) then begin
            if (i < d^.i.Min) then
              i := Trunc(d^.i.Min);
            if (i > d^.i.Max) then
              i := Trunc(d^.i.Max);
          end;
          PByte(d^.i.Data)^ := i;
          if Assigned(d^.Event) then
            d^.Event(self);
        end;
      end;
      dtFloat32: begin
        if not TryStrToFloat(StringReplace(NewText, ',', '.', [rfIgnoreCase, rfReplaceAll]), PSingle(d^.i.Data)^, fFormatSettings) then
          MessageDlg('Error', Format('''%s'' ist keine gültige Fließkommazahl!', [NewText]), mtError, [mbOK], 0)
        else if Assigned(d^.Event) then
          d^.Event(self);
      end;
      dtString: begin
        PString(d^.i.Data)^ := NewText;
        if Assigned(d^.Event) then
          d^.Event(self);
      end;
    end;

    case d^.Tag of
      NODE_DATA_UPDATE_CHILDREN:
        RepaintNodes(Node);
      NODE_DATA_UPDATE_PARENT:
        SettingsVST.RepaintNode(Node^.Parent);
    end;
  end;
end;

end.

