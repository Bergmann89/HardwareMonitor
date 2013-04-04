program HardwareMonitor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, virtualtreeview_package, uMainForm, uUtils, uUSBD480_API, uUSBDisplay,
  uHardwareMonitor, uModulAPI, uColorForm, uSensorForm;

{$R *.res}

var
  i: Integer;
const
  SILENT_PARAM = '-silent';

begin
  Application.Initialize;
  for i := 1 to ParamCount do
    if ParamStr(i) = SILENT_PARAM then
      Application.ShowMainForm := false;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TColorForm, ColorForm);
  Application.CreateForm(TSensorForm, SensorForm);
  Application.Run;
end.

