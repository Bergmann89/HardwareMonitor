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

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TColorForm, ColorForm);
  Application.CreateForm(TSensorForm, SensorForm);
  Application.Run;
end.

