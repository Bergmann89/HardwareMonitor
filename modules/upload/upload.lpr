library upload;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Classes, uModulAPI, SysUtils, GDIPOBJ, uUSBDisplay, uUpload
  { you can add units after this };

{$R *.res}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function ModulInfo: TModulInfo; stdcall;
begin
  result.Name    := 'Netzwerk Upload';
  result.Autor   := 'Bergmann89';
  result.eMail   := 'info@bergmann89.de';
  result.Version := '1.0.0';
  result.Year    := 2012;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function CreateModul: Pointer; stdcall;
begin
  try
    result := TUpload.Create;
  except
    result := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure FreeModul(aHandle: Pointer); stdcall;
begin
  try
    if Assigned(aHandle) then
      FreeAndNil(aHandle);
  except
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure Resize(aHandle: Pointer; aSmall: Boolean; aSmallW, aSmallH, aLargeW, aLargeH: Integer); stdcall;
begin
  try
    if Assigned(aHandle) then
      TBasicModul(aHandle).Resize(aSmall, aSmallW, aSmallH, aLargeW, aLargeH);
  except
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure Update(aHandle: Pointer); stdcall;
begin
  try
    if Assigned(aHandle) then
      TBasicModul(aHandle).Update;
  except
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function Draw(aHandle: Pointer): TDrawResult; stdcall;
begin
  FillChar(result, SizeOf(result), 0);
  try
    if Assigned(aHandle) then
      result := TBasicModul(aHandle).Draw;
  except
    FillChar(result, SizeOf(result), 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure GetSettings(aHandle: Pointer; out aCount: Integer; out aData: PSettingsItem); stdcall;
begin
  aData := nil;
  aCount := 0;
  try
    if Assigned(aHandle) then
      TBasicModul(aHandle).GetSettings(aCount, aData);
  except
    aData := nil;
    aCount := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure SetSettings(aHandle: Pointer; aData: PSettingsItem); stdcall;
begin
  try
    if Assigned(aHandle) then
      TBasicModul(aHandle).SetSettings(aData);
  except
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function SendTouchReport(aHandle: Pointer; aPoint: TPoint; aPressure: Byte; aMode: TTouchMode): Boolean; stdcall;
begin
  result := false;
  try
    if Assigned(aHandle) then
      result := TBasicModul(aHandle).SendTouchReport(aPoint, aPressure, aMode);
  except
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure InitLibData; stdcall;
begin
  InitGDIPlus;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure FreeLibData; stdcall;
begin
  FreeGDIPlus;
end;

exports
  ModulInfo       name 'ModulInfo',
  CreateModul     name 'CreateModul',
  FreeModul       name 'FreeModul',
  Resize          name 'Resize',
  Update          name 'Update',
  Draw            name 'Draw',
  InitLibData     name 'InitLibData',
  FreeLibData     name 'FreeLibData',
  GetSettings     name 'GetSettings',
  SetSettings     name 'SetSettings',
  SendTouchReport name 'SendTouchReport';

begin
end.

