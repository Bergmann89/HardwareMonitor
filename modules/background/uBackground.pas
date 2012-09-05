unit uBackground;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, windows, uUSBDisplay,
  GDIPAPI, GDIPOBJ, GDIPUTIL;

type

  { TBackground }

  TBackground = class(TBasicModul)
  private
    fSize: TPoint;
    fHandle: HDC;
    fBitmap: HBITMAP;
    fBgColor: Cardinal;

    fGraphic: TGPGraphics;

    procedure CreateBitmapHandle;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); override;
    function Draw: TDrawResult; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TBackground.CreateBitmapHandle;
begin
  fBitmap := CreateBitmap(fSize.x, fSize.y, 1, 32, nil);
  SelectObject(fHandle, fBitmap);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TBackground.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
begin
  if Assigned(fGraphic) then
    FreeAndNil(fGraphic);
  fSize := Classes.Point(aSmallW, aSmallH);
  CreateBitmapHandle;
  fGraphic := TGPGraphics.Create(fHandle);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TBackground.Draw: TDrawResult;
begin
  with result do begin
    Handle      := fHandle;
    Width       := 1;
    Height      := 1;
    Transparent := true;
  end;
  fGraphic.Clear(fBgColor);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TBackground.Create;
begin
  inherited Create;
  fHandle := CreateCompatibleDC(GetDC(0));
  fBgColor := $60000000;
  fSize := Classes.Point(1, 1);
  CreateBitmapHandle;

  fGraphic := TGPGraphics.Create(fHandle);

  fSettingItems.Add(CreateSettingsItem(
    'background_color', 'Hintergrundfarbe', SETTINGS_FLAG_COLOR, dtInt32, @fBgColor));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TBackground.Destroy;
begin
  FreeAndNil(fGraphic);
  DeleteObject(fBitmap);
  DeleteDC(fHandle);
  inherited Destroy;
end;

end.

