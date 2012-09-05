unit uText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uModulAPI, windows,
  GDIPOBJ, GDIPUTIL, GDIPAPI;

type
  TText = class(TBasicModul)
  private
    fHandle: HDC;
    fBitmap: HBITMAP;
    fGraphic: TGPGraphics;
    fBrush: TGPSolidBrush;
    fFont: TGPFont;

    fSize: TPoint;
    fBgColor: Cardinal;
    fText: String;
    fFontData: TModulFontData;

    procedure CreateFont;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); override;
    procedure Update; override;
    function Draw: TDrawResult; override;
    procedure SetSettings(const aData: PSettingsItemRec); override;

    constructor Create;
    destructor Destroy; override;
  end;


implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TText.CreateFont;
begin
  if Assigned(fFont) then
    fFont.Free;
  fFont := TGPFont.Create(fFontData.Name, fFontData.Size, fFontData.Style);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TText.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
begin
  fSize := Classes.Point(aSmallW, aSmallH);
  if Assigned(fGraphic) then
    fGraphic.Free;
  DeleteObject(fBitmap);
  fBitmap := CreateBitmap(fSize.x, fSize.y, 1, 32, nil);
  SelectObject(fHandle, fBitmap);
  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TText.Update;
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TText.Draw: TDrawResult;
var
  s: WideString;
begin
  with result do begin
    Handle      := fHandle;
    Width       := fSize.x;
    Height      := fSize.y;
    Transparent := true;
  end;

  s := UTF8Decode(fText);
  fGraphic.Clear(fBgColor);
  fBrush.SetColor(fFontData.Color);
  fGraphic.DrawString(s, Length(s), fFont, MakePoint(0.0, 0.0), fBrush);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TText.SetSettings(const aData: PSettingsItemRec);
begin
  inherited SetSettings(aData);
  CreateFont;
  fText := StringReplace(fText, '%nl', sLineBreak, [rfIgnoreCase, rfReplaceAll]);
end;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TText.Create;
begin
  inherited Create;

  fFontData.Name  := 'Arial';
  fFontData.Size  := 12;
  fFontData.Color := $FEFFFFFF;
  fFontData.Style := FontStyleRegular;
  fBgColor := $60000000;
  fText := 'Text';

  CreateFontSettingItems(fSettingItems, 'font', 'Font', @fFontData);
  fSettingItems.Add(CreateSettingsItem(
    'background_color', 'Hintergrundfarbe', SETTINGS_FLAG_NONE, dtInt32, @fBgColor));
  fSettingItems.Add(CreateSettingsItem(
    'text', 'Text', SETTINGS_FLAG_NONE, dtString, @fText));

  fHandle := CreateCompatibleDC(GetDC(0));
  fBitmap := CreateBitmap(1, 1, 1, 32, nil);
  SelectObject(fHandle, fBitmap);

  fBrush := TGPSolidBrush.Create(0);
  fGraphic := TGPGraphics.Create(fHandle);
  fGraphic.SetTextRenderingHint(TextRenderingHintAntiAlias);

  CreateFont;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TText.Destroy;
begin
  if Assigned(fFont) then
    FreeAndNil(fFont);
  FreeAndNil(fBrush);
  FreeAndNil(fGraphic);
  DeleteObject(fBitmap);
  DeleteDC(fHandle);

  inherited Destroy;
end;

end.

