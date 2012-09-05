unit uClock;

{$mode objfpc}{$H+}

interface

uses
  Interfaces, Classes, SysUtils, Windows, uModulAPI, Graphics, uUSBDisplay,
  GDIPOBJ, GDIPAPI;

type
  TClock = class(TBasicModul)
  private
    g: TGPGraphics;
    b: TGPSolidBrush;

    fLargeFont: TGPFont;
    fSecFont: TGPFont;
    fDatFont: TGPFont;
    fLargeFontMetric: TTEXTMETRIC;
    fSecFontMetric: TTEXTMETRIC;
    fDatFontMetric: TTEXTMETRIC;

    fMinSize: TGPRectF;
    fFirstResize: Boolean;
//Settings
    fDatFormat: String;
    fSecFontSize: Single;
    fDatFontSize: Single;
    fFontData: TModulFontData;
    fBgColor: Cardinal;
    fIsKeyDown: Boolean;
    fIsSmall: Boolean;

    Size: TPoint;
    Handle: HDC;
    Bitmap: HBITMAP;
    Brush: HBRUSH;

    function CreateFont(const aHeight: Single): TGPFont;
    function CalcMinModulSize: TGPRectF;
  public
    procedure Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer); override;
    procedure Update; override;
    function Draw: TDrawResult; override;
    procedure SetSettings(const aData: PSettingsItemRec); override;
    function SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TClock.CreateFont(const aHeight: Single): TGPFont;
begin
  result := TGPFont.Create(fFontData.Name, aHeight, fFontData.Style);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TClock.CalcMinModulSize: TGPRectF;
var
  t: TDateTime;
  s: WideString;
  rec: TGPRectF;
begin
  if not Assigned(fLargeFont) then
    exit;

  t := now;
  s := UTF8Decode(FormatDateTime('hh:mm', t));
  g.MeasureString(s, Length(s), fLargeFont, MakePoint(0.0, 0.0), rec);
  result := rec;
  result.Height := fLargeFont.GetHeight(g);

  if fSecFontSize > 0.0 then begin
    s := UTF8Decode(FormatDateTime(':ss', t));
    g.MeasureString(s, Length(s), fSecFont, MakePoint(0.0, 0.0), rec);
    result.Width := result.Width + rec.Width - (fSecFontMetric.tmInternalLeading + fSecFontMetric.tmExternalLeading);
  end;

  if fDatFontSize > 0.0 then begin
    s := UTF8Decode(FormatDateTime(fDatFormat, t));
    g.MeasureString(s, Length(s), fDatFont, MakePoint(0.0, 0.0), rec);
    result.Width := max(result.Width, rec.Width);
    result.Height := result.Height + rec.Height - fDatFontMetric.tmExternalLeading - fDatFontMetric.tmInternalLeading;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TClock.Resize(const aSmall: Boolean; const aSmallW, aSmallH, aLargeW, aLargeH: Integer);
var
  h, oldSize, oldHeight: Single;
begin
  fIsSmall := aSmall;
  if fIsSmall then
    Size := Classes.Point(aSmallW, aSmallH)
  else
    Size := Classes.Point(aLargeW, aLargeH);

  if Assigned(g) then
    g.Free;
  DeleteObject(Bitmap);
  Bitmap := CreateBitmap(Size.x, Size.y, 1, 32, nil);
  SelectObject(Handle, Bitmap);
  g := TGPGraphics.Create(Handle);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  fMinSize := CalcMinModulSize;
  if Assigned(fLargeFont) then begin
    oldHeight := fLargeFont.GetHeight(g);
    oldSize   := fLargeFont.GetSize;
    fLargeFont.Free;
  end;
  if Assigned(fSecFont) then
    fSecFont.Free;
  if Assigned(fDatFont) then
    fDatFont.Free;
  if (fMinSize.Width/fMinSize.Height < Size.x/Size.y) then
    h := oldSize * Size.y / oldHeight
  else
    h := oldSize * Size.x / fMinSize.Width * fMinSize.Height / oldHeight;
  h := h / (1 + fDatFontSize);
  fLargeFont := CreateFont(h);
  fSecFont   := CreateFont(h*fSecFontSize);
  fDatFont   := CreateFont(h*fDatFontSize);

  fLargeFontMetric := GetTextMetric(Handle, fLargeFont, g);
  fSecFontMetric := GetTextMetric(Handle, fSecFont, g);
  fDatFontMetric := GetTextMetric(Handle, fDatFont, g);

  if fFirstResize then begin
    fFirstResize := false;
    Resize(aSmall, aSmallW, aSmallH, aLargeW, aLargeH);
  end;
  fMinSize := CalcMinModulSize;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TClock.Update;
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TClock.Draw: TDrawResult;
var
  s: WideString;
  rec, r2: TGPRectF;
  t: TDateTime;
  lWidth, y: Single;
begin
  result.Width  := Size.x;
  result.Height := Size.y;
  result.Handle := Handle;
  result.Transparent := true;

  g.Clear(fBgColor);
//Uhrzeit
  t := now;
  y := (Size.y - fMinSize.Height)/2;

  lWidth := 0;
  if fSecFontSize > 0 then begin
    s := UTF8Decode(FormatDateTime(':ss', t));
    g.MeasureString(s, Length(s), fSecFont, MakePoint(0.0, 0.0), rec);
    lWidth := rec.Width - fLargeFontMetric.tmInternalLeading - fSecFontMetric.tmInternalLeading;
  end;
  s := UTF8Decode(FormatDateTime('hh:mm', t));
  g.MeasureString(s, Length(s), fLargeFont, MakePoint(0.0, 0.0), rec);
  lWidth := lWidth + rec.Width;

  //hh:mm
  s := UTF8Decode(FormatDateTime('hh:mm', t));
  b.SetColor(fFontData.Color);
  g.MeasureString(s, Length(s), fLargeFont, MakePoint(0.0, 0.0), rec);
  g.DrawString(s, Length(s), fLargeFont, MakePoint((Size.x - lWidth)/2, y), b);

  //:ss
  if fSecFontSize > 0.0 then begin
    s := UTF8Decode(FormatDateTime(':ss', t));
    g.DrawString(s, Length(s), fSecFont, MakePoint(
    {x} (Size.x - lWidth)/2 + rec.Width - fLargeFontMetric.tmInternalLeading - fSecFontMetric.tmInternalLeading -
          fSecFontMetric.tmExternalLeading,
    {y} y + fLargeFontMetric.tmAscent - fSecFontMetric.tmAscent), b);
  end;

  //date:
  if fDatFontSize > 0.0 then begin
    s := UTF8Decode(FormatDateTime(fDatFormat, t));
    g.MeasureString(s, Length(s), fDatFont, MakePoint(0.0, 0.0), r2);
    g.DrawString(s, Length(s), fDatFont, MakePoint(
      (Size.x - r2.Width)/2,
      y + rec.Height - fLargeFontMetric.tmExternalLeading - fLargeFontMetric.tmInternalLeading - fDatFontMetric.tmInternalLeading), b);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TClock.SetSettings(const aData: PSettingsItemRec);
begin
  inherited SetSettings(aData);
  Resize(true, Size.x, Size.y, 0, 0);
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TClock.SendTouchReport(const aPoint: TPoint; const aPressure: Byte; const aMode: TTouchMode): Boolean;
begin
  result := inherited SendTouchReport(aPoint, aPressure, aMode);
  if (aMode = tmPenDown) then
    fIsKeyDown := true;
  if (aMode = tmPenUp) and fIsKeyDown then begin
    result := true;
    fIsKeyDown := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TClock.Create;
begin
  inherited Create;

//Settings
  fDatFormat   := 'ddd dd. mmm yyyy';
  fSecFontSize := 0.75;
  fDatFontSize := 0.4;
  fBgColor     := $60000000;
  fFontData.Name  := 'Arial';
  fFontData.Style := 0;
  fFontData.Color := $FFFFFFFF;
  fIsKeyDown := false;
  fIsSmall := true;

  fSettingItems.Add(CreateSettingsItem(
    'date_format', 'Datumsformat', SETTINGS_FLAG_NONE, dtString, @fDatFormat));
  fSettingItems.Add(CreateSettingsItem(
    'sec_size', 'Größe der Sec. Anzeige', SETTINGS_FLAG_NONE, dtFloat32, @fSecFontSize));
  fSettingItems.Add(CreateSettingsItem(
    'dat_size', 'Größe der Datum Anzeige', SETTINGS_FLAG_NONE, dtFloat32, @fDatFontSize));
  fSettingItems.Add(CreateSettingsItem(
    'background_color', 'Hintergrundfarbe', SETTINGS_FLAG_COLOR, dtInt32, @fBgColor));
  CreateFontSettingItems(fSettingItems, 'font', 'Font', @fFontData);

  Handle := CreateCompatibleDC(GetDC(0));
  Brush  := CreateSolidBrush($60000000);
  SelectObject(Handle, Brush);
  Bitmap := CreateBitmap(1, 1, 1, 32, nil);
  SelectObject(Handle, Bitmap);

  fFirstResize := true;
  fLargeFont   := CreateFont(20);
  fSecFont     := CreateFont(20);
  fDatFont     := CreateFont(20);

  b := TGPSolidBrush.Create(0);
  g := TGPGraphics.Create(Handle);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TClock.Destroy;
begin
  fLargeFont.Free;
  fSecFont.Free;
  fDatFont.Free;
  b.Free;
  if Assigned(g) then
    g.Free;

  DeleteObject(Brush);
  DeleteObject(Bitmap);
  DeleteDC(Handle);
  inherited Destroy;
end;

end.

