unit uUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, windows;

type
  TRGB565Arr = packed array of Word;

function ToRGB565(const r, g, b: Byte): Word;
function ToRGB565f(const r, g, b: Single): Word;
procedure BitmapTo565(const aBitmap: Graphics.TBitmap; var aBuffer: TRGB565Arr);
function ToStr(const f: Single; const Digits: Integer = -3): String;
function TryHexToInt(const aHex: String; out int: Cardinal): Boolean;

implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function ToRGB565(const r, g, b: Byte): Word;
begin
  result :=
    (((r * $1F div $FF) and $1F) shl 11) or
    (((g * $3F div $FF) and $3F) shl  5) or
    (((b * $1F div $FF) and $1F)       );
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function ToRGB565f(const r, g, b: Single): Word;
begin
  result := ToRGB565(Round(r), Round(g), Round(b));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure BitmapTo565(const aBitmap: Graphics.TBitmap; var aBuffer: TRGB565Arr);
var
  i: integer;
  bits: Pointer;
  s: PRGBTRIPLE;
  bInfo: BITMAPINFO;
  d: PWord;
begin
  SetLength(aBuffer, aBitmap.Height * aBitmap.Width);

  FillChar(bInfo{%H-}, SizeOf(bInfo), 0);
  with bInfo.bmiHeader do begin
    biSize:= SizeOf(bInfo.bmiHeader);
    biWidth:= aBitmap.Width;
    biHeight:= -aBitmap.Height;
    biPlanes:= 1;
    biBitCount:= 24;
    biCompression:= BI_RGB;
  end;
  bits := GetMem(aBitmap.Width * aBitmap.Height * 3);
  try
    if GetDIBits(aBitmap.Canvas.Handle, aBitmap.Handle, 0, aBitmap.Height, bits, bInfo, DIB_RGB_COLORS) = 0 then
      raise Exception.Create(format('unable to read bitmap data. error code: %d', [GetLastError]));
    d := @aBuffer[0];
    s := bits;
    for i := 0 to High(aBuffer) do begin
      d^ := ToRGB565(s^.rgbtRed, s^.rgbtGreen, s^.rgbtBlue);
      inc(d);
      inc(s);
    end;
  finally
    FreeMem(bits);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//wandelt einen Single in einen String um
//@f: Single der umgewandelt werden soll;
//@Digits: Nachkommastellen;
//@result: String mit den SingleDaten;
function ToStr(const f: Single; const Digits: Integer = -3): String;
var
  format: TFormatSettings;
  p: Integer;
begin
  format.DecimalSeparator := '.';
  result := FloatToStrF(f, ffFixed, 12, -Digits, format);
  if Digits < 0 then begin
    p := pos('.', result);
    if (p <= 0) then begin
      result := result + '.0';
      p := Length(result) - 1;
    end;
    while (Length(result) - p) < -Digits do
      result := result + '0';

    Delete(result, p-Digits+1, Length(result));
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TryHexToInt(const aHex: String; out int: Cardinal): Boolean;
var
  i, v: Integer;
  s: String;
begin
  result := false;
  int := 0;
  s := LowerCase(aHex);
  for i := 1 to Length(s) do begin
    case s[i] of
      '0'..'9':
        v := Ord(aHex[i]) - $30;
      'a':
        v := 10;
      'b':
        v := 11;
      'c':
        v := 12;
      'd':
        v := 13;
      'e':
        v := 14;
      'f':
        v := 15;
      else
        int := 0;
        exit;
    end;
    int := int + Round(v * Power(16, Length(s)-i));
  end;
  result := true;
end;

end.

