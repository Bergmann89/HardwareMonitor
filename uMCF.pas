unit uMCF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TmcfValue = class(TObject)
  private
    fValue: String;
  public
    property Value : String read fValue write fValue;

    procedure Assign(const aValue: TmcfValue);

    function GetInt(const aDefault: Int64 = 0): Int64;
    function GetFloat(const aDefault: Double = 0): Double;
    function GetBool(const aDefault: Boolean = false): Boolean;
    function GetString: String;

    procedure SetInt(const aValue: Int64);
    procedure SetFloat(const aValue: Double);
    procedure SetBool(const aValue: Boolean);
    procedure SetString(const aValue: String);
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TmcfListAssignOp = (aoCopy, aoAdd);
  TmcfAttributes = class(TObject)
  private
    fList: TStringList;
  protected
    function Get(const aIndex: Integer): TmcfValue; overload;
    function Get(const aName: String): TmcfValue; overload;
    function GetName(const aIndex: Integer): String;
    procedure Put(const aIndex: Integer; const aValue: TmcfValue); overload;
    procedure Put(const aName: String; const aValue: TmcfValue); overload;
    procedure PutName(const aIndex: Integer; const aValue: String);
    function GetCount: Integer;
  public
    property ValueByIndex[const aIndex: Integer]: TmcfValue read Get write Put; default;
    property ValueByName[const aName: String]: TmcfValue read Get write Put;
    property Names[const aIndex: Integer]: String read GetName write PutName;
    property Count: Integer read GetCount;

    procedure Assign(const aValue: TmcfAttributes; const aOperator: TmcfListAssignOp = aoCopy);
    function Add(const aName: String): TmcfValue;
    function Insert(const aIndex: Integer; const aName: String): TmcfValue;
    procedure Delete(const aIndex: Integer);
    function Remove(const aName: String): Integer;
    function IndexOf(const aValue: TmcfValue): Integer; overload;
    function IndexOf(const aName: String): Integer; overload;
    procedure Clear;

    function Exists(const aName: String): Boolean;
    procedure List(const aStrings: TStrings; const aListValues: Boolean = false; aPrefix: String = '');

    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TmcfSection = class;

  TmcfSections = class(TObject)
  private
    fList: TStringList;
  protected
    function Get(const aIndex: Integer): TmcfSection; overload;
    function Get(const aName: String): TmcfSection; overload;
    function GetName(const aIndex: Integer): String;
    procedure Put(const aIndex: Integer; const aValue: TmcfSection); overload;
    procedure Put(const aName: String; const aValue: TmcfSection); overload;
    procedure PutName(const aIndex: Integer; const aValue: String);
    function GetCount: Integer;
  public
    property SectionByIndex[const aIndex: Integer]: TmcfSection read Get write Put; default;
    property SectionByName[const aName: String]: TmcfSection read Get write Put;
    property Names[const aIndex: Integer]: String read GetName write PutName;
    property Count: Integer read GetCount;

    procedure Assign(const aValue: TmcfSections; const aOperator: TmcfListAssignOp = aoCopy);
    function Add(const aName: String): TmcfSection;
    function Insert(const aIndex: Integer; const aName: String): TmcfSection;
    procedure Delete(const aIndex: Integer);
    function Remove(const aName: String): Integer;
    function IndexOf(const aValue: TmcfSection): Integer;
    function IndexOf(const aName: String): Integer;
    procedure Clear;

    function FindSection(const aPath: String): TmcfSection;
    function Exists(const aPath: String): Boolean;
    procedure List(const aStrings: TStrings; const aPrefix: String;
      const aListAttributes: Boolean = false; const aListValues: Boolean = false);

    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TmcfSection = class(TObject)
  private
    fAttributes: TmcfAttributes;
    fSections: TmcfSections;
  public
    property Attributes: TmcfAttributes read fAttributes;
    property Sections  : TmcfSections   read fSections;

    function GetInt(const aName: String; const aDefault: Int64 = 0): Int64;
    function GetFloat(const aName: String; const aDefault: Double = 0): Double;
    function GetBool(const aName: String; const aDefault: Boolean = false): Boolean;
    function GetString(const aName: String; const aDefault: String = ''): String;

    procedure SetInt(const aName: String; const aValue: Int64);
    procedure SetFloat(const aName: String; const aValue: Double);
    procedure SetBool(const aName: String; const aValue: Boolean);
    procedure SetString(const aName: String; const aValue: String);

    procedure Clear;
    procedure List(const aStrings: TStrings; const aPrefix: String;
      const aListAttributes: Boolean = false; const aListValues: Boolean = false);

    procedure Assign(const aValue: TmcfSection);
    constructor Create;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TmcfFile = class(TmcfSection)
  public
    procedure LoadFromFile(const aFilename: String);
    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToFile(const aFilename: String);
    procedure SaveToStream(const aStream: TStream);
  end;

  EmcfParseError = class(Exception);
  EmcfInvalidName = class(Exception);
  EmcfSections = class(Exception);
  EmcfAttributes = class(Exception);

  function mcfEscapeSpecChars(const aStr: String; const aFillChar: Char = #0): String;

implementation

uses
  strutils;

const
  SPEC_CHARS: array[0..4] of Char = ('.', ';', ':', '=', ' ');

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function CheckSpecChars(const aName: String): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to High(SPEC_CHARS) do
    if Pos(SPEC_CHARS[i], aName) > 0 then
      exit;
  result := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function mcfEscapeSpecChars(const aStr: String; const aFillChar: Char = #0): String;
var
  i: Integer;
  p, pOld: Integer;
begin
  result := aStr;
  for i := 0 to High(SPEC_CHARS) do begin
    p := Pos(SPEC_CHARS[i], result);
    while p > 0 do begin
      if aFillChar = #0 then begin
        Delete(result, p, 1);
        dec(p);
      end else
        result[p] := aFillChar;
      if p > 0 then
        p := PosEx(SPEC_CHARS[i], result, p)
      else
        p := Pos(SPEC_CHARS[i], result);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TmcfValue/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfValue.Assign(const aValue: TmcfValue);
begin
  fValue := aValue.fValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfValue.GetInt(const aDefault: Int64 = 0): Int64;
begin
  if not TryStrToInt64(fValue, result) then
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfValue.GetFloat(const aDefault: Double = 0): Double;
var
  format: TFormatSettings;
begin
  format.DecimalSeparator := '.';
  if not TryStrToFloat(fValue, result, format) then
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfValue.GetBool(const aDefault: Boolean = false): Boolean;
var
  i: Integer;
begin
  if TryStrToInt(fValue, i) then
    result := Boolean(i)
  else
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfValue.GetString: String;
begin
  result := fValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfValue.SetInt(const aValue: Int64);
begin
  fValue := IntToStr(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfValue.SetFloat(const aValue: Double);
var
  format: TFormatSettings;
begin
  format.DecimalSeparator := '.';
  fValue := FloatToStr(aValue, format);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfValue.SetBool(const aValue: Boolean);
begin
  fValue := IntToStr(Byte(aValue));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfValue.SetString(const aValue: String);
begin
  fValue := aValue;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TmcfAttributes////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Get(const aIndex: Integer): TmcfValue;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := (fList.Objects[aIndex] as TmcfValue)
  else
    raise EmcfAttributes.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Get(const aName: String): TmcfValue;
var
  i: Integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    result := Get(i)
  else
    result := Add(aName);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.GetName(const aIndex: Integer): String;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := fList[aIndex]
  else
    raise EmcfAttributes.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.Put(const aIndex: Integer; const aValue: TmcfValue);
begin
  if (aIndex >= 0) and (aIndex < Count) then
    fList.Objects[aIndex] := aValue
  else
    raise EmcfAttributes.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.Put(const aName: String; const aValue: TmcfValue);
var
  i: Integer;
  Attrib: TmcfValue;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Put(i, aValue)
  else
    Add(aName).Assign(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.PutName(const aIndex: Integer; const aValue: String);
begin
  if (aIndex >= 0) and (aIndex < Count) then begin
    if not CheckSpecChars(aValue) then
      raise EmcfInvalidName.Create('invalid characters in name: ' + aValue);
    if IndexOf(aValue) >= 0 then
      raise EmcfInvalidName.Create('name already exists: ' + aValue);
    fList[aIndex] := aValue
  end else
    raise EmcfAttributes.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.GetCount: Integer;
begin
  result := fList.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.Assign(const aValue: TmcfAttributes; const aOperator: TmcfListAssignOp = aoCopy);
var
  i: Integer;
  Attrib: TmcfValue;
begin
  if aOperator = aoCopy then
    Clear;
  for i := 0 to aValue.Count-1 do
    Add(aValue.Names[i]).Assign(aValue[i]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Add(const aName: String): TmcfValue;
begin
  result := Insert(Count, aName);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Insert(const aIndex: Integer; const aName: String): TmcfValue;
begin
  if not CheckSpecChars(aName) then
    raise EmcfInvalidName.Create('invalid characters in name: ' + aName);
  if IndexOf(aName) >= 0 then
    raise EmcfInvalidName.Create('name already exists: ' + aName);

  result := TmcfValue.Create;
  fList.InsertObject(aIndex, aName, result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.Delete(const aIndex: Integer);
begin
  if (aIndex >= 0) and (aIndex < Count) then
    fList.Delete(aIndex)
  else
    raise EmcfAttributes.Create('Index out of bounds: ' + IntToStr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Remove(const aName: String): Integer;
begin
  result := IndexOf(aName);
  if result >= 0 then
    Delete(result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.IndexOf(const aValue: TmcfValue): Integer;
begin
  result := fList.IndexOfObject(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.IndexOf(const aName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Names[i] = aName then begin
      result := i;
      exit;
    end;
  result := -1;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.Clear;
begin
  while Count > 0 do
    Delete(Count-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfAttributes.Exists(const aName: String): Boolean;
begin
  result := (IndexOf(aName) >= 0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfAttributes.List(const aStrings: TStrings; const aListValues: Boolean = false; aPrefix: String = '');
var
  i: Integer;
  s: String;
begin
  for i := 0 to Count-1 do begin
    s := Names[i];
    if aListValues then
      s := s + ' = ' + Get(i).GetString;
    if aPrefix <> '' then
      s := aPrefix + '.' + s;
    aStrings.Add(s);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TmcfAttributes.Create;
begin
  inherited Create;
  fList := TStringList.Create;
  fList.OwnsObjects := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TmcfAttributes.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TmcfSections//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Get(const aIndex: Integer): TmcfSection;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := (fList.Objects[aIndex] as TmcfSection)
  else
    raise EmcfSections.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Get(const aName: String): TmcfSection;
var
  i: Integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    result := Get(i)
  else
    result := Add(aName);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.GetName(const aIndex: Integer): String;
begin
  if (aIndex >= 0) and (aIndex < Count) then
    result := fList[aIndex]
  else
    raise EmcfSections.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.Put(const aIndex: Integer; const aValue: TmcfSection);
begin
  if (aIndex >= 0) and (aIndex < Count) then
    fList.Objects[aIndex] := aValue
  else
    raise EmcfSections.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.Put(const aName: String; const aValue: TmcfSection);
var
  i: Integer;
begin
  i := IndexOf(aName);
  if i < 0 then
    Add(aName).Assign(aValue)
  else
    Put(i, aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.PutName(const aIndex: Integer; const aValue: String);
begin
  if (aIndex >= 0) and (aIndex < Count) then begin
    if not CheckSpecChars(aValue) then
      raise EmcfInvalidName.Create('invalid characters in name: ' + aValue);
    if IndexOf(aValue) >= 0 then
      raise EmcfInvalidName.Create('name already exists: ' + aValue);
    fList[aIndex] := aValue
  end else
    raise EmcfSections.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.GetCount: Integer;
begin
  result := fList.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.Assign(const aValue: TmcfSections; const aOperator: TmcfListAssignOp = aoCopy);
var
  i: Integer;
begin
  if aOperator = aoCopy then
    Clear;
  for i := 0 to aValue.Count-1 do
    Add(aValue.Names[i]).Assign(aValue[i]);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Add(const aName: String): TmcfSection;
begin
  result := Insert(Count, aName);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Insert(const aIndex: Integer; const aName: String): TmcfSection;
begin
  if not CheckSpecChars(aName) then
    raise EmcfInvalidName.Create('invalid characters in name');
  if IndexOf(aName) >= 0 then
    raise EmcfInvalidName.Create('name already exists');
  result := TmcfSection.Create;
  fList.InsertObject(aIndex, aName, result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.Delete(const aIndex: Integer);
begin
  if (aIndex >= 0) and (aIndex < Count) then
    fList.Delete(aIndex)
  else
    raise EmcfSections.Create('Index out of bounds: ' + IntToSTr(aIndex));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Remove(const aName: String): Integer;
begin
  result := IndexOf(aName);
  if result >= 0 then
    Delete(result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.IndexOf(const aValue: TmcfSection): Integer;
begin
  result := fList.IndexOfObject(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.IndexOf(const aName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Names[i] = aName then begin
      result := i;
      exit;
    end;
  result := -1;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.Clear;
begin
  while Count > 0 do
    Delete(Count-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.FindSection(const aPath: String): TmcfSection;
var
  p: Integer;
  Name: String;
begin
  p := Pos('.', aPath);
  if p > 0 then
    Name := Copy(aPath, 1, p-1)
  else
    Name := aPath;

  if p > 0 then
    result := Get(Name).Sections.FindSection(Copy(aPath, p+1, Length(aPath)))
  else
    result := Get(Name);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSections.Exists(const aPath: String): Boolean;
var
  p, Index: Integer;
  Name: String;
begin
  p := Pos('.', aPath);
  if p > 0 then
    Name := Copy(aPath, 1, p-1)
  else
    Name := aPath;

  Index := IndexOf(Name);
  if Index >= 0 then begin
    if p > 0 then begin
      result := Get(Index).Sections.Exists(Copy(aPath, p+1, Length(aPath)));
    end else result := true;
  end else result := false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSections.List(const aStrings: TStrings; const aPrefix: String;
            const aListAttributes: Boolean = false; const aListValues: Boolean = false);
var
  i: Integer;
  s: String;
begin
  for i := 0 to Count-1 do begin
    s := Names[i];
    if aPrefix <> '' then
      s := aPrefix + '.' + s;
    aStrings.Add(s);
    if aListAttributes then
      Get(i).Attributes.List(aStrings, aListValues, s);
    Get(i).Sections.List(aStrings, s, aListAttributes, aListValues);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TmcfSections.Create;
begin
  inherited Create;
  fList := TStringList.Create;
  fList.OwnsObjects := true;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TmcfSections.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TmcfSection///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSection.GetInt(const aName: String; const aDefault: Int64 = 0): Int64;
begin
  if fAttributes.Exists(aName) then
    result := fAttributes.Get(aName).GetInt(aDefault)
  else
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSection.GetFloat(const aName: String; const aDefault: Double = 0): Double;
begin
  if fAttributes.Exists(aName) then
    result := fAttributes.Get(aName).GetFloat(aDefault)
  else
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSection.GetBool(const aName: String; const aDefault: Boolean = false): Boolean;
begin
  if fAttributes.Exists(aName) then
    result := fAttributes.Get(aName).GetBool(aDefault)
  else
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TmcfSection.GetString(const aName: String; const aDefault: String = ''): String;
begin
  if fAttributes.Exists(aName) then
    result := fAttributes.Get(aName).GetString
  else
    result := aDefault;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.SetInt(const aName: String; const aValue: Int64);
begin
  fAttributes.Get(aName).SetInt(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.SetFloat(const aName: String; const aValue: Double);
begin
  fAttributes.Get(aName).SetFloat(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.SetBool(const aName: String; const aValue: Boolean);
begin
  fAttributes.Get(aName).SetBool(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.SetString(const aName: String; const aValue: String);
begin
  fAttributes.Get(aName).SetString(aValue);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.Clear;
begin
  Attributes.Clear;
  Sections.Clear;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.List(const aStrings: TStrings; const aPrefix: String;
  const aListAttributes: Boolean = false; const aListValues: Boolean = false);
begin
  if aListAttributes then
    Attributes.List(aStrings, aListValues, '');
  Sections.List(aStrings, aPrefix, aListAttributes, aListValues);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfSection.Assign(const aValue: TmcfSection);
begin
  fAttributes.Assign(aValue.Attributes);
  fSections.Assign(aValue.Sections);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TmcfSection.Create;
begin
  inherited Create;
  fAttributes := TmcfAttributes.Create;
  fSections   := TmcfSections.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
destructor TmcfSection.Destroy;
begin
  fAttributes.Free;
  fSections.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfFile.LoadFromFile(const aFilename: String);
var
  Stream: TStream;
begin
  if not FileExists(aFilename) then
    raise Exception.Create('file not found: '+aFilename);

  Stream := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfFile.LoadFromStream(const aStream: TStream);

  function NextChar: Char;
  begin
    result := #0;
    if aStream.Position < aStream.Size then
      aStream.Read(result, SizeOf(result));
  end;

  procedure ParseValue(const aValue: TmcfValue);
  var
    buf: String;
    c: Char;
    isStr: Boolean;
  begin
    buf := '';
    c := NextChar;
    isStr := false;
    while (c <> #0) do begin
      if (c = '''') then
        isStr := not isStr;
      if (c = ';') and not isStr then
        break;
      buf := buf + c;
      c := NextChar;
    end;
    buf := Trim(buf);
    if (Length(buf) > 0) and (buf[1] = '''') then
      Delete(buf, 1, 1);
    if (Length(buf) > 0) and (buf[Length(buf)] = '''') then
      Delete(buf, Length(buf), 1);
    aValue.SetString(buf);
  end;

  procedure ParseSection(const aSection: TmcfSection);
  var
    buf: String;
    c: Char;
  begin
    buf := '';
    c := NextChar;
    while c <> #0 do begin
      case c of
        ':': begin
          buf := Trim(buf);
          if not CheckSpecChars(buf) then
            raise EmcfParseError.Create('invalid section name: ' + buf);
          ParseSection(aSection.Sections.Add(buf));
          buf := '';
        end;
        '=': begin
          buf := Trim(buf);
          if not CheckSpecChars(buf) then
            raise EmcfParseError.Create('invalid attribute name: ' + buf);
          ParseValue(aSection.Attributes.Add(buf));
          buf := '';
        end;
        ';': begin
          buf := LowerCase(Trim(buf));
          if not (buf = 'end') then
            raise EmcfParseError.Create('invalid section end: ' + buf);
          exit;
        end;
      else
        buf := buf + c;
      end;
      c := NextChar;
    end;
  end;

begin
  Clear;
  ParseSection(self);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfFile.SaveToFile(const aFilename: String);
var
  Stream: TFileStream;
  dir: String;
begin
  dir := ExtractFilePath(aFilename);
  if (dir <> '') and not DirectoryExists(dir) then
    ForceDirectories(dir);

  Stream := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TmcfFile.SaveToStream(const aStream: TStream);

  procedure WriteStr(const aStr: String);
  begin
    aStream.Write(aStr[1], Length(aStr));
  end;

  procedure SaveSection(const aSection: TmcfSection; const aPrefix: String);
  var
    i: Integer;
    s: String;
  begin
    with aSection do begin
      for i := 0 to Attributes.Count-1 do begin
        s := aPrefix + Attributes.Names[i] + ' = ';
        if not CheckSpecChars(Attributes[i].GetString) then
          s := s + '''' + Attributes[i].GetString + ''''
        else
          s := s + Attributes[i].GetString;
        s := s + ';' + sLineBreak;
        WriteStr(s);
      end;
      for i := 0 to Sections.Count-1 do begin
        WriteStr(aPrefix + Sections.Names[i] + ':' + sLineBreak);
        SaveSection(Sections[i], aPrefix + '  ');
        WriteStr(aPrefix + 'end;' + sLineBreak);
      end;
    end;
  end;

begin
  SaveSection(self, '');
end;

end.

