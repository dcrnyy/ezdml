function ExtStr(Str: string; Len: Integer): string;
var
  I: Integer;
begin
  Str := Trim(Str);
  if Length(Str) <= Len then
    for I := Length(Str) to Len do
      Str := Str + ' ';
  Result := Str;
end;

function DEF_CTMETAFIELD_DATATYPE_NAMES_JAVA(idx: TCtFieldDataType): string;
begin
  case Integer(idx) of
    0: Result := 'Unknown';
    1: Result := 'String';
    2: Result := 'int';
    3: Result := 'double';
    4: Result := 'Date';
    5: Result := 'boolean';
    6: Result := 'int';
    7: Result := 'Object';
    8: Result := 'Object';
    9: Result := 'List';
    10: Result := 'function';
    11: Result := 'EventClass';
    12: Result := 'class';
  else
    Result := 'Unknown';
  end;
end;


function getProtectName(N: string): string;
begin
  Result := N;
  if (Result <> '') and (N <> UpperCase(N)) then
    if Result[1] >= 'A' then
      if Result[1] <= 'Z' then
        Result[1] := Chr(Ord(Result[1]) + (Ord('a') - Ord('A')));
end;

function getPublicName(N: string): string;
begin
  Result := N;
  if (Result <> '') then
    if Result[1] >= 'a' then
      if Result[1] <= 'z' then
        Result[1] := Chr(Ord(Result[1]) - (Ord('a') - Ord('A')));
end;

function GetDesName(p, n: string): string;
begin
  if p = '' then
    Result := n
  else
    Result := p;
end;

function GFieldName(F: TCtMetaField): string;
begin
  Result := GetDesName(f.Name, f.DisplayName);
end;

function GFieldType(F: TCtMetaField): string;
begin
  if f.DataType = cfdtOther then
    Result := f.DataTypeName
  else
    Result := DEF_CTMETAFIELD_DATATYPE_NAMES_JAVA(f.DataType);
end;

procedure AddFieldInfo(F: TCtMetaField);
var
  S, T, T1, T2, FT: string;
begin
  S := GetDesName(f.Name, f.DisplayName);
  T := F.GetFieldComments;
  if T <> '' then
  begin
    //T := F.Comment;
    T := StringReplace(T, #13#10, #13#10' * ', [rfReplaceAll]);
    T := StringReplace(T, #13, #13' * ', [rfReplaceAll]);
    T := StringReplace(T, #10, #10' * ', [rfReplaceAll]);
  end;

  if f.DataType = cfdtFunction then
  begin
    FT := f.DataTypeName;
    if FT = '' then
      FT := 'void';
    if T<>'' then
      T2 := '/**'#13#10' * ' + T + #13#10' */'#13#10
    else
      T2 := '';
    S := T2+ 'public ' + FT + ' ' + getPublicName(S) + '()'#13#10
      + '{' + #13#10
      + '}';
  end
  else
  begin
    if f.DataType = cfdtOther then
      FT := f.DataTypeName
    else if f.DataType = cfdtEnum then
      FT := GFieldType(f)
    else
      FT := DEF_CTMETAFIELD_DATATYPE_NAMES_JAVA(f.DataType);
    if T<>'' then
    begin
      T1 := '/**'#13#10' * ªÒ»°' + T + #13#10' */'#13#10
      T2 := '/**'#13#10' * …Ë÷√' + T + #13#10' */'#13#10
    end
    else
    begin
      T1 := '';
      T2 := '';
    end;
    S := T1+'public ' + FT + ' get' + getPublicName(S) + '()'#13#10
      + '{' + #13#10
      + '  return ' + getProtectName(S) + ';' + #13#10
      + '}' + #13#10
      + T2+'public void set' + getPublicName(S) + '(' + FT + ' value)'#13#10
      + '{' + #13#10
      + '  this.' + getProtectName(S) + ' = value;' + #13#10
      + '}';
  end;

  CurOut.Add('  ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]));
end;


var
  I, L: Integer;
  clsName, S, V: string;
  f: TCtMetaField;
begin
  with CurTable do
  begin
    S := GetTableComments;
    CurOut.Add('/**');
    CurOut.Add(' * ' + Name);
    if S <> '' then
      CurOut.Add(' * ' + StringReplace(S, #13#10, #13#10' * ', [rfReplaceAll]));
    CurOut.Add(' * @author User(EMAIL) ' + DateTimeToStr(Now));
    CurOut.Add(' * @version 1.0 ' + DateTimeToStr(Now));
    CurOut.Add('');
    CurOut.Add(Describe);
    CurOut.Add('');
    CurOut.Add('SQLMAP:');
    CurOut.Add('<resultMap id="resMap_' + Name + '"');
    CurOut.Add('	class="' + Name + '.' + Name + '">');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      CurOut.Add('	<result property="' + getProtectName(GFieldName(f)) + '" column="' + UpperCase(GFieldName(f)) + '" />');
    end;
    CurOut.Add('</resultMap>');
    CurOut.Add('<select id="sel_' + Name + '" resultMap="resMap_' + Name + '"><![CDATA[');
    CurOut.Add('  select t.* from ' + Name + ' t');
    CurOut.Add(']]></select>');
    CurOut.Add('');

    CurOut.Add('*/');
    CurOut.Add('');

    CurOut.Add('package ' + Name + ';');
    CurOut.Add('');
    CurOut.Add('import java.sql.Date;');
    CurOut.Add('');

    S := GetTableComments;
    if S <> '' then
    begin
      S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
      CurOut.Add('/** '#13#10' * ' + StringReplace(S, #13#10, #13#10' * ', [rfReplaceAll]) + #13#10' */');
    end;

    L := 0;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GetDesName(f.Name, f.DisplayName);
      if L < Length(S) then
        L := Length(S);
    end;

    clsName := Name;

    CurOut.Add('public class ' + clsName);
    CurOut.Add('{');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtEnum:
          begin
            CurOut.Add('  public static final int ' + clsName + getPublicName(GFieldName(f)) + 'Unknown = 0;');
            CurOut.Add('  public static final int ' + clsName + getPublicName(GFieldName(f)) + 'Value1 = 1;');
            CurOut.Add('  public static final int ' + clsName + getPublicName(GFieldName(f)) + 'Value2 = 2;');
            CurOut.Add('');
          end;
      end;
    end;

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      if f.DataType <> cfdtFunction then
        CurOut.Add('  protected ' + GFieldType(f) + ' ' + getProtectName(GFieldName(f)) + ';');
    end;
    CurOut.Add('');

    CurOut.Add('  public ' + clsName + '()');
    CurOut.Add('  {');
    CurOut.Add('  }');

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      AddFieldInfo(f);
    end;

    CurOut.Add('');
    CurOut.Add('  public void reset()');
    CurOut.Add('  {');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := '';
      case f.DataType of
        cfdtInteger, cfdtFloat:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = 0;');
        cfdtString:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = null;');
        cfdtDate:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = null;');
        cfdtEnum:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = ' + getPublicName(GFieldName(f)) + '_Unknown;');
        cfdtBool:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = false;');
        cfdtList:
          CurOut.Add('    ' + ExtStr(V + S, L) + '.Clear();');
        cfdtEvent:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = null;');
        cfdtFunction: ;
      else
        CurOut.Add('    ' + ExtStr(S, L) + ' .reset();');
      end;
    end;
    CurOut.Add('  }');
    CurOut.Add('');


    CurOut.Add('  public void assignFrom(' + clsName + ' AObj)');
    CurOut.Add('  {');
    CurOut.Add('    if(AObj==null)');
    CurOut.Add('    {');
    CurOut.Add('      Reset();');
    CurOut.Add('      return;');
    CurOut.Add('    }');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := ''; //RemRootCtField(f.Name);
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = AObj.' + S + ';');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('    ' + ExtStr(S, L) + ' .  assignFrom(AObj.' + S + ');');
      end;
    end;
    CurOut.Add('  }');
    CurOut.Add('');


    CurOut.Add('}');
    CurOut.Add('');

  end;
end.
d.
