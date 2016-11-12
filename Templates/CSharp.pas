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

function DEF_CTMETAFIELD_DATATYPE_NAMES_CSHARP(idx: TCtFieldDataType): string;
begin
  case Integer(idx) of
    0: Result := 'unknown';
    1: Result := 'string';
    2: Result := 'int';
    3: Result := 'double';
    4: Result := 'DateTime';
    5: Result := 'bool';
    6: Result := 'enum';
    7: Result := 'var';
    8: Result := 'object';
    9: Result := 'List';
    10: Result := 'function';
    11: Result := 'EventHnadler';
    12: Result := 'type';
  else
    Result := 'unknown';
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

var
  I, L: Integer;
  clsName, S, T, V, FT: string;
  f: TCtMetaField;

function GFieldName(Fld: TCtMetaField): string;
begin
  Result := GetDesName(f.Name, f.DisplayName);
end;

function GFieldType(Fld: TCtMetaField): string;
begin
  if f.DataType = cfdtOther then
    Result := f.DataTypeName
  else if f.DataType = cfdtEnum then
    Result := getPublicName(GFieldName(f)) + 'Enum'
  else
    Result := DEF_CTMETAFIELD_DATATYPE_NAMES_CSHARP(f.DataType);
end;

procedure AddFieldInfo;
begin
  S := GetDesName(f.Name, f.DisplayName);
  if f.DataType = cfdtFunction then
  begin
    FT := f.DataTypeName;
    if FT = '' then
      FT := 'void';
    S := 'public ' + FT + ' ' + getPublicName(S) + '()'#13#10
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
      FT := DEF_CTMETAFIELD_DATATYPE_NAMES_CSHARP(f.DataType);
    S := 'public ' + FT + ' ' + getPublicName(S) + #13#10
      + '{' + #13#10
      + '  get { return ' + getProtectName(S) + '; }' + #13#10
      + '  set { ' + getProtectName(S) + ' = value; }' + #13#10
      + '}';
  end;

  T := F.GetFieldComments;
  if T <> '' then
  begin
      //T := F.Comment;
    if (Pos(#13, T) > 0) or (Pos(#10, T) > 0) then
    begin
      if Length(T) <= 100 then
      begin
        T := StringReplace(T, #13#10, ' ', [rfReplaceAll]);
        T := StringReplace(T, #13, ' ', [rfReplaceAll]);
        T := StringReplace(T, #10, ' ', [rfReplaceAll]);
        S := '//' + T + #13#10 + S;
      end
      else
      begin
        T := '{' + StringReplace(T, '}', '%7D', [rfReplaceAll]) + '}';
        S := T + #13#10 + S;
      end;
    end
    else
      S := '//' + T + #13#10 + S;
  end;

  CurOut.Add('    ' + StringReplace(S, #13#10, #13#10'    ', [rfReplaceAll]));
end;
begin
  with CurTable do
  begin
    S := GetTableComments;
    CurOut.Add('/*');      
    CurOut.Add('  ###CSharp Code Generate###');
    CurOut.Add('  ' + Name);
    CurOut.Add('  Create by User(EMAIL) ' + DateTimeToStr(Now));
    S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
    if S <> '' then
      CurOut.Add('  ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]));
    CurOut.Add('');
    CurOut.Add(Describe);
    CurOut.Add('*/');
    CurOut.Add('');

    CurOut.Add('using System;');
    CurOut.Add('using System.Text;');
    CurOut.Add('');
    CurOut.Add('namespace ' + Name);
    CurOut.Add('{');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtEnum:
          begin
            CurOut.Add('  public enum ' + GFieldType(f) + '{ Unknown, Value1, Value2 }');
            CurOut.Add('');
          end;
      end;
    end;

    S := GetTableComments;
    if S <> '' then
    begin
      S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
      CurOut.Add('  /* ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]) + ' */');
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

    CurOut.Add('  public class ' + clsName);
    CurOut.Add('  {');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      if f.DataType <> cfdtFunction then
        CurOut.Add('    protected ' + GFieldType(f) + ' ' + getProtectName(GFieldName(f)) + ';');
    end;
    CurOut.Add('');

    CurOut.Add('    public ' + clsName + '()');
    CurOut.Add('    {');
    CurOut.Add('    }');

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      AddFieldInfo;
    end;

    CurOut.Add('    public void Reset()');
    CurOut.Add('    {');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := '';
      case f.DataType of
        cfdtInteger, cfdtFloat:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = 0;');
        cfdtString:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = null;');
        cfdtDate:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = 0;');
        cfdtEnum:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = ' + GFieldType(f) + '.Unknown;');
        cfdtBool:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = false;');
        cfdtList:
          CurOut.Add('        ' + ExtStr(V + S, L) + '.Clear();');
        cfdtEvent:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = null;');
        cfdtFunction: ;
      else
        CurOut.Add('        ' + ExtStr(S, L) + ' .Reset();');
      end;
    end;
    CurOut.Add('    }');
    CurOut.Add('');


    CurOut.Add('    public void AssignFrom(' + clsName + ' AObj)');
    CurOut.Add('    {');
    CurOut.Add('        if(AObj==null)');
    CurOut.Add('        {');
    CurOut.Add('            Reset();');
    CurOut.Add('            return;');
    CurOut.Add('        }');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := ''; //RemRootCtField(f.Name);
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('        ' + ExtStr(V + S, L) + ' = AObj.' + S + ';');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('        ' + ExtStr(S, L) + ' .  AssignFrom(AObj.' + S + ');');
      end;
    end;
    CurOut.Add('    }');
    CurOut.Add('');


    CurOut.Add('  }');
    CurOut.Add('');

    CurOut.Add('}');

    CurOut.Add('');

  end;
end.

