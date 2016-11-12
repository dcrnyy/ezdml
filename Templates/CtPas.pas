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

function DEF_CTMETAFIELD_DATATYPE_NAMES_PAS(idx: TCtFieldDataType): string;
begin
  case Integer(idx) of
    0: Result := 'TUnknown';
    1: Result := 'String';
    2: Result := 'Integer';
    3: Result := 'Double';
    4: Result := 'TDateTime';
    5: Result := 'Boolean';
    6: Result := 'Enum';
    7: Result := 'Variant';
    8: Result := 'TObject';
    9: Result := 'TList';
    10: Result := 'Function';
    11: Result := 'TNotifyEvent';
    12: Result := 'Type';
  else
    Result := 'TUnknown';
  end;
end;

const
  srCtobjFuns = 'CT Object Functions';   
  srListNameFmt = '%sList';
  srListName2Fmt = 'List class %s';

function GetDesName(p, n: string): string;
begin
  if p = '' then
    Result := n
  else
    Result := p;
end;

function RootCtFields(idx: Integer): string;
begin
  case idx of
    0: Result := 'ID';
    1: Result := 'RID';
    2: Result := 'PID';
    3: Result := 'NAME';
    4: Result := 'CAPTION';
    5: Result := 'MEMO';
    6: Result := 'CreateDate';
    7: Result := 'CreatorName';
    8: Result := 'Creator';
    9: Result := 'Modifier';
    10: Result := 'ModifyDate';
    11: Result := 'ModifierName';
    12: Result := 'TypeName';
    13: Result := 'VersionNo';
    14: Result := 'HistoryID';
    15: Result := 'LockStamp';
    16: Result := 'DataLevel';
    17: Result := 'OrderNo';
  else
    Result := '';
  end;
end;

function IsRootCtField(AField: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to 17 do
    if UpperCase(RootCtFields(I)) = UpperCase(AField) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function RemRootCtField(AField: string): string;
begin
  if IsRootCtField(AField) then
    Result := '//'
  else
    Result := '';
end;
var
  I, L, lastFieldidx: Integer;
  clsName, S, T, FT, V, X: string;
  Infos: TStringList;
  f: TCtMetaField;

function GFieldName(Fld: TCtMetaField): string;
begin
  Result := GetDesName(f.Name, f.DisplayName);
end;

function GFieldType(Fld: TCtMetaField): string;
begin
  if (f.DataType = cfdtOther) or (f.DataType = cfdtObject)
    or (f.DataType = cfdtList) or (f.DataType = cfdtEvent) then
  begin
    Result := f.DataTypeName;
    if Result = '' then
      Result := DEF_CTMETAFIELD_DATATYPE_NAMES_PAS(f.DataType);
  end
  else if f.DataType = cfdtEnum then
    Result := 'T' + GFieldName(fld) + 'Enum'
  else
    Result := DEF_CTMETAFIELD_DATATYPE_NAMES_PAS(f.DataType);
end;

procedure AddFieldInfo;
begin
  if f.DataType = cfdtFunction then
  begin
    S := ExtStr(GetDesName(f.Name, f.DisplayName), L - 1);
    FT := f.DataTypeName;
    if (FT <> '') and (Pos('(', FT) = 0) then
      FT := '(): ' + FT;
    S := ExtStr('function ', 8) + S + ' ' + FT + ';';
  end
  else
  begin
    S := GetDesName(f.Name, f.DisplayName);
    if IsRootCtField(f.Name) then
      V := S + ';//'
    else
      V := S;
    S := ExtStr(S, L - 1);
    V := ExtStr(V, L - 1);
    FT := GFieldType(f);
    S := ExtStr('property ', 8) + V + ': ' + ExtStr(FT, 12)
      + ' read F' + S + ' write F' + S + ';';
  end;


  T := F.GetFieldComments;
  if T <> '' then
  begin
      //T := F.Memo;
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
    CurOut.Add('(*');      
    CurOut.Add('  ###CtPascal Code Generate###');
    CurOut.Add('  ' + Name);
    CurOut.Add('  Create by User(EMAIL) ' + DateTimeToStr(Now));
    S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
    if S <> '' then
      CurOut.Add('  ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]));
    CurOut.Add('');
    CurOut.Add(Describe);
    CurOut.Add('*)');
    CurOut.Add('');

    CurOut.Add('unit ' + Name + ';');
    CurOut.Add('');

    CurOut.Add('interface');
    CurOut.Add('');
    CurOut.Add('uses');
    CurOut.Add('  Classes, Windows, SysUtils, Variants, Graphics, Controls,');
    CurOut.Add('  CTMetaData, CtObjSerialer, Oracle;');
    CurOut.Add('');

    CurOut.Add('type');
    CurOut.Add('');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtEnum:
          begin
            CurOut.Add('  ' + GFieldType(f) + '=( ' + GFieldType(f) + 'Unknown, ' + GFieldType(f) + 'Value1, ' + GFieldType(f) + 'Value2 );');
            CurOut.Add('');
          end;
      end;
    end;

    S := GetTableComments;
    if S <> '' then
    begin
      S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
      CurOut.Add('  { ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]) + ' }');
    end;

    L := 0;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GetDesName(f.Name, f.DisplayName);
      if L < Length(S) then
        L := Length(S);
    end;

    clsName := 'T' + Name;

    CurOut.Add('  ' + clsName + ' = class(TCtObject)');
    CurOut.Add('  private');
    CurOut.Add('  protected');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction: ;
      else
        CurOut.Add('    ' + ExtStr(RemRootCtField(f.Name) + 'F' + GFieldName(f), L) + ' : ' + GFieldType(f) + ';');
      end;
    end;
    CurOut.Add('  public');
    CurOut.Add('    constructor Create; override;');
    CurOut.Add('    destructor Destroy; override;');
    CurOut.Add('');

    CurOut.Add('    //' + srCtobjFuns);
    CurOut.Add('    procedure Reset; override;');
    CurOut.Add('    procedure AssignFrom(ACtObj: TCtObject); override;');
    CurOut.Add('    procedure LoadFromSerialer(ASerialer: TCtObjSerialer); override;');
    CurOut.Add('    procedure SaveToSerialer(ASerialer: TCtObjSerialer); override;');
    CurOut.Add('');
    CurOut.Add('    procedure LoadFromOracle(AQuery: TOracleQuery; AID: Integer; AReopen: Boolean);');
    CurOut.Add('    procedure SaveToOracle(AQuery: TOracleQuery; AInsert: Boolean);');
    CurOut.Add('    function  GetNextSeqID(AQuery: TOracleQuery): Integer;');
    CurOut.Add('');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction:
          AddFieldInfo;
      end;
    end;
    CurOut.Add('');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction: ;
      else
        AddFieldInfo;
      end;
    end;
    CurOut.Add('  end;');
    CurOut.Add('');

    S := GetTableComments;
    if S <> '' then
    begin
      if (S = Caption) or (S = Name) then
        S := Format(srListNameFmt, [S])
      else
        S := Format(srListName2Fmt, [S]);
      S := StringReplace(S, '}', '%7D', [rfReplaceAll]);
      CurOut.Add('  { ' + StringReplace(S, #13#10, #13#10'  ', [rfReplaceAll]) + ' }');
    end;

    CurOut.Add('  ' + clsName + 'List = class(TCtObjectList)');
    CurOut.Add('  private');
    CurOut.Add('    function GetItem(Index: Integer): ' + clsName + ';');
    CurOut.Add('    procedure PutItem(Index: Integer; const Value: ' + clsName + ');');
    CurOut.Add('  protected');
    CurOut.Add('    function CreateObj: TCtObject; override;');
    CurOut.Add('  public');
    CurOut.Add('    property Items[Index: Integer]: ' + clsName + ' read GetItem write PutItem; default;');
    CurOut.Add('  end;');
    CurOut.Add('');

    CurOut.Add('implementation');
    CurOut.Add('');
    CurOut.Add('');

    CurOut.Add('{ ' + clsName + ' }');
    CurOut.Add('');
    CurOut.Add('constructor ' + clsName + '.Create;');
    CurOut.Add('begin');
    CurOut.Add('  inherited;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtObject, cfdtList, cfdtOther:
          CurOut.Add('  F' + ExtStr(GFieldName(f), L - 1) + ' := ' + GFieldType(f) + '.Create;');
      end;
    end;
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('destructor ' + clsName + '.Destroy;');
    CurOut.Add('begin');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtObject, cfdtList, cfdtOther:
          CurOut.Add('  FreeAndNil(F' + GFieldName(f) + ');');
      end;
    end;
    CurOut.Add('  inherited;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.Reset;');
    CurOut.Add('begin');
    CurOut.Add('  inherited;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      V := RemRootCtField(f.Name);
      case f.DataType of
        cfdtInteger, cfdtFloat:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := 0;');
        cfdtString:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := '''';');
        cfdtDate:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := 0;');
        cfdtEnum:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := ' + GFieldType(f) + 'Unknown;');
        cfdtBool:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := False;');
        cfdtList:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + '.Clear;');
        cfdtEvent:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := nil;');
        cfdtFunction: ;
      else
        CurOut.Add('  ' + ExtStr('F' + GFieldName(f), L) + ' .Reset;');
      end;
    end;
    CurOut.Add('end;');
    CurOut.Add('');


    CurOut.Add('procedure ' + clsName + '.AssignFrom(ACtObj: TCtObject);');
    CurOut.Add('var');
    CurOut.Add('  cobj: ' + clsName + ';');
    CurOut.Add('begin');
    CurOut.Add('  inherited;');
    CurOut.Add('  if not (ACtObj is ' + clsName + ') then');
    CurOut.Add('    Exit;');
    CurOut.Add('  cobj:= ' + clsName + '(ACtObj);');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      V := RemRootCtField(f.Name);
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := cobj.F' + GFieldName(f) + ';');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('  F' + ExtStr(GFieldName(f), L - 1) + ' .  AssignFrom(cobj.F' + GFieldName(f) + ');');
      end;
    end;
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.LoadFromSerialer(ASerialer: TCtObjSerialer);');
    CurOut.Add('begin');
    CurOut.Add('  BeginSerial(ASerialer, True);');
    CurOut.Add('  try');
    CurOut.Add('    inherited;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      V := RemRootCtField(f.Name);
      case f.DataType of
        cfdtInteger:
          CurOut.Add('    ' + V + 'ASerialer.ReadInteger(''' + S + ''', F' + S + ');');
        cfdtEnum:
          CurOut.Add('    ' + V + 'F' + S + ' := ' + GFieldType(f) + '(ASerialer.ReadInt(''' + S + '''));');
        cfdtString:
          CurOut.Add('    ' + V + 'ASerialer.ReadString(''' + S + ''', F' + S + ');');
        cfdtFloat:
          CurOut.Add('    ' + V + 'ASerialer.ReadFloat(''' + S + ''', F' + S + ');');
        cfdtDate:
          CurOut.Add('    ' + V + 'ASerialer.ReadDate(''' + S + ''', F' + S + ');');
        cfdtBool:
          CurOut.Add('    ' + V + 'ASerialer.ReadBool(''' + S + ''', F' + S + ');');
        cfdtFunction, cfdtEvent: ;
      else
        ; //CurOut.Add('    ASerialer.ReadObject(''' + S + ''', F' + S + ');');
      end;
    end;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum: ;
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('    F' + GFieldName(f) + '.LoadFromSerialer(ASerialer);');
      end;
    end;
    CurOut.Add('  finally');
    CurOut.Add('    EndSerial(ASerialer, True);');
    CurOut.Add('  end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.SaveToSerialer(ASerialer: TCtObjSerialer);');
    CurOut.Add('begin');
    CurOut.Add('  BeginSerial(ASerialer, False);');
    CurOut.Add('  try');
    CurOut.Add('    inherited;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      V := RemRootCtField(f.Name);
      case f.DataType of
        cfdtInteger:
          CurOut.Add('    ' + V + 'ASerialer.WriteInteger(''' + S + ''', F' + S + ');');
        cfdtEnum:
          CurOut.Add('    ' + V + 'ASerialer.WriteInteger(''' + S + ''', Integer(F' + S + '));');
        cfdtString:
          CurOut.Add('    ' + V + 'ASerialer.WriteString(''' + S + ''', F' + S + ');');
        cfdtFloat:
          CurOut.Add('    ' + V + 'ASerialer.WriteFloat(''' + S + ''', F' + S + ');');
        cfdtDate:
          CurOut.Add('    ' + V + 'ASerialer.WriteDate(''' + S + ''', F' + S + ');');
        cfdtBool:
          CurOut.Add('    ' + V + 'ASerialer.WriteBool(''' + S + ''', F' + S + ');');
        cfdtFunction, cfdtEvent: ;
      else
        ; //CurOut.Add('    ASerialer.WriteObject(''' + S + ''', F' + S + ');');
      end;
    end;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum: ;
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('    F' + GFieldName(f) + '.SaveToSerialer(ASerialer);');
      end;
    end;
    CurOut.Add('  finally');
    CurOut.Add('    EndSerial(ASerialer, False);');
    CurOut.Add('  end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.LoadFromOracle(AQuery: TOracleQuery; AID: Integer; AReopen: Boolean);');
    CurOut.Add('begin');
    CurOut.Add('  with AQuery do');
    CurOut.Add('  begin');
    CurOut.Add('    if AReopen then');
    CurOut.Add('    begin');
    CurOut.Add('      Clear;');
    CurOut.Add('      Sql.Text := ''select * from ' + Name + ' where ' + KeyFieldName + '=:V_ID'';');
    CurOut.Add('      DeclareVariable(''V_ID'', otInteger);');
    CurOut.Add('      SetVariable(''V_ID'', AID);');
    CurOut.Add('      Execute;');
    CurOut.Add('    end;');
    CurOut.Add('    if Eof then');
    CurOut.Add('      raise Exception.Create(''Load object failed: data not found'');');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      case f.DataType of
        cfdtInteger:
          CurOut.Add('    F' + S + ' := FieldAsInteger(''' + S + ''');');
        cfdtEnum:
          CurOut.Add('    F' + S + ' := ' + GFieldType(f) + '(FieldAsInteger(''' + S + '''));');
        cfdtString:
          CurOut.Add('    F' + S + ' := FieldAsString(''' + S + ''');');
        cfdtFloat:
          CurOut.Add('    F' + S + ' := FieldAsFloat(''' + S + ''');');
        cfdtDate:
          CurOut.Add('    F' + S + ' := FieldAsDate(''' + S + ''');');
        cfdtBool:
          CurOut.Add('    F' + S + ' := (FieldAsInteger(''' + S + ''')=1);');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('    F' + S + ' := FieldAsObject(''' + S + ''');');
      end;
    end;
    CurOut.Add('  end;');
    CurOut.Add('end;');
    CurOut.Add('');


    lastFieldidx := 0;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          lastFieldidx := I;
      end;
    end;
    CurOut.Add('procedure ' + clsName + '.SaveToOracle(AQuery: TOracleQuery; AInsert: Boolean);');
    CurOut.Add('begin');
    CurOut.Add('  with AQuery do');
    CurOut.Add('  begin');
    CurOut.Add('    Clear;');
    CurOut.Add('    if AInsert then');
    CurOut.Add('    begin');
    CurOut.Add('      Sql.Add(''insert into ' + Name + '('');');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      if I <> lastFieldidx then
        X := ','
      else
        X := '';
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('      Sql.Add(''      ' + S + X + ''');');
      end;
    end;
    CurOut.Add('      Sql.Add('')'');');
    CurOut.Add('      Sql.Add(''values('');');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      if I <> lastFieldidx then
        X := ','
      else
        X := '';
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('      Sql.Add(''      :V_' + S + X + ''');');
      end;
    end;
    CurOut.Add('      Sql.Add('')'');');
    CurOut.Add('    end');
    CurOut.Add('    else');
    CurOut.Add('    begin');
    CurOut.Add('      Sql.Add(''update ' + Name + ''');');
    CurOut.Add('      Sql.Add(''   set'');');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      if I <> lastFieldidx then
        X := ','
      else
        X := '';
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('      Sql.Add(''      ' + S + ' = :V_' + S + X + ''');');
      end;
    end;
    CurOut.Add('      Sql.Add('' where ' + KeyFieldName + '=:V_' + KeyFieldName + ''');');
    CurOut.Add('    end;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      case f.DataType of
        cfdtInteger:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otInteger);');
        cfdtEnum:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otInteger);');
        cfdtString:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otString);');
        cfdtFloat:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otFloat);');
        cfdtDate:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otDate);');
        cfdtBool:
          CurOut.Add('    DeclareVariable(''V_' + S + ''', otInteger);');
      end;
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate:
          CurOut.Add('    SetVariable(''V_' + S + ''', F' + S + ');');
        cfdtBool, cfdtEnum:
          CurOut.Add('    SetVariable(''V_' + S + ''', Integer(F' + S + '));');
      end;
    end;
    CurOut.Add('    Execute;');
    CurOut.Add('  end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('function ' + clsName + '.GetNextSeqID(AQuery: TOracleQuery): Integer;');
    CurOut.Add('begin');
    CurOut.Add('  with AQuery do');
    CurOut.Add('  begin');
    CurOut.Add('    Clear;');
    CurOut.Add('    Sql.Text := ''select SEQ_' + Name + '.nextval from dual'';');
    CurOut.Add('    Execute;');
    CurOut.Add('    Result := FieldAsInteger(0);');
    CurOut.Add('  end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('{ ' + clsName + 'List }');
    CurOut.Add('');
    CurOut.Add('function ' + clsName + 'List.GetItem(Index: Integer): ' + clsName + ';');
    CurOut.Add('begin');
    CurOut.Add('  Result := ' + clsName + '(inherited Get(Index));');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + 'List.PutItem(Index: Integer; const Value: ' + clsName + ');');
    CurOut.Add('begin');
    CurOut.Add('  inherited Put(Index, Value);');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('function ' + clsName + 'List.CreateObj: TCtObject;');
    CurOut.Add('begin');
    CurOut.Add('  if Assigned(ItemClass) then');
    CurOut.Add('    Result := ItemClass.Create');
    CurOut.Add('  else');
    CurOut.Add('    Result := ' + clsName + '.Create;');
    CurOut.Add('  if Result.Name = '''' then');
    CurOut.Add('    Result.Name := ''New ' + clsName + ''';');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('end.');
    CurOut.Add('');
  end;
end.

