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
  S := ExtStr(GetDesName(f.Name, f.DisplayName), L - 1);
  FT := GFieldType(f);
  if f.DataType = cfdtFunction then
  begin
    FT := f.DataTypeName;
    if (FT <> '') and (Pos('(', FT) = 0) then
      FT := '(): ' + FT;
    S := ExtStr('function ', 8) + S + ' ' + FT + ';';
  end
  else
    S := ExtStr('property ', 8) + S + ': ' + ExtStr(FT, 12)
      + ' read F' + S + ' write F' + S + ';';


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
    CurOut.Add('  ###Pascal Code Generate###');
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
    CurOut.Add('  Classes, Windows, SysUtils, Variants, Graphics, Controls, IniFiles;');
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
    if L < 12 then
      L := 12;

    clsName := 'T' + Name;

    CurOut.Add('  ' + clsName + ' = class(TObject)');
    CurOut.Add('  private');
    CurOut.Add('  protected');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction: ;
      else
        CurOut.Add('    F' + ExtStr(GFieldName(f), L - 1) + ' : ' + GFieldType(f) + ';');
      end;
    end;
    CurOut.Add('    F' + ExtStr('DataModified', L - 1) + ' : Boolean;');
    CurOut.Add('  public');
    CurOut.Add('    constructor Create; virtual;');
    CurOut.Add('    destructor Destroy; override;');
    CurOut.Add('');

    CurOut.Add('    procedure Reset; virtual;');
    CurOut.Add('    procedure AssignFrom(AObj: ' + clsName + '); virtual;');
    CurOut.Add('    procedure LoadFromIni(AIni: TCustomIniFile; ASec: string); virtual;');
    CurOut.Add('    procedure SaveToIni(AIni: TCustomIniFile; ASec: string); virtual;');
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
    CurOut.Add('    //数据是否被修改');
    CurOut.Add('    property ' + ExtStr('DataModified', L - 1) + ': ' + ExtStr('Boolean', 12) +
      ' read F' + ExtStr('DataModified', L - 1) +
      ' write F' + ExtStr('DataModified', L - 1) + ';');
    CurOut.Add('');

    CurOut.Add('  end;');
    CurOut.Add('');

    CurOut.Add('  ' + clsName + 'List = class(TList)');
    CurOut.Add('  private');
    CurOut.Add('    function GetItem(Index: Integer): ' + clsName + ';');
    CurOut.Add('    procedure PutItem(Index: Integer; const Value: ' + clsName + ');');
    CurOut.Add('  protected');
    CurOut.Add('    procedure Notify(Ptr: Pointer; Action: TListNotification); override;');
    CurOut.Add('    function GetDataModified: Boolean;');
    CurOut.Add('    procedure SetDataModified(const Value: Boolean);');
    CurOut.Add('  public');
    CurOut.Add('    function NewItem: ' + clsName + '; virtual;');
    CurOut.Add('    property Items[Index: Integer]: ' + clsName + ' read GetItem write PutItem; default;');
    CurOut.Add('    property DataModified: Boolean read GetDataModified write SetDataModified;');
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
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('destructor ' + clsName + '.Destroy;');
    CurOut.Add('begin');
    CurOut.Add('  inherited;');
    CurOut.Add('end;');
    CurOut.Add('');



    CurOut.Add('procedure ' + clsName + '.Reset;');
    CurOut.Add('begin');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      V := '';
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
    CurOut.Add('  ' + ExtStr('FDataModified', L) + ' := False;');
    CurOut.Add('end;');
    CurOut.Add('');


    CurOut.Add('procedure ' + clsName + '.AssignFrom(AObj: ' + clsName + ');');
    CurOut.Add('begin');
    CurOut.Add('  if not Assigned(AObj) then');
    CurOut.Add('  begin');
    CurOut.Add('    Reset;');
    CurOut.Add('  end;');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      V := ''; //RemRootCtField(f.Name);
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('  ' + ExtStr(V + 'F' + GFieldName(f), L) + ' := AObj.F' + GFieldName(f) + ';');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('  F' + ExtStr(GFieldName(f), L - 1) + ' .  AssignFrom(AObj.F' + GFieldName(f) + ');');
      end;
    end;
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.LoadFromIni(AIni: TCustomIniFile; ASec: string);');
    CurOut.Add('begin');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      V := ExtStr('F' + S, L) + ' := ';
      case f.DataType of
        cfdtInteger:
          CurOut.Add('  ' + V + 'AIni.ReadInteger(ASec, ''' + S + ''', F' + S + ');');
        cfdtEnum:
          CurOut.Add('  ' + V + GFieldType(f) + '(AIni.ReadInteger(ASec, ''' + S + ''', Integer(F' + S + ')));');
        cfdtString:
          CurOut.Add('  ' + V + 'AIni.ReadString(ASec, ''' + S + ''', F' + S + ');');
        cfdtFloat:
          CurOut.Add('  ' + V + 'AIni.ReadFloat(ASec, ''' + S + ''', F' + S + ');');
        cfdtDate:
          CurOut.Add('  ' + V + 'AIni.ReadDateTime(ASec, ''' + S + ''', F' + S + ');');
        cfdtBool:
          CurOut.Add('  ' + V + 'AIni.ReadBool(ASec, ''' + S + ''', F' + S + ');');
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
        CurOut.Add('  F' + ExtStr(GFieldName(f), L - 1) + ' .  LoadFromIni(AIni, ASec + ''' + GFieldName(f) + ''');');
      end;
    end;
    CurOut.Add('  ' + ExtStr('FDataModified', L) + ' := False;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + '.SaveToIni(AIni: TCustomIniFile; ASec: string);');
    CurOut.Add('begin');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GFieldName(f);
      V := '';
      case f.DataType of
        cfdtInteger:
          CurOut.Add('  ' + V + 'AIni.WriteInteger(ASec, ''' + S + ''', F' + S + ');');
        cfdtEnum:
          CurOut.Add('  ' + V + 'AIni.WriteInteger(ASec, ''' + S + ''', Integer(F' + S + '));');
        cfdtString:
          CurOut.Add('  ' + V + 'AIni.WriteString(ASec, ''' + S + ''', F' + S + ');');
        cfdtFloat:
          CurOut.Add('  ' + V + 'AIni.WriteFloat(ASec, ''' + S + ''', F' + S + ');');
        cfdtDate:
          CurOut.Add('  ' + V + 'AIni.WriteDateTime(ASec, ''' + S + ''', F' + S + ');');
        cfdtBool:
          CurOut.Add('  ' + V + 'AIni.WriteBool(ASec, ''' + S + ''', F' + S + ');');
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
        CurOut.Add('  F' + ExtStr(GFieldName(f), L - 1) + ' .  SaveToIni(AIni, ASec + ''' + GFieldName(f) + ''');');
      end;
    end;
    CurOut.Add('  ' + ExtStr('FDataModified', L) + ' := False;');
    CurOut.Add('end;');
    CurOut.Add('');



    CurOut.Add('{ ' + clsName + 'List }');
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

    CurOut.Add('function ' + clsName + 'List.NewItem: ' + clsName + ';');
    CurOut.Add('begin');
    CurOut.Add('  Result := ' + clsName + '.Create;');
    CurOut.Add('  Add(Result);');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + 'List.Notify(Ptr: Pointer; Action: TListNotification);');
    CurOut.Add('begin');
    CurOut.Add('  inherited;');
    CurOut.Add('  if Action = lnDeleted then');
    CurOut.Add('    if Assigned(Ptr) then');
    CurOut.Add('      TObject(Ptr).Free;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('function ' + clsName + 'List.GetDataModified: Boolean;');
    CurOut.Add('var');
    CurOut.Add('  I: Integer;');
    CurOut.Add('begin');
    CurOut.Add('  Result := False;');
    CurOut.Add('  for I := 0 to Count - 1 do');
    CurOut.Add('    if Items[I].DataModified then');
    CurOut.Add('    begin');
    CurOut.Add('      Result := True;');
    CurOut.Add('      Exit;');
    CurOut.Add('    end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('procedure ' + clsName + 'List.SetDataModified(const Value: Boolean);');
    CurOut.Add('var');
    CurOut.Add('  I: Integer;');
    CurOut.Add('begin');
    CurOut.Add('  if not Value then');
    CurOut.Add('    for I := 0 to Count - 1 do');
    CurOut.Add('      if Items[I].DataModified then');
    CurOut.Add('      begin');
    CurOut.Add('        Items[I].DataModified := False;');
    CurOut.Add('      end;');
    CurOut.Add('end;');
    CurOut.Add('');

    CurOut.Add('end.');
    CurOut.Add('');
  end;

end.

