(*[SettingsPanel]
Control="Edit";Name="Author";Caption="Your name:";Value="huz"
Control="Edit";Name="EMail";Caption="Email:";Value="huzzz@163.com"
[/SettingsPanel]*)

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

function DEF_CTMETAFIELD_DATATYPE_NAMES_CPP(idx: TCtFieldDataType): string;
begin
  case Integer(idx) of
    0: Result := 'NSUnknown *';
    1: Result := 'NSString *';
    2: Result := 'int';
    3: Result := 'double';
    4: Result := 'NSDate *';
    5: Result := 'BOOL';
    6: Result := 'enum';
    7: Result := 'NSObject *';
    8: Result := 'NSObject *';
    9: Result := 'NSMutableArray *';
    10: Result := 'void';
    11: Result := 'SEL';
    12: Result := 'NSObject *';
  else
    Result := 'NSUnknown *';
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
  if ((f.DataType = cfdtOther) or (f.DataType = cfdtObject))
    and (f.DataTypeName<>'') then
    Result := f.DataTypeName
  else if f.DataType = cfdtEnum then
    Result := getPublicName(f.Name) + 'Enum'
  else
    Result := DEF_CTMETAFIELD_DATATYPE_NAMES_CPP(f.DataType);
end;

function GetCurFieldComments:String;
var
  X: string;
begin
  Result:='';
  X := F.GetFieldComments;
  if X <> '' then
  begin
    X := StringReplace(X, #13#10, ' ', [rfReplaceAll]);
    X := StringReplace(X, #13, ' ', [rfReplaceAll]);
    X := StringReplace(X, #10, ' ', [rfReplaceAll]);
    Result := '/*!'#13#10' @abstract ' + X +#13#10' */';
  end;
end;

procedure AddFieldInfo(isHeader: Boolean);
begin
  S := GetDesName(f.Name, f.DisplayName);
  if f.DataType = cfdtFunction then
  begin
    FT := f.DataTypeName;
    if FT = '' then
    begin
      FT :=Copy(S,Length(S)-4,5);
      if FT='Click' then
        FT := 'IBAction'
      else
        FT := 'void';
    end;
    S :='- (' + FT + ')' + getProtectName(S) ;
    if FT='IBAction' then
      S:=S+':(id)sender';
    if isHeader then
      S :=S+';'
    else
      S := S+#13#10
        + '{' + #13#10
        + '}';
  end
  else
  begin
    if ((f.DataType = cfdtOther) or (f.DataType = cfdtObject)) and (f.DataTypeName<>'') then
      FT := f.DataTypeName + '*'
    else if f.DataType = cfdtEnum then
      FT := GFieldType(f)
    else
      FT := DEF_CTMETAFIELD_DATATYPE_NAMES_CPP(f.DataType);
    if isHeader then
      S :='- ('+FT + ')get' + getPublicName(S) + ';'#13#10 +
        '- (void)set' + getPublicName(S) + ':(' + FT + ')value;'
    else
      S :='- ('+FT + ')get' + getPublicName(S) + ''#13#10 +
        + '{' + #13#10
        + '  return ' + getProtectName(S) + ';' + #13#10
        + '}' + #13#10
        '- (void)set' + getPublicName(S) + ':(' + FT + ')value'#13#10
        + '{' + #13#10
        + '    ' + getProtectName(S) + ' = value;' + #13#10
        + '}';
  end;

  T:=GetCurFieldComments;
  if T<>'' then
    S :=T+#13#10+S;

  if not isHeader then
    ;//S := '    ' + StringReplace(S, #13#10, #13#10'    ', [rfReplaceAll]);
  CurOut.Add(S);
end;
begin
  with CurTable do
  begin
    S := GetTableComments;
    if S = '' then
      S:=Name;
    S:=StringReplace(S, #13#10, ' ', [rfReplaceAll]);
    S := StringReplace(S, '/', '%4F', [rfReplaceAll]);
    CurOut.Add('/*!');
    CurOut.Add(' @header ' + Name+'.h');
    CurOut.Add(' @abstract ' + S);
    CurOut.Add(' @author '+GetParamValueDef('Author','User')+'('+GetParamValueDef('EMail','EMail')+')');
    CurOut.Add(' @version 1.0 '+DateTimeToStr(Now));
    CurOut.Add('');
    CurOut.Add(Describe);
    CurOut.Add('');
    CurOut.Add('*/');
    CurOut.Add('');

    CurOut.Add('/******** ' + Name + '.h ********/');
    CurOut.Add('');

    CurOut.Add('#import <Foundation/Foundation.h>');
    CurOut.Add('#import <UIKit/UIKit.h>');
    CurOut.Add('');

    L := 0;
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := GetDesName(f.Name, f.DisplayName);
      if L < Length(S) then
        L := Length(S);
    end;

    clsName := Name;

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtUnknow,
        cfdtBlob,
        cfdtObject,
        cfdtEvent,
        cfdtOther:
          begin
            if (f.DataType = cfdtOther) or (f.DataType = cfdtObject) then
            if f.DataTypeName<>'' then
            begin
              S:=StringReplace(f.DataTypeName,'*','' ,[]);
              CurOut.Add('@class ' + S + ';');
            end;
          end;
      end;
    end;
    CurOut.Add('');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtEnum:
          begin
            CurOut.Add('typedef	enum{');
            CurOut.Add('    ' + getPublicName(GFieldName(f)) + '_Unknown = 0,');
            CurOut.Add('    ' + getPublicName(GFieldName(f)) + '_Value1,');
            CurOut.Add('    ' + getPublicName(GFieldName(f)) + '_Value2');
            CurOut.Add('}' + GFieldType(f) + ';');
            CurOut.Add('');
          end;
      end;
    end;

    S := GetTableComments;
    if S <> '' then
    begin
      S := StringReplace(S, '/', '%4F', [rfReplaceAll]);
      CurOut.Add('/*! '#13#10' @abstract ' + StringReplace(S, #13#10, #13#10' * ', [rfReplaceAll]) + #13#10' */');
    end;

    CurOut.Add('@interface '+clsName+' : NSObject' );
    CurOut.Add('{');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      if f.DataType <> cfdtFunction then
      begin
        if ((f.DataType = cfdtOther) or (f.DataType = cfdtObject)) and (f.DataTypeName<>'') then
          S := f.DataTypeName +' *'
        else
          S := GFieldType(f);
        CurOut.Add('    ' + S + ' ' + getProtectName(GFieldName(f)) + ';');
      end;
    end;
    CurOut.Add('}');

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtUnknow,
        cfdtBlob,
        cfdtObject,
        cfdtDate,
        cfdtString,
        cfdtList,
        cfdtOther:
          begin
            CurOut.Add(GetCurFieldComments);
            if ((f.DataType = cfdtOther) or (f.DataType = cfdtObject)) and (f.DataTypeName<>'') then
            begin
              S := f.DataTypeName;
              if Copy(S,1,2)='UI' then
                CurOut.Add('@property (nonatomic, retain) IBOutlet ' + S + ' * '+getProtectName(GFieldName(f))+';')
              else
                CurOut.Add('@property (nonatomic, retain) ' + S + ' * '+getProtectName(GFieldName(f))+';');
            end
            else
            begin
              CurOut.Add('@property (nonatomic, retain) ' + GFieldType(f) + ' '+getProtectName(GFieldName(f))+';');
            end;
          end;
        cfdtFunction:
          begin
          end;
        else
        begin
          CurOut.Add(GetCurFieldComments);
          CurOut.Add('@property ' + GFieldType(f) + ' '+getProtectName(GFieldName(f))+';');
        end;
      end;
    end;

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction:
        begin
          CurOut.Add(GetCurFieldComments);
          AddFieldInfo(True);
        end;
      end;
    end;

    CurOut.Add('');
    CurOut.Add('- ('+clsName+' *)init;');
    CurOut.Add('- (void)dealloc;');
    CurOut.Add('- (void)reset;');
    CurOut.Add('- (void)assignFrom:(' + clsName + ' *)obj;');
    CurOut.Add('');
    CurOut.Add('@end');
    CurOut.Add('/******** END of ' + Name + '.h ********/');
    CurOut.Add('');
    CurOut.Add('');
    CurOut.Add('/******** ' + Name + '.m ********/');
    CurOut.Add('');

    CurOut.Add('#import "' + Name + '.h"');
    CurOut.Add('');

    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtUnknow,
        cfdtBlob,
        cfdtObject,
        cfdtEvent,
        cfdtOther:
          begin
            if (f.DataType = cfdtOther) or (f.DataType = cfdtObject) then
            if f.DataTypeName<>'' then
            begin
              S:=StringReplace(f.DataTypeName,'*','' ,[]);
              CurOut.Add('#import "' + S + '.h"');
            end;
          end;
      end;
    end;
                   ;
    CurOut.Add('');
    CurOut.Add('@implementation ' + clsName + '');

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction:;
        else
          begin
            CurOut.Add('@synthesize '+getProtectName(GFieldName(f))+';');
          end;
      end;
    end;

    CurOut.Add('');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtFunction:
          AddFieldInfo(False);
      end;
    end;

    CurOut.Add('');
    CurOut.Add('- ('+clsName+' *)init');
    CurOut.Add('{');
    CurOut.Add('    if(![super init])');
    CurOut.Add('      return nil;');
    CurOut.Add('    //your init code');
    CurOut.Add('    return self;');
    CurOut.Add('}');
    CurOut.Add('');
    CurOut.Add('- (void)dealloc;');
    CurOut.Add('{');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      case f.DataType of
        cfdtUnknow,
        cfdtBlob,
        cfdtObject,
        cfdtDate,
        cfdtString,
        cfdtList,
        cfdtOther:
          begin
            CurOut.Add('    ['+getProtectName(GFieldName(f))+' release];');
          end;
      end;
    end;
    CurOut.Add('    [super dealloc];');
    CurOut.Add('}');
    CurOut.Add('');

    CurOut.Add('- (void)reset');
    CurOut.Add('{');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := '';
      case f.DataType of
        cfdtInteger, cfdtFloat:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = 0;');
        cfdtString:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = nil;');
        cfdtDate:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = nil;');
        cfdtEnum:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = ' + getPublicName(GFieldName(f)) + '_Unknown;');
        cfdtBool:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = NO;');
        cfdtList:
          CurOut.Add('    [' + ExtStr(V + S, L) + ' removeAllObjects];');
        cfdtEvent:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = nil;');
        cfdtFunction: ;
      else
        CurOut.Add('    //[' + ExtStr(S, L) + ' reset];');
      end;
    end;
    CurOut.Add('}');
    CurOut.Add('');

    CurOut.Add('- (void)assignFrom:(' + clsName + ' *)obj');
    CurOut.Add('{');
    CurOut.Add('    if(obj==nil)');
    CurOut.Add('    {');
    CurOut.Add('      [self reset];');
    CurOut.Add('      return;');
    CurOut.Add('    }');
    for I := 0 to MetaFields.Count - 1 do
    begin
      f := MetaFields[I];
      S := getProtectName(GFieldName(f));
      V := ''; //RemRootCtField(f.Name);
      case f.DataType of
        cfdtString, cfdtInteger, cfdtFloat, cfdtDate, cfdtBool, cfdtEnum:
          CurOut.Add('    ' + ExtStr(V + S, L) + ' = obj->' + S + ';');
        cfdtFunction, cfdtEvent: ;
      else
        CurOut.Add('    //' + ExtStr(S, L) + ' -> assignFrom(obj->' + S + ');');
      end;
    end;
    CurOut.Add('}');
    CurOut.Add('');
  end;
  
  CurOut.Add('');
  CurOut.Add('@end');
  CurOut.Add('');

end.
