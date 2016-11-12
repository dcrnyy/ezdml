// Direct Oracle Access - TOracleProvider component
// Based on Borland's TBaseProvider
// Copyright 1998 - 1999 Allround Automations
// support@allroundautomations.nl
// http://www.allroundautomations.nl

{$I Oracle.inc}

{$IFNDEF CompilerVersion5}

unit OracleProvider;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Provider, DB, DBClient, BDEProv, DsIntf, DsgnIntf, OracleData;

type
  // TOraclePacketDataSet
  TOraclePacketDataSet = class(TClientDataSet)
  private
    FOldRecBuf: PChar;
    FCurRecBuf: PChar;
    FUseCurValues: Boolean;
    FNewValuesModified: Boolean;
  protected
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    function  GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    {$IFDEF CompilerVersion4}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField;
      const Value: Variant); override;
    {$ELSE}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField;
      Value: Variant); override;
    {$ENDIF}
  public
    procedure CreateFromDelta(DataSet: TClientDataSet);
    function  HasCurValues: Boolean;
    procedure InitAltRecBuffers;
    procedure InitCurValues(const CurValues: Variant);
    function  HasMergeConflicts: Boolean;
    function  UpdateKind: TUpdateKind;
    property  NewValuesModified: Boolean read FNewValuesModified;
    property  UseCurValues: Boolean read FUseCurValues write FUseCurValues;
  end;
  // TOracleProvider
  TResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);
  TProviderDataEvent = procedure(DataSet: TClientDataSet) of object;
  TUpdateRecordEvent = procedure(DataSet: TClientDataSet;
    UpdateKind: TUpdateKind; var Applied: Boolean) of object;
  TResolverErrorEvent = procedure(DataSet: TClientDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var Response: TResolverResponse) of object;
  TOracleProvider = class(TCustomProvider)
  private
    FFetchFirst: Boolean;
    FPacketData: TOraclePacketDataSet;
    FDeltaData: TOraclePacketDataSet;
    FResultData: TOraclePacketDataSet;
    FPrevResponse: TResolverResponse;
    FIncludeMetaData: Boolean;
    FDoInitialize: Boolean;
    FOnGetData: TProviderDataEvent;
    FOnUpdateData: TProviderDataEvent;
    FOnUpdateError: TResolverErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    HeaderSize: Integer;
    MetaDataSize: Integer;
    FDataSet: TOracleDataSet;
    FFirstRecord: Boolean;
    FUniqueFields: string;
    FUsedUniqueFields: string;
    FReset: Boolean;
  protected
    procedure StartTransaction; virtual;
    procedure EndTransaction(Commit: Boolean); virtual;
    procedure InitFieldDefs; virtual;
    procedure InternalAfterCreate; virtual;
    procedure InternalAbortUpdate(DeltaData: TOraclePacketDataSet; E: EDatabaseError;
      var MaxErrors, ErrorCount: Integer; Response: TResolverResponse);
    procedure InternalAfterApplyUpdates(DeltaData, ResultData: TOraclePacketDataSet;
      MaxErrors, ErrorCount: Integer); virtual;
    procedure InternalConnect; virtual;
    procedure InternalReset(MetaData: Boolean); virtual;
    procedure InitializeData; virtual;
    procedure SetParamByName(ParamName: String; Value: OleVariant); virtual;
    procedure SetParamByIndex(ParamIndex: Integer; Value: OleVariant); virtual;
    procedure InternalDoUpdate(DeltaData: TOraclePacketDataSet; UpdateKind: TUpdateKind); virtual;
    procedure InternalInitUpdate(DeltaData: TOraclePacketDataSet; UpdateKind: TUpdateKind); virtual;
    procedure GetUniqueFieldList(List: TStrings);
    function  FindRecord(DeltaData: TOraclePacketDataSet): Boolean;
    procedure DoUpdate(DeltaData: TOraclePacketDataSet); virtual;
    function  HandleUpdateError(DeltaData: TOraclePacketDataSet; E: EDatabaseError;
      var MaxErrors, ErrorCount: Integer): Boolean; virtual;
    procedure InitializeConflictBuffer(DeltaData: TOraclePacketDataSet); virtual;
    procedure LogUpdateError(DeltaData: TOraclePacketDataSet; E: EDatabaseError;
      Response: TResolverResponse); virtual;
    function  PackageRecords(Count: Integer): Integer; virtual;
    function  GetNextRecord: Boolean; virtual;
    procedure AssignFieldData(Field: TField); virtual;
    function  GetParamCount: Integer; virtual;
    function  GetConnected: Boolean; virtual;
    procedure SetConnected(Value: Boolean); virtual;
    procedure SetDataSet(ADataSet: TOracleDataSet);
    function  GetUsedUniqueFields: string;
    property  FetchFirst: Boolean read FFetchFirst;
    property  ParamCount: Integer read GetParamCount;
    property  PacketData: TOraclePacketDataSet read FPacketData;
    property  Connected: Boolean read GetConnected write SetConnected;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF CompilerVersion4}
    function ApplyUpdates(var Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; override;
    {$ELSE}
    function ApplyUpdates(Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; override;
    {$ENDIF}
    function  GetRecords(Count: Integer; out RecsOut: Integer): OleVariant; override;
    procedure Reset(MetaData: WordBool); override;
    procedure SetParams(Values: OleVariant); override;
    property  Provider;
    property  UsedUniqueFields: string read GetUsedUniqueFields;
  published
    property DataSet: TOracleDataSet read FDataSet write SetDataSet;
    property UniqueFields: String read FUniqueFields write FUniqueFields;
    property OnDataRequest;
    property OnGetData: TProviderDataEvent read FOnGetData write FOnGetData;
    property OnUpdateData: TProviderDataEvent read FOnUpdateData write FOnUpdateData;
    property OnUpdateError: TResolverErrorEvent read FOnUpdateError write FOnUpdateError;
//    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;
  // Component editor
  TOracleProviderEditor = class(TComponentEditor)
  private
    FEditor: TComponentEditor;
  public
    {$IFDEF CompilerVersion4}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
    {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: TFormDesigner); override;
    {$ENDIF}
    destructor  Destroy; override;
    procedure Edit; override;
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;

const
  OracleProviderEditorClass: TComponentEditorClass = nil;

procedure Register;

implementation

const
  SNoContraints    = 'Constraints not supported';
  SInvalidResponse = 'Invalid response';
  UpdateFailed  = 1;
  UpdateSkipped = 2;
  DBIMAXMSGLEN           = 127;  // Max message len (from BDE unit)
  dspropAUTOINC_DISABLED = 6;    // rw BOOL, Autoinc disabled
  dspropDONTINCLMETADATA = 8;    // rw BOOL, if TRUE do not inc. metadata in StreamDS

// TOraclePacketDataSet

procedure TOraclePacketDataSet.InternalInitRecord(Buffer: PChar);
var I: Integer;
begin
  inherited InternalInitRecord(Buffer);
  // Initialize new records in the error result dataset to unchanged values
  for I := 1 to FieldCount do
    DSBase.PutBlank(PByte(Buffer), 0, I, BLANK_NOTCHANGED);
end;

procedure TOraclePacketDataSet.CreateFromDelta(DataSet: TClientDataSet);
var I: Integer;
begin
  // Create a result dataset given a delta dataset, adding addition fields for
  // the error information as required for the reconcile method
  Data := Null;
  FieldDefs.Clear;
  FieldDefs.Add('ERROR_RECORDNO', ftInteger, 0, False);
  FieldDefs.Add('ERROR_RESPONSE', ftInteger, 0, False);
  FieldDefs.Add('ERROR_MESSAGE',  ftString,  DBIMAXMSGLEN, False);
  FieldDefs.Add('ERROR_CONTEXT',  ftString,  DBIMAXMSGLEN, False);
  FieldDefs.Add('ERROR_CATEGORY', ftInteger, 0, False);
  FieldDefs.Add('ERROR_CODE',     ftInteger, 0, False);
  for I := 0 to DataSet.FieldDefs.Count - 1 do
    with DataSet.FieldDefs[I] do FieldDefs.Add(Name, DataType, Size, False);
  CreateDataSet;
  LogChanges := False;
  DSBase.SetProp(DSProp(dspropAUTOINC_DISABLED), Integer(True));
end;

procedure TOraclePacketDataSet.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if Event = deDataSetScroll then FNewValuesModified := False;
  inherited DataEvent(Event, Info);
end;

procedure TOraclePacketDataSet.InternalOpen;
begin
  inherited InternalOpen;
  FOldRecBuf := AllocRecordBuffer;
end;

procedure TOraclePacketDataSet.InternalClose;
begin
  inherited InternalClose;
  {$IFDEF CompilerVersion4}
  if FOldRecBuf <> nil then FreeMem(FOldRecBuf);
  FOldRecBuf := nil;
  if FCurRecBuf <> nil then FreeMem(FCurRecBuf);
  FCurRecBuf := nil;
  {$ELSE}
  StrDispose(FOldRecBuf);
  FOldRecBuf := nil;
  StrDispose(FCurRecBuf);
  FCurRecBuf := nil;
  {$ENDIF}
end;

function TOraclePacketDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  // When reading an OldValue, return the CurValue instead if we have one
  if FUseCurValues and (State = dsOldValue) and HasCurValues then
  begin
    Result := inherited GetStateFieldValue(dsCurValue, Field);
    if not VarIsEmpty(Result) then Exit;
  end;
  Result := inherited GetStateFieldValue(State, Field);
end;

{$IFDEF CompilerVersion4}
procedure TOraclePacketDataSet.SetStateFieldValue(State: TDataSetState; Field: TField;
  const Value: Variant);
{$ELSE}
procedure TOraclePacketDataSet.SetStateFieldValue(State: TDataSetState; Field: TField;
  Value: Variant);
{$ENDIF}
begin
  // Set a flag when any of the field's NewValue properties are modified
  if State = dsNewValue then FNewValuesModified := True;
  inherited SetStateFieldValue(State, Field, Value);
end;

function TOraclePacketDataSet.UpdateKind: TUpdateKind;
begin
  case UpdateStatus of
    usInserted: Result := ukInsert;
    usDeleted: Result := ukDelete;
  else
    Result := ukModify;
  end;
end;

function TOraclePacketDataSet.HasCurValues: Boolean;
begin
  Result := FCurRecBuf <> nil;
end;

procedure TOraclePacketDataSet.InitAltRecBuffers;
begin
  if UpdateStatus in [usUnmodified, usDeleted] then GetCurrentRecord(FOldRecBuf);
  if UpdateStatus = usUnmodified then Next;
  if UpdateStatus = usInserted then
    SetAltRecBuffers(nil, ActiveBuffer, FCurRecBuf)
  else
    SetAltRecBuffers(FOldRecBuf, ActiveBuffer, FCurRecBuf);
end;

procedure TOraclePacketDataSet.InitCurValues(const CurValues: Variant);
var I: Integer;
    CurVal: Variant;
    F: TField;
begin
  if VarIsArray(CurValues) then
  begin
    Insert;
    try
      for I := VarArrayLowBound(CurValues, 1) to VarArrayHighBound(CurValues, 1) do
      begin
        if VarIsArray(CurValues[I]) then
        begin
          CurVal := CurValues[I][1];
          F := FieldByName(CurValues[I][0])
        end else begin
          CurVal := CurValues[I];
          F := Fields[I];
        end;
        if not VarIsEmpty(CurVal) then
          if (F.OldValue <> CurVal) then Fields[I].Value := CurVal;
      end;
      FCurRecBuf := AllocRecordBuffer;
      Move(ActiveBuffer^, FCurRecBuf^, RecordSize);
    finally
      Cancel;
    end;
  end else begin
    {$IFDEF CompilerVersion4}
    if FCurRecBuf <> nil then FreeMem(FCurRecBuf);
    FCurRecBuf := nil;
    {$ELSE}
    StrDispose(FCurRecBuf);
    FCurRecBuf := nil;
    {$ENDIF}
    FUseCurValues := False;
  end;
  InitAltRecBuffers;
end;

function TOraclePacketDataSet.HasMergeConflicts: Boolean;
var I: Integer;
    CurVal, NewVal: Variant;
begin
  Result := False;
  for I := 0 to FieldCount - 1 do with Fields[I] do
  begin
    CurVal := CurValue;
    if VarIsEmpty(CurVal) then Continue;
    NewVal := NewValue;
    if VarIsEmpty(NewVal) then Continue;
    if CurVal = NewVal then Continue;
    Result := True;
    Break;
  end;
end;

// Component Editor

{$IFDEF CompilerVersion4}
constructor TOracleProviderEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
{$ELSE}
constructor TOracleProviderEditor.Create(AComponent: TComponent; ADesigner: TFormDesigner);
{$ENDIF}
begin
  inherited Create(AComponent, ADesigner);
  FEditor := OracleProviderEditorClass.Create(AComponent, ADesigner);
end;

destructor TOracleProviderEditor.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited Destroy
end;

function TOracleProviderEditor.GetVerbCount: Integer;
begin
  Result := FEditor.GetVerbCount;
end;

function TOracleProviderEditor.GetVerb(Index: Integer): string;
begin
  Result := FEditor.GetVerb(Index);
end;

procedure TOracleProviderEditor.ExecuteVerb(Index: Integer);
begin
  FEditor.ExecuteVerb(Index);
end;

procedure TOracleProviderEditor.Edit;
begin
  ExecuteVerb(1);
end;

// TOracleProvider

constructor TOracleProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPacketData := TOraclePacketDataSet.Create(Self);
  FDeltaData := TOraclePacketDataSet.Create(Self);
  FResultData := TOraclePacketDataSet.Create(Self);
  Constraints := False;
  FIncludeMetaData := True;
  FDoInitialize := True;
  FFetchFirst := True;
end;

procedure TOracleProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataSet) then FDataSet := nil;
end;

function TOracleProvider.GetParamCount: Integer;
begin
  if FDataSet <> nil then Result := FDataSet.Variables.Count else Result := 0;
end;

function TOracleProvider.GetConnected: Boolean;
begin
  Result := FDataSet.Active and not FReset;
end;

procedure TOracleProvider.SetConnected(Value: Boolean);
begin
  if Value = Connected then Exit;
  if Value then InternalConnect;
end;

procedure TOracleProvider.SetDataSet(ADataSet: TOracledataSet);
begin
  FDataSet := ADataSet;
  if FDataSet <> nil then
  begin
    FDataSet.FreeNotification(Self);
    FDataSet.CommitOnPost := False;
    FDataSet.OracleDictionary.EnforceConstraints := False;
    FDataSet.QueryAllRecords := False;
    FDataSet.LockingMode := lmLockDelayed;
  end;
end;

function TOracleProvider.GetUsedUniqueFields: string;
var i: Integer;
    List: TStringList;
begin
  if FUniqueFields <> '' then
    Result := FUniqueFields
  else begin
    if FUsedUniqueFields = '' then
    begin
      List := TStringList.Create;
      FDataSet.GetIdColumns(List);
      for i := 0 to List.Count - 1 do
      begin
        if i > 0 then FUsedUniqueFields := FUsedUniqueFields + ';';
        FUsedUniqueFields := FUsedUniqueFields + List[i];
      end;
      List.Free;
    end;
    Result := FUsedUniqueFields;
  end;
end;

// Data Retrieval

procedure TOracleProvider.InternalConnect;
begin
  if not FDataSet.Active then
  begin
    FUsedUniqueFields := '';
    FDataSet.Open;
  end else
    if FReset then FDataSet.Refresh;
  FReset := False;
end;

procedure TOracleProvider.InternalReset(MetaData: Boolean);
begin
  FReset := True;
end;

procedure TOracleProvider.InternalAfterCreate;
begin
end;

procedure TOracleProvider.InitializeData;
begin
  FDataSet.First;
  FFirstRecord := True;
end;

procedure TOracleProvider.GetUniqueFieldList(List: TStrings);
var i: Integer;
    TempStr: String;
begin
  List.Clear;
  TempStr := UsedUniqueFields;
  repeat
    i := Pos(';', TempStr);
    if i > 0 then TempStr[i] := ',' else break;
  until False;
  List.CommaText := TempStr;
end;

function TOracleProvider.FindRecord(DeltaData: TOraclePacketDataSet): Boolean;
var i: Integer;
    KeyValues: Variant;
    FieldList: TStringList;
begin
  Result := False;
  FieldList := TStringList.Create;
  try
    GetUniqueFieldList(FieldList);
    if FieldList.Count = 0 then
      raise Exception.Create('No unique fields to identify records');
    if FieldList.Count = 1 then
      Result := FDataSet.Locate(UsedUniqueFields, DeltaData.FieldByName(FieldList[0]).OldValue, [])
    else begin
      KeyValues := VarArrayCreate([0, FieldList.Count - 1], varVariant);
      for i := 0 to FieldList.Count - 1 do
        KeyValues[i] := DeltaData.FieldByName(FieldList[i]).OldValue;
      Result := FDataSet.Locate(UsedUniqueFields, KeyValues, []);
    end;
  finally
    FieldList.Free;
  end;
end;

procedure TOracleProvider.InitializeConflictBuffer(DeltaData: TOraclePacketDataSet);
var i: Integer;
    CurValues: Variant;
    FieldName: String;
begin
  // Set the conflict buffer to the current values of the data
  DeltaData.InitCurValues(Null);
  if not FindRecord(DeltaData) then Exit;
  FDataSet.Resync([]);
  CurValues := VarArrayCreate([0, DeltaData.FieldCount-1], varVariant);
  for i := 0 to DeltaData.FieldCount - 1 do
  begin
    FieldName := DeltaData.Fields[i].FieldName;
    CurValues[i] := VarArrayOf([FieldName, FDataSet.FieldByName(FieldName).Value]);
  end;
  DeltaData.InitCurValues(CurValues);
end;

procedure TOracleProvider.InternalDoUpdate(DeltaData: TOraclePacketDataSet; UpdateKind: TUpdateKind);
var i: Integer;
    Field: TField;
begin
  case UpdateKind of
    ukInsert:
    begin
      FDataSet.Append;
      try
        for i := 0 to DeltaData.FieldCount - 1 do
        begin
          Field := FDataSet.FindField(DeltaData.Fields[i].FieldName);
          if Field <> nil then Field.Assign(DeltaData.Fields[i]);
        end;
        FDataSet.Post;
      except
        FDataSet.Cancel;
        raise;
      end;
    end;
    ukDelete:
      if FindRecord(DeltaData) then
        FDataSet.Delete
      else
        DatabaseError('Cannot find record');
    ukModify:
    begin
      if not FindRecord(DeltaData) then DatabaseError('Cannot find record');
      FDataSet.Edit;
      try
        for i := 0 to DeltaData.FieldCount - 1 do
          if not VarIsEmpty(DeltaData.Fields[i].NewValue) then
          begin
            Field := FDataSet.FindField(DeltaData.Fields[i].FieldName);
            if Field <> nil then Field.Assign(DeltaData.Fields[i]);
          end;
        FDataSet.Post;
      except
        FDataSet.Cancel;
        raise;
      end;
    end;
  end;
end;

(*
procedure TOracleProvider.InitFieldDefs;
begin
  PacketData.FieldDefs.Assign(FDataSet.FieldDefs);
end;
*)
procedure TOracleProvider.InitFieldDefs;
var
  i : Integer;
  oDataType : TFieldType;
begin
  with FDataset.FieldDefs do for i := 0 to Count -1 do with Items[i] do
  begin
    if (DataType = ftString) and (Size > 255) then
      oDataType := ftMemo
    else
      oDataType := DataType;
    PacketData.FieldDefs.Add(Name, oDataType, Size, Required);
  end;
end;

procedure TOracleProvider.SetParamByName(ParamName: String; Value: OleVariant);
begin
  FDataSet.SetVariable(ParamName, Value);
end;

procedure TOracleProvider.SetParamByIndex(ParamIndex: Integer; Value: OleVariant);
begin
  FDataSet.SetVariable(FDataSet.Variables.Data(ParamIndex).Name, Value);
end;

procedure TOracleProvider.Reset(MetaData: WordBool);
begin
  InternalReset(MetaData);
  FIncludeMetaData := MetaData;
  if MetaData then
    FPacketData.Data := Null
  else
    if FPacketData.Active then FPacketData.EmptyDataset;    {This line by JMR}
  FDoInitialize := True;
  FFetchFirst := True;
end;

procedure TOracleProvider.SetParams(Values: OleVariant);
var I, MaxIndex: Integer;
begin
  if not VarIsNull(Values) and not VarIsArray(Values) then Values := VarArrayOf([Values]);
  if not VarIsNull(Values) then  {this line by JMR}
  begin
    MaxIndex := VarArrayHighBound(Values, 1);
    if MaxIndex >= ParamCount then MaxIndex := ParamCount - 1;
    for I := 0 to MaxIndex do
      if VarIsArray(Values[I]) then
        SetParamByName(Values[I][0], Values[I][1])
      else
        SetParamByIndex(i, Values[I]);
  end;
//  Reset(False);
end;

function TOracleProvider.PackageRecords(Count: Integer): Integer;
var I: Integer;
begin
  Result := 0;
  if Count = -1 then Count := High(Integer);
  while (Result < Count) and GetNextRecord do
  begin
    FPacketData.Append;
    for I := 0 to FPacketData.FieldCount - 1 do
      AssignFieldData(FPacketData.Fields[I]);
    FPacketData.Post;
    Inc(Result);
    FFetchFirst := False;
  end;
end;

function TOracleProvider.GetNextRecord: Boolean;
begin
  // Notification to go to the next record.
  // Return False if you have reached the end of the data.
  if not FFirstRecord then FDataSet.Next else FFirstRecord := False;
  Result := not FDataSet.EOF;
end;

procedure TOracleProvider.AssignFieldData(Field: TField);
begin
  // Assign data to the field.
  Field.Assign(FDataSet.FieldByName(Field.FieldName));
end;

function TOracleProvider.GetRecords(Count: Integer; out RecsOut: Integer): OleVariant;
var S, D: Pointer;
    i: Integer;
begin
  if not Connected then Connected := True;
  if FDoInitialize then
  begin
    InitializeData;
    FDoInitialize := False;
  end;
  if Constraints then DatabaseError(SNoContraints);
  try
    if not FPacketData.Active then
    begin
      FPacketData.FieldDefs.Clear;
      InitFieldDefs;
      FPacketData.CreateDataSet;
      HeaderSize := FPacketData.Data[8];
      MetaDataSize := VarArrayHighBound(FPacketData.Data, 1) - (HeaderSize - 1);
      FPacketData.LogChanges := False;
      InternalAfterCreate;
    end;
    if (Count <> 0) then
      RecsOut := PackageRecords(Count)
    else
      RecsOut := 0;
//    if not FPacketData.Active then Log('NOT Active');
//    FPacketData.StreamMetaData := FIncludeMetaData or (Count = 0);
//    Log('FPacketData.IntStreamMetaData: '  + IntToStr(FPacketData.IntStreamMetaData));
    if Assigned(FOnGetData) then FOnGetData(FPacketData);
    Result := FPacketData.Data;
    if not (FIncludeMetaData or (Count = 0)) then
    begin
      Result[18] := 0;
      S := VarArrayLock(Result);
      D := Pointer(Integer(S) + HeaderSize);
      S := Pointer(Integer(S) + HeaderSize + MetaDataSize);
      i := 1 + VarArrayHighBound(Result,1) - (HeaderSize + MetaDataSize);
      Move(S^, D^, i);
      VarArrayUnlock(Result);
      VarArrayRedim(Result, VarArrayHighBound(Result,1) - MetaDataSize);
//      for i := MetaDataSize+1 to VarArrayHighBound(Result,1) do
//      begin
//        Log(IntToStr(i)+' '+IntToStr(Result[i]));
//        Result[i-MetaDataSize]:=Result[i];
//      end;
    end;

    if (RecsOut <> Count) then
      Reset(False)
    else begin
      FPacketData.EmptyDataset;
      FIncludeMetaData := False;
    end;
  except
    Reset(True);
    raise;
  end;
end;

// Update Error Handling

procedure TOracleProvider.LogUpdateError(DeltaData: TOraclePacketDataSet;
  E: EDatabaseError; Response: TResolverResponse);
var I: Integer;
    CurVal: Variant;
begin
  if not FResultData.Active then FResultData.CreateFromDelta(DeltaData);
  FResultData.Append;
  FResultData.SetFields([DeltaData.RecNo, Ord(Response)+1, E.Message, '',
    UpdateFailed, UpdateFailed]);
  if DeltaData.HasCurValues then
    for I := 0 to DeltaData.FieldCount - 1 do
    begin
      CurVal := DeltaData.Fields[I].CurValue;
      if not VarIsEmpty(CurVal) then
        FResultData.FieldByName(DeltaData.Fields[I].FieldName).Value := CurVal;
    end;
  FResultData.Post;
end;

procedure TOracleProvider.InternalAbortUpdate(DeltaData: TOraclePacketDataSet; E: EDatabaseError;
  var MaxErrors, ErrorCount: Integer; Response: TResolverResponse);
begin
  MaxErrors := ErrorCount - 1;
end;

function TOracleProvider.HandleUpdateError(DeltaData: TOraclePacketDataSet;
  E: EDatabaseError; var MaxErrors, ErrorCount: Integer): Boolean;
var Response: TResolverResponse;
    UpdateKind: TUpdateKind;
begin
  UpdateKind := DeltaData.UpdateKind;
  if ErrorCount < MaxErrors then Response := rrSkip else Response := rrAbort;
  InitializeConflictBuffer(DeltaData);
  if Assigned(OnUpdateError) then
    OnUpdateError(DeltaData, E, UpdateKind, Response);
  if Response in [rrSkip, rrAbort] then
  begin
    Inc(ErrorCount);
    if ErrorCount > MaxErrors then Response := rrAbort;
    if (Response = rrAbort) then
      InternalAbortUpdate(DeltaData, E, MaxErrors, ErrorCount, Response);
    if Response in [rrSkip, rrAbort] then
      LogUpdateError(DeltaData, E, Response);
  end;
  FPrevResponse := Response;
  Result := Response in [rrMerge, rrApply];
end;

// Updates

procedure TOracleProvider.InternalInitUpdate(DeltaData: TOraclePacketDataSet;
  UpdateKind: TUpdateKind);
begin
end;

procedure TOracleProvider.DoUpdate(DeltaData: TOraclePacketDataSet);
var RecNoSave: Integer;
    Applied: Boolean;
    UpdateKind: TUpdateKind;
begin
  UpdateKind := DeltaData.UpdateKind;
  if ((UpdateKind = ukInsert) and (FPrevResponse in [rrMerge, rrApply])) or
     ((FPrevResponse = rrMerge) and DeltaData.HasMergeConflicts) then
    DatabaseError(SInvalidResponse);
  DeltaData.InitAltRecBuffers;
  InternalInitUpdate(DeltaData, UpdateKind);
  Applied := False;
  DeltaData.UseCurValues := False;
  RecNoSave := DeltaData.RecNo;
  try
    if Assigned(FOnUpdateRecord) then
      FOnUpdateRecord(DeltaData, UpdateKind, Applied);
  finally
    if DeltaData.RecNo <> RecNoSave then DeltaData.RecNo := RecNoSave;
  end;
  if not Applied then InternalDoUpdate(DeltaData, UpdateKind);
end;

procedure TOracleProvider.StartTransaction;
begin
  FDataSet.StartUpdates;
  FDataSet.CommitOnPost := False;
end;

procedure TOracleProvider.EndTransaction(Commit: Boolean);
begin
  if Commit then
  begin
    FDataSet.Session.Commit;
    FDataSet.CommitUpdates;
  end else begin
    FDataSet.Session.Rollback;
    FDataSet.CancelUpdates;
  end;
end;

procedure TOracleProvider.InternalAfterApplyUpdates(DeltaData, ResultData: TOraclePacketDataSet;
  MaxErrors, ErrorCount: Integer);
begin
end;

{$IFDEF CompilerVersion4}
function TOracleProvider.ApplyUpdates(var Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
{$ELSE}
function TOracleProvider.ApplyUpdates(Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
{$ENDIF}
begin
//  if not Connected then Connected := True;
  ErrorCount := 0;
  FDeltaData.Data := Delta;
  try
    if Assigned(OnUpdateData) then OnUpdateData(FDeltaData);
    FDeltaData.First;
    if MaxErrors = -1 then MaxErrors := MaxInt;
    FPrevResponse := rrSkip;
    StartTransaction;
    while (ErrorCount <= MaxErrors) and not FDeltaData.EOF do
    begin
      try
        DoUpdate(FDeltaData);
      except
        on E: EDatabaseError do
        begin
          if HandleUpdateError(FDeltaData, E, MaxErrors, ErrorCount) then
          begin
            FDeltaData.UseCurValues := True;
            Continue;
          end;
        end;
      end;
      FPrevResponse := rrSkip;
      FDeltaData.Next;
    end;
    InternalAfterApplyUpdates(FDeltaData, FResultData, MaxErrors, ErrorCount);
    if FResultData.Active then
      Result := FResultData.Data else Result := Null;
  finally
    FDeltaData.Data  := Null;
    FResultData.Data := Null;
    EndTransaction(ErrorCount <= MaxErrors);
  end;
end;

procedure Register;
var TmpProvider: TProvider;
    TmpProviderEditor: TComponentEditor;
begin
  try
    TmpProvider := nil;
    TmpProviderEditor := nil;
    RegisterComponents('Data Access', [TOracleProvider]);
    // Some tricks to add the standard component editor
    try
      TmpProvider := TProvider.Create(nil);
      TmpProviderEditor := GetComponentEditor(TmpProvider, nil);
      OracleProviderEditorClass := TComponentEditorClass(TmpProviderEditor.ClassType);
      RegisterComponentEditor(TOracleProvider, TOracleProviderEditor);
    finally
      TmpProviderEditor.Free;
      TmpProvider.Free;
    end;
  except;
    // If it fails, it obviously wasn't the C/S version
  end;
end;

{$ELSE CompilerVersion5}

unit OracleProvider;

interface

uses
  Provider;

type
  TOracleProvider = class(TDataSetProvider)
  private
    FUniqueFields: string;
    procedure SetUniqueFields(const Value: string);
  published
    property UniqueFields: string read FUniqueFields write SetUniqueFields;
  end;

procedure Register;

implementation

uses Classes, Dialogs;

procedure TOracleProvider.SetUniqueFields(const Value: string);
begin
  if Value = FUniqueFields then Exit;
  FUniqueFields := Value;
  if Value <> '' then
    ShowMessage('WARNING: The UniqueFields property is no longer supported,' + #13#10 +
                'use Field.ProviderFlags instead');
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TOracleProvider]);
end;

{$ENDIF CompilerVersion5}

end.


