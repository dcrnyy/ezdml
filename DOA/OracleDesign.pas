// Direct Oracle Access - Designtime interface
// Copyright 1998 - 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleDesign;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion6}
    DesignIntf, DesignEditors,
    {$IFNDEF NODATASET} DSDesign, {$ENDIF}
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  {$IFNDEF NODATASET} DB, DBConsts, {$ENDIF}
  Classes, WinTypes, Controls, SysUtils, Dialogs, Forms, OracleQB;
{$ELSE}
uses
  {$IFNDEF NODATASET} DB, DBConsts, {$ENDIF}
  Classes, Types, Controls, DesignIntf, DesignEditors, SysUtils, QDialogs;
{$ENDIF}

type
  // Component & Property editors
  TOracleSessionEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TOracleQueryEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TOracleScriptEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TOracleDirectPathLoaderEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TOracleDefaultEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  {$IFNDEF NODATASET}
  TOracleDataSetEditor = class(TComponentEditor)
  private
    {$IFNDEF CompilerVersion6}
    FEditor: TComponentEditor;
    {$ENDIF}
  public
    {$IFDEF CompilerVersion6}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ELSE}
      {$IFDEF CompilerVersion4}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
      {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: TFormDesigner); override;
      {$ENDIF}
    {$ENDIF}
    destructor  Destroy; override;
    procedure Edit; override;
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  {$ENDIF}
  TOraclePackageEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TOracleLogonEditor = class(TComponentEditor)
    function  GetVerbCount: Integer;              override;
    function  GetVerb(Index: Integer): string;    override;
    procedure ExecuteVerb(Index: Integer);        override;
  end;
  TVariablesProperty = class(TPropertyEditor)
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string;                   override;
  end;
  TDirectPathColumnsProperty = class(TPropertyEditor)
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string;                   override;
  end;
  {$IFNDEF NODATASET}
  TSequenceFieldProperty = class(TPropertyEditor)
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string;                   override;
  end;
  TQBEDefinitionProperty = class(TPropertyEditor)
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string;                   override;
  end;
  TDisabledConstraintsProperty = class(TPropertyEditor)
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string;                   override;
  end;
  {$ENDIF}
  TDatabaseProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc);       override;
  end;

{$IFNDEF NODATASET}
const
  OracleDataSetEditorClass: TComponentEditorClass = nil;
{$ENDIF}

implementation

uses
  Oracle, OracleCI, OracleTypes, OracleLogon,
  {$IFNDEF NODATASET}
  OracleData, OracleSeqFldEdit, OracleQBEEdit, OracleConstraintsEdit,
  {$ENDIF}
  OracleVarEdit, OracleExplorer, OracleSQLEdit,
  OracleDPColumnsEdit, OraclePreferences, OracleDefaults;

{$IFNDEF NODATASET}
type
  // Trick to access the 'protected' part of TOracleDataSet
  TOracleDataSet = class(OracleData.TOracleDataSet)
  end;
{$ENDIF}

// OracleSession component editor

function TOracleSessionEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TOracleSessionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'E&xplore...';
    1 : Result := '&Preferences...';
    2 : Result := '&Defaults...';
    3 : Result := '&Info...';
  end;
end;

procedure TOracleSessionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : ExecuteExplorer(Component as TOracleSession, nil);
    1 : DoOraclePreferences;
    2 : ExecuteDefaultsEditor(Component);
    3 : ShowDOAInfo(Component as TOracleSession);
  end;
end;

// OracleQuery component editor

function TOracleQueryEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

function TOracleQueryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&SQL Editor...';
    1 : Result := 'Query &Builder...';
    2 : Result := '&Variables Editor...';
    3 : Result := '&Defaults...';
    4 : Result := 'E&xplore...';
  end;
end;

function ExecuteQueryBuilder(Query: TOracleQuery): Boolean;
var S: string;
begin
  Result := False;
  S := Query.SQL.Text;
  {$IFNDEF LINUX}
  QBCreate(Application.Handle);
  if not CanQB then
  begin
    QBFree;
    ShowMessage('The Query Builder is not installed,' + #13#10 +
                'you can download it for free at www.allroundautomations.com');
    Exit;
  end;
  if DoQueryBuilder(Query.Session, S) then
  begin
    Query.SQL.Text := S;
    Result := True;
  end;
  QBFree;
  {$ENDIF}
end;


procedure TOracleQueryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : if ExecuteSQLEditor(Component as TOracleQuery) then Designer.Modified;
    1 : if ExecuteQueryBuilder(Component as TOracleQuery) then Designer.Modified;
    2 : if ExecuteVariablesEditor(Component as TOracleQuery, Component.Name) then Designer.Modified;
    3 : ExecuteDefaultsEditor(Component);
    4 : ExecuteExplorer((Component as TOracleQuery).Session, nil);
  end;
end;

// OracleScript component editor

function TOracleScriptEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TOracleScriptEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Script Editor...';
    1 : Result := '&Defaults...';
    2 : Result := 'E&xplore...';
  end;
end;

procedure TOracleScriptEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : if ExecuteCommandEditor(Component as TOracleScript) then Designer.Modified;
    1 : ExecuteDefaultsEditor(Component);
    2 : ExecuteExplorer((Component as TOracleScript).Session, nil);
  end;
end;

// OracleDirectPathLoader component editor

function TOracleDirectPathLoaderEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TOracleDirectPathLoaderEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Columns Editor...';
    1 : Result := '&Defaults...';
    2 : Result := 'E&xplore...';
  end;
end;

procedure TOracleDirectPathLoaderEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : if ExecuteDirectPathColumnEditor(Component as TOracleDirectPathLoader) then Designer.Modified;
    1 : ExecuteDefaultsEditor(Component);
    2 : ExecuteExplorer((Component as TOracleDirectPathLoader).Session, nil);
  end;
end;

// OracleDataSet component editor

{$IFNDEF NODATASET}

{$IFDEF CompilerVersion6}
constructor TOracleDataSetEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
{$ELSE}
  {$IFDEF CompilerVersion4}
constructor TOracleDataSetEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
  {$ELSE}
constructor TOracleDataSetEditor.Create(AComponent: TComponent; ADesigner: TFormDesigner);
  {$ENDIF}
{$ENDIF}
begin
  inherited Create(AComponent, ADesigner);
  {$IFNDEF CompilerVersion6}
  FEditor := TComponentEditor(OracleDataSetEditorClass.Create(AComponent, ADesigner));
  {$ENDIF}
end;

destructor TOracleDataSetEditor.Destroy;
begin
  {$IFNDEF CompilerVersion6}
  FEditor.Free;
  FEditor := nil;
  {$ENDIF}
  inherited Destroy
end;

function TOracleDataSetEditor.GetVerbCount: Integer;
begin
  Result := 7;
end;

function TOracleDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Fields Editor';
    1 : Result := '&SQL Editor...';
    2 : Result := 'Query &Builder...';
    3 : Result := '&Variables Editor...';
    4 : Result := '&QBE Definition Editor...';
    5 : Result := '&Defaults...';
    6 : Result := 'E&xplore...';
  end;
end;

procedure TOracleDataSetEditor.ExecuteVerb(Index: Integer);
var TmpSQL: TStringList;
begin
  case Index of
    0 : {$IFDEF CompilerVersion6}
         {$IFDEF LINUX}
         ; // How to start the default fields editor from Kylix?
         {$ELSE}
         ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
         {$ENDIF}
        {$ELSE}
        FEditor.ExecuteVerb(0);
        {$ENDIF}
    1 : with TOracleDataSet(Component) do if ExecuteSQLEditor(InternalQuery) then
        begin
          // Modify the SQL property as usual
          TmpSQL := TStringList.Create;
          TmpSQL.Assign(InternalQuery.SQL);
          try
            SQL := TmpSQL;
          finally
            TmpSQL.Free;
            Self.Designer.Modified;
          end;
        end;
    2 : if ExecuteQueryBuilder(TOracleDataSet(Component).InternalQuery) then
        begin
          TOracleDataSet(Component).Reactivate;
          Designer.Modified;
        end;
    3 : with TOracleDataSet(Component) do if ExecuteVariablesEditor(InternalQuery, Component.Name) then
        begin
          // When the variables change and the dataset is active, it needs to be
          // reactivated to completely reinitialise
          Reactivate;
          Self.Designer.Modified;
        end;
    4 : if ExecuteQBEDefinitionEditor(TOracleDataSet(Component)) then
        begin
          Self.Designer.Modified;
        end;
    5 : ExecuteDefaultsEditor(Component);
    6 : ExecuteExplorer(TOracleDataSet(Component).Session, nil);
  end;
end;

procedure TOracleDataSetEditor.Edit;
begin
  ExecuteVerb(0);
end;

{$ENDIF}

// OraclePackage component editor

function TOraclePackageEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOraclePackageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Defaults...';
    1 : Result := 'E&xplore...';
  end;
end;

procedure TOraclePackageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : ExecuteDefaultsEditor(Component);
    1 : if ExecuteExplorer((Component as TOraclePackage).Session, Component as TOraclePackage) then Designer.modified;
  end;
end;

// OracleQueue component editor

function TOracleDefaultEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOracleDefaultEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Defaults...';
    1 : Result := 'E&xplore...';
  end;
end;

procedure TOracleDefaultEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : ExecuteDefaultsEditor(Component);
    1 : begin
          if Component is TOracleEvent then ExecuteExplorer(TOracleEvent(Component).Session, nil);
          if Component is TOracleQueue then ExecuteExplorer(TOracleQueue(Component).Session, nil);
        end;
  end;
end;

// OracleLogon component editor

function TOracleLogonEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TOracleLogonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Logon...';
    1 : Result := '&Change password...';
    2 : Result := '&Defaults...';
  end;
end;

procedure TOracleLogonEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : (Component as TOracleLogon).Execute;
    1 : (Component as TOracleLogon).SetPassword;
    2 : ExecuteDefaultsEditor(Component);
  end;
end;

// Variables Property editor

procedure TVariablesProperty.Edit;
begin
  if ExecuteVariablesEditor(TVariables(GetOrdValue).Owner as TOracleQuery, '') then
    Modified;
end;

function TVariablesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TVariablesProperty.GetValue: string;
var Q: TOracleQuery;
    i: Integer;
begin
  Q := (TVariables(GetOrdValue).Owner as TOracleQuery);
  with Q do
  begin
    Result := '';
    for i := 0 to VariableCount - 1 do
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + VariableName(i);
    end;
  end;
end;

// TDirectPathColumnsProperty Property editor

procedure TDirectPathColumnsProperty.Edit;
begin
  if ExecuteDirectPathColumnEditor(TOracleDirectPathLoader(GetComponent(0))) then
    Modified;
end;

function TDirectPathColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TDirectPathColumnsProperty.GetValue: string;
var i: Integer;
begin
  Result := '';
  with TOracleDirectPathLoader(GetComponent(0)) do
  begin
    for i := 0 to Columns.Count - 1 do
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + Columns[i].Name;
    end;
  end;
end;

// SequenceField Property editor

{$IFNDEF NODATASET}

procedure TSequenceFieldProperty.Edit;
var DataSet: TOracleDataSet;
    i: Integer;
begin
  DataSet := TOracleDataSet(GetComponent(0));
  with TSequenceFieldForm.Create(nil) do
  begin
    SequenceListBox.Text := DataSet.SequenceField.Sequence;
    FieldListBox.Text := DataSet.SequenceField.Field;
    ApplyGroup.ItemIndex := Ord(DataSet.SequenceField.ApplyMoment);
    for i := 0 to DataSet.FieldCount - 1 do
      if DataSet.Fields[i].FieldKind = fkData then
        FieldListBox.Items.Add(DataSet.Fields[i].FieldName);
    if DataSet.Session <> nil then
    begin
      SequenceSession.logonUsername := DataSet.Session.LogonUsername;
      SequenceSession.logonPassword := DataSet.Session.LogonPassword;
      SequenceSession.logonDatabase := DataSet.Session.LogonDatabase;
    end;
    Caption := DataSet.Owner.Name + '.' + DataSet.Name + ' SequenceField';
    if (ShowModal = mrOK) and Changed then
    begin
      DataSet.SequenceField.Sequence := Trim(SequenceListBox.Text);
      DataSet.SequenceField.Field := Trim(FieldListBox.Text);
      DataSet.SequenceField.ApplyMoment := TSequenceApplyMoment(ApplyGroup.ItemIndex);
      Modified;
    end;
    Free;
  end;
end;

function TSequenceFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSequenceFieldProperty.GetValue: string;
begin
  Result := TOracleDataSet(GetComponent(0)).SequenceField.ValueName;
end;

{$ENDIF}

// QBEDefinition Property editor

{$IFNDEF NODATASET}

procedure TQBEDefinitionProperty.Edit;
begin
  if ExecuteQBEDefinitionEditor(TOracleDataSet(GetComponent(0))) then Modified;
end;

function TQBEDefinitionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TQBEDefinitionProperty.GetValue: string;
begin
  Result := '';
  with TOracleDataSet(GetComponent(0)).QBEDefinition do
  begin
    if SaveQBEValues then Result := 'SaveQBEValues';
    if AllowFileWildcards then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + 'AllowFileWildcards';
    end;
  end;
  Result := '[' + Result + ']';
end;

{$ENDIF}

// DisabledConstraints Property editor

{$IFNDEF NODATASET}

procedure TDisabledConstraintsProperty.Edit;
begin
  if ExecuteDisabledConstraintsEditor(TOracleDictionary(GetComponent(0)).DataSet) then Modified;
end;

function TDisabledConstraintsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TDisabledConstraintsProperty.GetValue: string;
var i: Integer;
begin
  Result := '';
  with TOracleDictionary(GetComponent(0)) do
  begin
    for i := 0 to DisabledConstraints.Count - 1 do
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + DisabledConstraints[i];
    end;
  end;
  Result := '[' + Result + ']';
end;

{$ENDIF}

// Database Property editor

function TDatabaseProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TDatabaseProperty.GetValues(Proc: TGetStrProc);
var i: Integer;
begin
  BuildOracleAliasList;
  for i := 0 to OracleAliasList.Count - 1 do Proc(OracleAliasList[i]);
end;

{$IFDEF EVALUATION}
{$DEFINE DESIGN}
{$I OracleEvaluation.inc}
{$ENDIF}

end.
