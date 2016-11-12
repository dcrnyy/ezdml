// Direct Oracle Access - Component registration unit
// Copyright 1997, 2002 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$WARNINGS OFF}

unit OracleReg;

interface

uses
  {$IFDEF CONDITIONALEXPRESSIONS} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  {$IFNDEF NODATASET} DB, OracleData, OracleNavigator, {$ENDIF}
  Classes, SysUtils, Oracle, OracleCI, OracleTypes, OracleLogon,
  OracleTools, OracleDesign;

procedure Register;

implementation

{$R Oracle.dcr}

procedure Register;
{$IFNDEF NODATASET}
{$IFNDEF CONDITIONALEXPRESSIONS}
var TmpDataSet: TDataSet;
    TmpDataSetEditor: TComponentEditor;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF NODATASET}
  RegisterComponents('Oracle', [TOracleSession]);
  RegisterComponents('Oracle', [TOracleSessionPool]);
  RegisterComponents('Oracle', [TOracleLogon]);
  RegisterComponents('Oracle', [TOracleQuery]);
  RegisterComponents('Oracle', [TOraclePackage]);
  RegisterComponents('Oracle', [TOracleEvent]);
  RegisterComponents('Oracle', [TOracleQueue]);
  RegisterComponents('Oracle', [TOracleScript]);
  RegisterComponents('Oracle', [TOracleDirectPathLoader]);
  {$ELSE}
  RegisterComponents('Data Access', [TOracleSession]);
  RegisterComponents('Data Access', [TOracleSessionPool]);
  RegisterComponents('Data Access', [TOracleLogon]);
  RegisterComponents('Data Access', [TOracleQuery]);
  RegisterComponents('Data Access', [TOracleDataSet]);
  RegisterComponents('Data Access', [TOraclePackage]);
  RegisterComponents('Data Access', [TOracleEvent]);
  RegisterComponents('Data Access', [TOracleQueue]);
  RegisterComponents('Data Access', [TOracleScript]);
  RegisterComponents('Data Access', [TOracleDirectPathLoader]);
  RegisterComponents('Data Access', [TOracleNavigator]);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TVariables), nil, '', TVariablesProperty);
  {$IFNDEF NODATASET}
  RegisterPropertyEditor(TypeInfo(TSequenceField), nil, '', TSequenceFieldProperty);
  RegisterPropertyEditor(TypeInfo(TQBEDefinition), nil, '', TQBEDefinitionProperty);
  RegisterPropertyEditor(TypeInfo(TDisabledConstraints), nil, '', TDisabledConstraintsProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TOracleSession, 'LogonDatabase', TDatabaseProperty);
  RegisterPropertyEditor(TypeInfo(TDirectPathColumns), nil, '', TDirectPathColumnsProperty);
  RegisterComponentEditor(TOracleQuery, TOracleQueryEditor);
  RegisterComponentEditor(TOracleSession, TOracleSessionEditor);
  RegisterComponentEditor(TOraclePackage, TOraclePackageEditor);
  RegisterComponentEditor(TOracleScript, TOracleScriptEditor);
  RegisterComponentEditor(TOracleDirectPathLoader, TOracleDirectPathLoaderEditor);
  RegisterComponentEditor(TOracleLogon, TOracleLogonEditor);
  RegisterComponentEditor(TOracleEvent, TOracleDefaultEditor);
  RegisterComponentEditor(TOracleQueue, TOracleDefaultEditor);
  {$IFNDEF NODATASET}
  {$IFDEF CONDITIONALEXPRESSIONS}
  RegisterComponentEditor(TOracleDataSet, TOracleDataSetEditor);
  {$ELSE}
  // Some tricks to add component editors to TOracleDataSet's Fields Editor
  TmpDataSet := nil;
  TmpDataSetEditor := nil;
  try
    TmpDataSet := TDataSet.Create(nil);
    TmpDataSetEditor := GetComponentEditor(TmpDataSet, nil);
    OracleDataSetEditorClass := TComponentEditorClass(TmpDataSetEditor.ClassType);
    RegisterComponentEditor(TOracleDataSet, TOracleDataSetEditor);
  finally
    TmpDataSetEditor.Free;
    TmpDataSet.Free;
  end;
  {$ENDIF}
  {$ENDIF}
  RegisterOracleTools;
end;

end.

