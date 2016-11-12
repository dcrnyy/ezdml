program ezdml;

{%File 'dict.txt'}
{%File 'readme.txt'}
{%File 'readme_CHS.txt'}
                      
uses
  Forms,
  wMainDml in 'wMainDml.pas' {frmMainDml},
  CtObjSerialer in 'DML\Ctobj\CtObjSerialer.pas',
  CTMetaData in 'DML\Ctobj\CTMetaData.pas',
  WindowFuncs in 'DML\Ctobj\WindowFuncs.pas',
  wDmlHelp in 'wDmlHelp.pas' {frmHelpAbout},
  CtMetaTable in 'DML\CtMetaTable.pas',
  CtMetaSqlsvrDb in 'DML\CtMetaSqlsvrDb.pas',
  CtObjXmlSerial in 'DML\Ctobj\CtObjXmlSerial.pas',
  uFormAddCtFields in 'DML\uFormAddCtFields.pas' {frmAddCtFields},
  uFormAddTbLink in 'DML\uFormAddTbLink.pas' {frmAddTbLink},
  uFormCtDbLogon in 'DML\uFormCtDbLogon.pas' {frmLogonCtDB},
  uFormCtDML in 'DML\uFormCtDML.pas' {frmCtDML},
  wPostgreSqlDBConfig in 'DML\wPostgreSqlDBConfig.pas' {frmPostgreSQLDBConfig},
  uFormCtFieldProp in 'DML\uFormCtFieldProp.pas' {frmCtMetaFieldProp},
  uFormCtTableProp in 'DML\uFormCtTableProp.pas' {frmCtTableProp},
  uFormGenSql in 'DML\uFormGenSql.pas' {frmCtGenSQL},
  uFormImpTable in 'DML\uFormImpTable.pas' {frmImportCtTable},
  uFrameCtFieldDef in 'DML\uFrameCtFieldDef.pas' {FrameCtFieldDef: TFrame},
  uFrameCtTableDef in 'DML\uFrameCtTableDef.pas' {FrameCtTableDef: TFrame},
  uFrameCtTableList in 'DML\uFrameCtTableList.pas' {FrameCtTableList: TFrame},
  uWaitWnd in 'DML\uWaitWnd.pas' {frmWaitWnd},
  uFrameCtTableProp in 'DML\uFrameCtTableProp.pas' {FrameCtTableProp: TFrame},
  uFrameDML in 'DML\uFrameDML.pas' {FrameDML: TFrame},
  uFrmDML in 'DML\uFrmDML.pas' {frmDML},
  dmlstrs in 'DML\dmlstrs.pas',
  ezdmlstrs in 'ezdmlstrs.pas',
  DMLObjs in 'DML\DMLObjs.pas',
  uColorStyles in 'DML\uColorStyles.pas' {frmColorStyles},
  AutoNameCapitalize in 'DML\AutoNameCapitalize.pas',
  CtMetaPostgreSqlDb in 'DML\CtMetaPostgreSqlDb.pas',
  XLSfile in 'DML\XLSfile.pas',
  CtMetaAdoDb in 'DML\CtMetaAdoDb.pas',
  CtMetaOracleDb in 'DML\CtMetaOracleDb.pas',
  DmlScript in 'DML\DmlScript.pas',
  DmlScriptControl in 'DML\DmlScriptControl.pas',
  dlgConfirmReplace in 'DML\PSDebug\dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  dlgSearchText in 'DML\PSDebug\dlgSearchText.pas' {TextSearchDialog},
  ide_editor in 'DML\PSDebug\ide_editor.pas' {editor},
  uFrmGotoLine in 'DML\PSDebug\uFrmGotoLine.pas' {frmGotoLine},
  dlgReplaceText in 'DML\PSDebug\dlgReplaceText.pas' {TextReplaceDialog},
  uFormDmlSearch in 'DML\uFormDmlSearch.pas' {frmDmlSearch},
  CtDataSetFile in 'DML\CtDataSetFile.pas',
  uDMLSqlEditor in 'DML\uDMLSqlEditor.pas' {frmDmlSqlEditor},
  UCNSpell in 'DML\UCNSpell.pas',
  DMLGraph in 'DML\DMLGraph.pas',
  ImgView in 'DML\ImgView.pas',
  CtMetaMysqlDb in 'DML\CtMetaMysqlDb.pas',
  CtMetaSqliteDb in 'DML\CtMetaSqliteDb.pas',
  wOracleDBConfig in 'DML\wOracleDBConfig.pas' {frmOraDBConfig};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := srEzdmlAppTitle;
  Application.CreateForm(TfrmMainDml, frmMainDml);
  Application.Run;
end.
