unit wMainDml;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls, XPMan,
  uFrameCtTableDef, CtMetaTable, CtMetaData, CtObjSerialer, CtObjXmlSerial, wDmlHelp,
  Oracle, uWaitWnd, ActnList, StdActns, ZcDropFile;

const
  WMZ_CUSTCMD = WM_USER + $1001;

type
  TfrmMainDml = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    MnOpen1: TMenuItem;
    MnSave1: TMenuItem;
    MnExit1: TMenuItem;
    MnAbout1: TMenuItem;
    TimerInit: TTimer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    XPManifest1: TXPManifest;
    MnImportDB: TMenuItem;
    MnGenSql: TMenuItem;
    MnClear: TMenuItem;
    MNNewTb: TMenuItem;
    Mn_File: TMenuItem;
    N2: TMenuItem;
    MnExitWithoutSave: TMenuItem;
    ActionList1: TActionList;
    EditSelectAll1: TEditSelectAll;
    MN_Saveas: TMenuItem;
    MN_Modal: TMenuItem;
    Mn_Help: TMenuItem;
    Mn_About: TMenuItem;
    N1: TMenuItem;
    Mn_NewModal: TMenuItem;
    Mn_ShowPhyView: TMenuItem;
    MN_ColorStyles: TMenuItem;
    N3: TMenuItem;
    MN_ExportXls: TMenuItem;
    mn_EzdmlHomePage: TMenuItem;
    MN_SearchFields: TMenuItem;
    MN_Recentfiles: TMenuItem;
    actOpenLastFile1: TAction;
    Openlastfile1: TMenuItem;
    MN_EditMyDict: TMenuItem;
    MnTools1: TMenuItem;
    MnBackupDatabase: TMenuItem;
    MnRestoreDatabase: TMenuItem;
    MNSqlTool: TMenuItem;
    MN_FindHex: TMenuItem;
    TimerAutoSave: TTimer;
    MN_LocInExplorer: TMenuItem;
    N4: TMenuItem;
    MN_EditINIfile: TMenuItem;
    mn_EZDMLforum: TMenuItem;
    MN_editGlobalScript: TMenuItem;
    MemoGScript: TMemo;
    MN_ExecScript: TMenuItem;
    procedure MN_ExecScriptClick(Sender: TObject);
    procedure MnRestoreDatabaseClick(Sender: TObject);
    procedure MnBackupDatabaseClick(Sender: TObject);
    procedure MN_EditMyDictClick(Sender: TObject);
    procedure actOpenLastFile1Execute(Sender: TObject);
    procedure MN_SearchFieldsClick(Sender: TObject);
    procedure mn_EzdmlHomePageClick(Sender: TObject);
    procedure MN_ExportXlsClick(Sender: TObject);
    procedure Mn_AboutClick(Sender: TObject);
    procedure MN_ColorStylesClick(Sender: TObject);
    procedure Mn_ShowPhyViewClick(Sender: TObject);
    procedure Mn_NewModalClick(Sender: TObject);
    procedure MNNewTbClick(Sender: TObject);
    procedure MnClearClick(Sender: TObject);
    procedure MnGenSqlClick(Sender: TObject);
    procedure MnImportDBClick(Sender: TObject);
    procedure MnSave1Click(Sender: TObject);
    procedure MnOpen1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TimerInitTimer(Sender: TObject);
    procedure MnAbout1Click(Sender: TObject);
    procedure MnExit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MnExitWithoutSaveClick(Sender: TObject);
    procedure MN_SaveasClick(Sender: TObject);
    procedure MNSqlToolClick(Sender: TObject);
    procedure MN_FindHexClick(Sender: TObject);
    procedure TimerAutoSaveTimer(Sender: TObject);
    procedure MN_LocInExplorerClick(Sender: TObject);
    procedure MN_EditINIfileClick(Sender: TObject);
    procedure mn_EZDMLforumClick(Sender: TObject);
    procedure MN_editGlobalScriptClick(Sender: TObject);
  private
    { Private declarations }
    FFrameCtTableDef: TFrameCtTableDef;
    FCtDataModelList: TCtDataModelGraphList;
    FCurFileName: string;

    FfrmMetaImport: TForm;

    FProgressAll: Integer;
    FProgressCur: Integer;

    FWaitWnd: TfrmWaitWnd;
    FOrginalCaption: string;

    FZcDropFile: TZcDropFile;
    FGlobeOpeningFile: string;
    FRecentFiles: TStringList;

    FAutoSaveMinutes: Integer;
    FAutoSaveCounter: Integer;
    FIsAutoSaving: Boolean;

    FFindHexDlg: TForm;
    FGlobalScriptor: TObject;

    FFileLockMutex: THandle;

    procedure _OnDMLObjProgress(Sender: TObject; const Prompt: string;
      Cur, All: Integer; var bContinue: Boolean);
    procedure On_ZcDropFileDropFile(APoint: TPoint; AKeyState: Integer;
      bDeleteSrc: Boolean; AFiles: array of string);
    procedure _OnRecentFileClick(Sender: TObject);

    procedure PromptOpenFile(fn: string);
    procedure LoadFromFile(fn: string);
    procedure SaveToFile(fn: string);
    procedure CheckCaption;

    procedure LoadIni;
    procedure SaveIni;
    procedure SetRecentFile(fn: string);
    procedure RemoveRecentFile(fn: string);
    procedure RecreateRecentMn;
    procedure BackupTmpFile(fn: string);
    procedure TryLockFile(fn: String);

    procedure CheckReloadGlobalScript;
  protected
    procedure CreateWnd; override;
    procedure _WMZ_CUSTCMD(var msg: TMessage); message WMZ_CUSTCMD;
  public
    { Public declarations }
  end;

function EzdmlExecAppCmd(Cmd, param1, param2: String): String;

var
  frmMainDml: TfrmMainDml;
const
  DEF_GSCRIPT_FN='GlobalScript.ps';

implementation

uses
  uFormImpTable, uFormGenSql, CtMetaOracleDb, CtMetaAdoDb, AdoDB, ide_editor, DmlScript, uPSComponent, 
  CtMetaSqlsvrDb, CtMetaMysqlDb, CtMetaSqliteDb, CtMetaPostgreSqlDb, uFormCtDml,
  ezdmlstrs, dmlstrs, DMLObjs, ShellAPI, IniFiles, AutoNameCapitalize, uDMLSqlEditor, FindHexDlg;

{$R *.dfm}

function EzdmlExecAppCmd(Cmd, param1, param2: String): String;
  function GenDmlGraphBase64(dmlName: String): String;
  var
    dml: TCtDataModelGraph;
  begin
    if dmlName='(CUR_DATA_MODEL)' then
    begin
      Result := frmMainDml.FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.SaveWmfImage('(BASE64TEXT)');
      Exit;
    end;

    with TfrmCtDML.Create(Application) do
    try
      dml := TCtDataModelGraph(FGlobeDataModelList.ItemByName(dmlName));
      if dml=nil then
        dml := FGlobeDataModelList.CurDataModal;
      Init(dml, True, True);
      Result := FFrameCtDML.SaveWmfImage('(BASE64TEXT)');
    finally
      Free;
    end;
  end;
begin
  Result:='';
  if cmd='GET_DML_GRAPH_BASE64TEXT' then
  begin
    Result := GenDmlGraphBase64(param1);
  end;
end;
     
function GetFileAges(fn: string; var vFileDate, vCreateDate: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  Res: Integer;
begin
  Result := False;
  vFileDate := Now;
  vCreateDate := vFileDate;
  Handle := FindFirstFile(PChar(Fn), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := True;
    Windows.FindClose(Handle);

    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Res).Hi,
      LongRec(Res).Lo);
    vFileDate := FileDateToDateTime(res);
    FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(res).Hi,
      LongRec(res).Lo);
    vCreateDate := FileDateToDateTime(res);
  end;
end;

function SetFileAges(fn: string; vFileDate, vCreateDate: TDateTime): Boolean;
var
  Handle, f: THandle;
  FindData: TWin32FindData;
  LocalFileTime, FileTime: TFileTime;
  Age: Integer;
begin
  Result := False;
  Handle := FindFirstFile(PChar(Fn), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);

    f := CreateFile(PChar(fn), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_DELETE,
      nil, OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS, 0);

    //f := FileOpen(fn, fmOpenWrite);
    if f = THandle(-1) then
      RaiseLastOSError;

    if vFileDate>1 then
    begin
      Age := DateTimeToFileDate(vFileDate);
      if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
        LocalFileTimeToFileTime(LocalFileTime, FileTime) then
        SetFileTime(f, nil, nil, @FileTime);
    end;
                  
    if vCreateDate>1 then
    begin
      Age := DateTimeToFileDate(vCreateDate);
      if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
        LocalFileTimeToFileTime(LocalFileTime, FileTime) then
        SetFileTime(f, @FileTime, nil, nil);
    end;

    FileClose(f);

  end;
end;


procedure TfrmMainDml.CreateWnd;
begin
  inherited;

  if HandleAllocated and (FZcDropFile = nil) then
  try
    FZcDropFile := TZcDropFile.Create(Self);
    FZcDropFile.HandledTarget := Self;
    FZcDropFile.OnDropFile := On_ZcDropFileDropFile;
    FZcDropFile.Active := True;
  except
  end;
end;

procedure TfrmMainDml.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  fn, sfn, ext: string;
  vFileDate, vCreateDate: TDateTime;
begin
  sfn := FCurFileName;
  ext := LowerCase(ExtractFileExt(sfn));
  if ((ext = '.tmp') or (ext='')) and (FCtDataModelList.TableCount > 0) then
    case Application.MessageBox(PChar(srEzdmlConfirmExit), PChar(Application.Title),
      MB_YESNOCANCEL or MB_ICONWARNING) of
      IDYES:
      begin
        MN_SaveasClick(nil);            
        sfn := FCurFileName;
        ext := LowerCase(ExtractFileExt(sfn));
        if FCurFileName='' then
        begin
          CanClose := False;
          Exit;
        end;
      end;
      IDNO:
        FCtDataModelList.Clear;
    else
      CanClose := False;
      Exit;
    end;
  FFrameCtTableDef.Init(nil, False);
  if FFrameCtTableDef.FFrameCtTableList.TreeViewCttbs.Items.GetFirstNode <> nil then
    FFrameCtTableDef.FFrameCtTableList.TreeViewCttbs.Items.GetFirstNode.Text := srEzdmlExiting;
  Self.Refresh;
  if FCtDataModelList.TableCount > 0 then
  begin
    try
      fn := sfn;
      if ext <> '.tmp' then
        fn := fn + '.tmp';
      SaveToFile(fn);
      if GetFileAges(sfn, vFileDate, vCreateDate) then
        SetFileAges(fn, 0, vFileDate);
      FCurFileName := sfn;
      CheckCaption;
      {if (FCurFileName <> '') then
        SaveToFile(FCurFileName)
      else
      begin
        MN_SaveasClick(nil);
        if FCurFileName = '' then
          CanClose := False;
      end; }
    except
    end;
  end;
end;

procedure TfrmMainDml.FormCreate(Sender: TObject);
var
  db: TCtMetaDatabase;
begin
  //if LoadNewResourceModule($0409) <> 0 then
    //ReinitializeForms();
  FOrginalCaption := Caption;

  FRecentFiles := TStringList.Create;
  FAutoSaveMinutes := 5;

  FCtDataModelList := TCtDataModelGraphList.Create;
  FGlobeDataModelList := FCtDataModelList;
  FCtDataModelList.OnObjProgress := _OnDMLObjProgress;

  FFrameCtTableDef := TFrameCtTableDef.Create(Self);
  FFrameCtTableDef.Parent := Self;
  FFrameCtTableDef.Align := alClient;
  FFrameCtTableDef.Init(FCtDataModelList, False);

  with FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML do
  begin
    actFileNew.Caption := MnClear.Caption;
    actFileNew.OnExecute := MnClear.OnClick;
    actFileOpen.Caption := MnOpen1.Caption;
    actFileOpen.OnExecute := MnOpen1.OnClick;
    actFileSave.Caption := MnSave1.Caption;
    actFileSave.OnExecute := MnSave1.OnClick;
  end;

  if GetCtMetaDBReg('ORACLE')^.DbImpl = nil then
  begin
    db := TCtMetaOracleDbUtf8.Create;
    TCtMetaOracleDb(db).Session := Oracle.TOracleSession.Create(Self);
    TCtMetaOracleDb(db).Session.RollbackOnDisconnect := True;
    TCtMetaOracleDb(db).Session.BytesPerCharacter := Oracle.bcAutoDetect;
    GetCtMetaDBReg('ORACLE')^.DbImpl := db;
  end;
  if GetCtMetaDBReg('SQLSERVER')^.DbImpl = nil then
  begin
    db := TCtMetaSqlsvrDb.Create;
    TCtMetaSqlsvrDb(db).ADOConnection := TADOConnection.Create(Self);
    GetCtMetaDBReg('SQLSERVER')^.DbImpl := db;
  end;
  if GetCtMetaDBReg('ODBC')^.DbImpl = nil then
  begin
    db := TCtMetaAdoDb.Create;
    TCtMetaAdoDb(db).ADOConnection := TADOConnection.Create(Self);
    GetCtMetaDBReg('ODBC')^.DbImpl := db;
  end;
  if GetCtMetaDBReg('MYSQL')^.DbImpl = nil then
  begin
    db := TCtMetaMysqlDb.Create;
    //TCtMetaMysqlDb(db).ADOConnection := TADOConnection.Create(Self);
    GetCtMetaDBReg('MYSQL')^.DbImpl := db;
  end;
  if GetCtMetaDBReg('SQLITE')^.DbImpl = nil then
  begin
    db := TCtMetaSqliteDb.Create;
    GetCtMetaDBReg('SQLITE')^.DbImpl := db;
  end;
  if GetCtMetaDBReg('POSTGRESQL')^.DbImpl = nil then
  begin
    db := TCtMetaPostgreSqlDb.Create;
    GetCtMetaDBReg('POSTGRESQL')^.DbImpl := db;
  end;


  LoadIni;
  RecreateRecentMn;

end;

procedure TfrmMainDml.LoadFromFile(fn: string);
var
  fs: TCtObjSerialer;
begin
  if FileExists(fn) then
  begin
    try
      StatusBar1.SimpleText := Format(srEzdmlOpeningFileFmt, [fn]);
      Self.Refresh;
      if (LowerCase(ExtractFileExt(fn)) = '.dmx') or (LowerCase(ExtractFileExt(fn)) = '.xml') then
        fs := TCtObjXmlSerialer.Create(fn, fmOpenRead or fmShareDenyNone)
      else
        fs := TCtObjFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
      try
        fs.RootName := 'DataModels';
        FProgressAll := 0;
        FProgressCur := 0;
        FWaitWnd := TfrmWaitWnd.Create(Self);
        try
          FWaitWnd.Init(srEzdmlOpenFile + ' ' + ExtractFileName(fn), srEzdmlOpening,
            srEzdmlAbortOpening);
          FCtDataModelList.LoadFromSerialer(fs);
        finally
          FWaitWnd.Release;
          FWaitWnd := nil;
        end;
      finally
        fs.Free;

        FFrameCtTableDef.Init(FCtDataModelList, False);
      end;
      StatusBar1.SimpleText := fn;
      FCurFileName := fn;
      FAutoSaveCounter := 0;
      CheckCaption;
    except
      on E:Exception do
        if ExtractFileExt(fn)<>'.tmp' then
          raise
        else
          raise Exception.Create(Format(srEzdmlLoadTmpFileFailFmt,[fn, E.message]));
    end;
  end;
  CheckReloadGlobalScript;
end;

procedure TfrmMainDml.LoadIni;
var
  ini: TIniFile;
  I, L: Integer;
  S: string;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  try
    I := 0;
    FRecentFiles.Clear;
    while True do
    begin
      Inc(I);
      S := ini.ReadString('RecentFiles', IntToStr(I), '');
      if S = '' then
        Break;
      FRecentFiles.Add(S);
    end;

    I:=0;
    L:=0;
    SetLength(CtCustFieldTypeDefs, L);
    while True do
    begin
      Inc(I);
      S := Trim(ini.ReadString('DefaultFieldTypes', IntToStr(I), ''));
      if S = '' then
        Break;
      Inc(L);
      SetLength(CtCustFieldTypeDefs, L);
      CtCustFieldTypeDefs[L-1] := S;
    end;
    SetLength(DML_CustFieldTypeDefs, L);
    for I := 0 to L - 1 do
      DML_CustFieldTypeDefs[I] := CtCustFieldTypeDefs[I];

    I:=0;
    L:=0;
    SetLength(CtCustFieldTypeList, L);
    while True do
    begin
      Inc(I);
      S := Trim(ini.ReadString('CustFieldTypes', IntToStr(I), ''));
      if S = '' then
        Break;
      Inc(L);
      SetLength(CtCustFieldTypeList, L);
      CtCustFieldTypeList[L-1] := S;
    end;

    I:=0;
    L:=0;
    SetLength(CtCustDataTypeReplaces, L);
    while True do
    begin
      Inc(I);
      S := Trim(ini.ReadString('CustDataTypeReplaces', IntToStr(I), ''));
      if S = '' then
        Break;
      Inc(L);
      SetLength(CtCustDataTypeReplaces, L);
      CtCustDataTypeReplaces[L-1] := S;
    end;

    FCurFileName := ini.ReadString('RecentFiles', 'CurFileName', '');
    FAutoSaveMinutes := ini.ReadInteger('Options', 'AutoSaveMinutes', FAutoSaveMinutes);

    FieldNameMaxDrawSize := ini.ReadInteger('Options', 'FieldNameMaxDrawSize', FieldNameMaxDrawSize);
    FieldTypeMaxDrawSize := ini.ReadInteger('Options', 'FieldTypeMaxDrawSize', FieldTypeMaxDrawSize);
    FCreateSeqForOracle := ini.ReadBool('Options', 'CreateSeqForOracle', FCreateSeqForOracle);
    FBackupBeforeAlterColumn := ini.ReadBool('Options', 'BackupBeforeAlterColumn', FBackupBeforeAlterColumn);
    S := ini.ReadString('Options', 'OCIDLL', '');
    if S<>'' then
    begin
      Windows.SetEnvironmentVariable('_NS_ORA_INSTANT_CLIENT','True');
      Windows.SetEnvironmentVariable('_NS_OCIDLL', PAnsiChar(S));
    end;
  finally
    ini.Free;
  end;
end;

procedure TfrmMainDml.FormDestroy(Sender: TObject);
begin
  try
    FRecentFiles.Free;
    FCtDataModelList.Free;
    if Assigned(FGlobalScriptor) then
      FreeAndNil(FGlobalScriptor);
  except
  end;
end;

procedure TfrmMainDml.MnAbout1Click(Sender: TObject);
begin
  if frmHelpAbout = nil then
    frmHelpAbout := TfrmHelpAbout.Create(Self);
  frmHelpAbout.ShowModal;
end;

procedure TfrmMainDml.MnBackupDatabaseClick(Sender: TObject);
begin
  if not Assigned(FfrmMetaImport) then
    FfrmMetaImport := TfrmImportCtTable.Create(Self);
  TfrmImportCtTable(FfrmMetaImport).FCtMetaObjList := nil;
  TfrmImportCtTable(FfrmMetaImport).SetWorkMode(1);
  if FfrmMetaImport.ShowModal = mrOk then
  begin
    //FFrameCtTableDef.Init(FCtDataModelList, False);
    FFrameCtTableDef.FFrameCtTableList.RefreshTheTree;
  end;
end;

procedure TfrmMainDml.MnClearClick(Sender: TObject);
begin
  FCtDataModelList.Pack;
  if FCtDataModelList.Count = 0 then
    Exit;
  if Application.MessageBox(PChar(srEzdmlConfirmClearAll),
    PChar(srEzdmlNew), MB_OKCANCEL or MB_ICONWARNING) <> IDOK then
    Exit;
  FCtDataModelList.Clear;
  FCtDataModelList.SeqCounter := 0;
  FCtDataModelList.GlobeList.SeqCounter := 0;
  if FCtDataModelList.CurDataModal = nil then
    Exit;
  FFrameCtTableDef.Init(FCtDataModelList, False);
  StatusBar1.SimpleText := '';
  FCurFileName := '';
  FAutoSaveCounter := 0;    
  FCurDmlFileName := '';
  TryLockFile('');
  CheckCaption;
end;

procedure TfrmMainDml.MnExit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMainDml.MnGenSqlClick(Sender: TObject);
begin
  if not Assigned(frmCtGenSQL) then
    frmCtGenSQL := TfrmCtGenSQL.Create(Self);
  frmCtGenSQL.MetaObjList := FCtDataModelList.CurDataModal.Tables;
  frmCtGenSQL.SetWorkMode(0);

  if frmCtGenSQL.ShowModal = mrOk then
  begin
  end;
end;

procedure TfrmMainDml.MnImportDBClick(Sender: TObject);
begin
  if not Assigned(FfrmMetaImport) then
    FfrmMetaImport := TfrmImportCtTable.Create(Self);
  TfrmImportCtTable(FfrmMetaImport).FCtMetaObjList := Self.FCtDataModelList.CurDataModal.Tables;
  TfrmImportCtTable(FfrmMetaImport).SetWorkMode(0);
  if FfrmMetaImport.ShowModal = mrOk then
  begin
    //FFrameCtTableDef.Init(FCtDataModelList, False);
    FFrameCtTableDef.FFrameCtTableList.RefreshTheTree;
  end;
end;

procedure TfrmMainDml.MNNewTbClick(Sender: TObject);
begin
  //FFrameCtTableDef.FFrameCtTableList.actNewTable.Execute;
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actNewObj.Execute;
end;

procedure TfrmMainDml.MnOpen1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    PromptOpenFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmMainDml.MnRestoreDatabaseClick(Sender: TObject);
begin
  if not Assigned(frmCtGenSQL) then
    frmCtGenSQL := TfrmCtGenSQL.Create(Self);
  frmCtGenSQL.MetaObjList := nil;
  frmCtGenSQL.SetWorkMode(1);
  if frmCtGenSQL.LoadDbBackFile then
    if frmCtGenSQL.ShowModal = mrOk then
    begin
    end;
end;

procedure TfrmMainDml.MnSave1Click(Sender: TObject);
begin
  if FCurFileName <> '' then
    SaveToFile(FCurFileName)
  else
    MN_SaveasClick(nil);
end;

procedure TfrmMainDml.MNSqlToolClick(Sender: TObject);
begin
  if FDmlSqlEditorForm = nil then
    FDmlSqlEditorForm := TfrmDmlSqlEditor.Create(Application);
  FDmlSqlEditorForm.ShowModal;
end;

procedure TfrmMainDml.Mn_AboutClick(Sender: TObject);
var
  S: string;
begin
  S := srEzdmlAbout;
  S := Format(S, [srEzdmlVersionNum, srEzdmlVersionDate]);
  Application.MessageBox(PChar(S), PChar(Application.Title),
    MB_OK or MB_ICONINFORMATION);
end;

procedure TfrmMainDml.MN_ColorStylesClick(Sender: TObject);
begin
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actColorStyles.Execute;
end;

procedure TfrmMainDml.MN_editGlobalScriptClick(Sender: TObject);
var
  S, fn: string;
  sc: TDmlLanScriptor;
begin
  fn := DEF_GSCRIPT_FN;
  S := ExtractFilePath(Application.ExeName);
  if S <> '' then
    if S[Length(S)] <> '\' then
      S := S + '\';
  S := S + fn;
  if not FileExists(S) then
  begin
    if Application.MessageBox(PChar(Format(srEzdmlCreateGScriptTipFmt, [S])), PChar(Application.Title),
      MB_YESNOCANCEL or MB_ICONINFORMATION)<>IDYES then
      Exit;
    MemoGScript.Lines.SaveToFile(S);
  end;

  fn := S;
  if not Assigned(editor) then
    Application.CreateForm(Teditor, editor);
  sc := TDmlLanScriptor.Create;
  try
    sc.InitAnotherPS(editor.ce);
    //sc.Init('PASCAL_SCRIPT', nil, ss, nil);
    with editor do
    begin
      ed.ClearAll;
      ed.Lines.LoadFromFile(fn);
      ed.Modified := False;
      FileModified := False;
      aFile := fn;
      ShowModal;
    end;
  finally
    sc.Free;
  end;
  CheckReloadGlobalScript;
end;

procedure TfrmMainDml.MN_EditINIfileClick(Sender: TObject);
var
  S, fn: string;
begin
  fn := 'INI';
  if Application.MessageBox(PChar(Format(srEzdmlConfirmEditTextFmt, [fn])),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONINFORMATION) <> IDOK then
    Exit;
  S := ChangeFileExt(Application.ExeName,'.INI');
  if not FileExists(S) then
    with TFileStream.Create(S, fmCreate) do
      Free;
  ShellExecute(0, 'open', PChar(S), nil, nil, SW_SHOW);
  if Application.MessageBox(PChar(Format(srEzdmlConfirmEditedTextFmt, [fn])),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONINFORMATION) <> IDOK then
    Exit;
  LoadIni;
end;

procedure TfrmMainDml.MN_EditMyDictClick(Sender: TObject);
var
  S, fn: string;
begin
  fn := 'MyDict.txt';
  if Application.MessageBox(PChar(Format(srEzdmlConfirmEditTextFmt, [fn])),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONINFORMATION) <> IDOK then
    Exit;
  S := ExtractFilePath(Application.ExeName);
  if S <> '' then
    if S[Length(S)] <> '\' then
      S := S + '\';
  S := S + fn;
  if not FileExists(S) then
    with TFileStream.Create(S, fmCreate) do
      Free;
  ShellExecute(0, 'open', PChar(S), nil, nil, SW_SHOW);
  if Application.MessageBox(PChar(Format(srEzdmlConfirmEditedTextFmt, [fn])),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONINFORMATION) <> IDOK then
    Exit;
  GetAutoNameCapitalizer.ReloadDictFile;
end;

procedure TfrmMainDml.MN_ExecScriptClick(Sender: TObject);
begin
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actBatchOps.Execute;
end;

procedure TfrmMainDml.MN_ExportXlsClick(Sender: TObject);
begin
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actExportXls.Execute;
end;

procedure TfrmMainDml.mn_EZDMLforumClick(Sender: TObject);
var
  S, V: string;
begin
  S := 'http://www.ezdml.com/bbs';
  V := Format(srEzdmlConfirmOpenUrlFmt, [S]);

  if Application.MessageBox(PChar(V),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONWARNING) <> IDOK then
    Exit;

  ShellExecute(0, 'open', PChar(S), nil, nil, SW_SHOW);
end;

procedure TfrmMainDml.mn_EzdmlHomePageClick(Sender: TObject);
var
  S, V: string;
begin
  if srIsEnglish then
    S := 'http://www.ezdml.com/index.html'
  else
    S := 'http://www.ezdml.com/index_cn.html';
  //S := 'http://blog.csdn.net/huzgd/';
  V := Format(srEzdmlConfirmOpenUrlFmt, [S]);

  if Application.MessageBox(PChar(V),
    PChar(Application.Title), MB_OKCANCEL or MB_ICONWARNING) <> IDOK then
    Exit;

  ShellExecute(0, 'open', PChar(S), nil, nil, SW_SHOW);
end;

procedure TfrmMainDml.MN_FindHexClick(Sender: TObject);
begin
  if FFindHexDlg = nil then
    FFindHexDlg := TfrmFindHex.Create(Self);
  FFindHexDlg.ShowModal;
end;

procedure TfrmMainDml.MN_LocInExplorerClick(Sender: TObject);
var
  S, fn: String;
begin
  fn := FCurFileName;
  if (fn='') or not FileExists(fn) then
    fn := Application.ExeName;
  S := 'Explorer /select, "' + fn + '"';
  WinExec(PChar(S), SW_SHOW);
end;

procedure TfrmMainDml.Mn_NewModalClick(Sender: TObject);
begin
  FFrameCtTableDef.FFrameCtTableList.actNewModel.Execute;
end;

procedure TfrmMainDml.SaveIni;
var
  ini: TIniFile;
  I: Integer;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  try
    ini.EraseSection('RecentFiles');
    for I := 0 to FRecentFiles.Count - 1 do
      ini.WriteString('RecentFiles', IntToStr(I + 1), FRecentFiles[I]);
    ini.WriteString('RecentFiles', 'CurFileName', FCurFileName);
  finally
    ini.Free;
  end;
end;

procedure TfrmMainDml.SaveToFile(fn: string);
var
  fs: TCtObjSerialer;
  //I: Integer;
begin
  try
    if not FIsAutoSaving then
      if FFrameCtTableDef.FFrameCtTableList.TreeViewCttbs.CanFocus then
        FFrameCtTableDef.FFrameCtTableList.TreeViewCttbs.SetFocus;
  except
  end;
  StatusBar1.SimpleText := Format(srEzdmlSaveingFileFmt, [fn]);
  Self.Refresh;
  if FileExists(fn) then
    DeleteFile(fn);
  if (LowerCase(ExtractFileExt(fn)) = '.dmx') or (LowerCase(ExtractFileExt(fn)) = '.xml') then
    fs := TCtObjXmlSerialer.Create(fn, fmCreate)
  else
    fs := TCtObjFileStream.Create(fn, fmCreate);
  try
    fs.RootName := 'DataModels';
    FCtDataModelList.Pack;

    FProgressAll := 0;
    FProgressCur := 0;
    if FIsAutoSaving then
      FWaitWnd := nil
    else
      FWaitWnd := TfrmWaitWnd.Create(Self);
    try
      if not FIsAutoSaving then
        FWaitWnd.Init(srEzdmlSaveFile + ' ' + ExtractFileName(fn), srEzdmlSaving,
          srEzdmlAbortSaving);

      {for I := 0 to FTableList.Count - 1 do
        FTableList[I].MetaFields.Pack;}
      FCtDataModelList.SaveToSerialer(fs);

    finally
      if not FIsAutoSaving then
        FWaitWnd.Release;
      FWaitWnd := nil;
    end;
  finally
    fs.Free;
  end;
  StatusBar1.SimpleText := srEzdmlSaved + fn + ' ' + TimeToStr(Now);
  FCurFileName := fn;
  FAutoSaveCounter := 0;
  if not FIsAutoSaving then
    CheckCaption;
end;

procedure TfrmMainDml.SetRecentFile(fn: string);
var
  I: Integer;
  S: string;
begin
  S := LowerCase(fn);
  for I := 0 to FRecentFiles.Count - 1 do
    if LowerCase(FRecentFiles[I]) = S then
    begin
      if I > 0 then
      begin
        FRecentFiles.Exchange(I, 0);
        SaveIni;
        RecreateRecentMn;
      end;
      Exit;
    end;
  FRecentFiles.Insert(0, fn);
  SaveIni;
  RecreateRecentMn;
end;

procedure TfrmMainDml.TimerAutoSaveTimer(Sender: TObject);
var
  fn, sfn, ext: string;
begin
  if FIsAutoSaving or (FAutoSaveMinutes=0) then
    Exit;
  Inc(FAutoSaveCounter);
  if FAutoSaveCounter < FAutoSaveMinutes then
    Exit;

  FAutoSaveCounter := 0;
  if FCtDataModelList.TableCount <= 0 then
    Exit;

  sfn := FCurFileName;
  fn := sfn;
  if fn='' then
    fn := ChangeFileExt(Application.ExeName, '.dmh');
  ext := LowerCase(ExtractFileExt(fn));
  if ext <> '.tmp' then
    fn := fn + '.tmp';
  FIsAutoSaving := True;
  try
    SaveToFile(fn);
  finally
    FIsAutoSaving := False;
  end;
  FCurFileName := sfn;
  CheckCaption;
end;

procedure TfrmMainDml.TimerInitTimer(Sender: TObject);
var
  fn, sfn, ext: string;
  bCheck: Boolean;
  vFileDate1, vCreateDate1, vFileDate2, vCreateDate2: TDateTime;
begin
  TimerInit.Enabled := False;
  if ParamStr(1) <> '' then
  begin
    PromptOpenFile(ParamStr(1));
  end
  else if FCurFileName <> '' then
  begin
    try
      TryLockFile(FCurFileName);
    except
      FCurFileName := '';
      Exit;
    end;
    sfn := FCurFileName;
    ext := Lowercase(ExtractFileExt(sfn));
    fn := sfn;
    if ext <> '.tmp' then
      fn := fn + '.tmp';
    if FileExists(fn) then
    begin
      bCheck := False;
      if GetFileAges(sfn, vFileDate1, vCreateDate1)  and
        GetFileAges(fn, vFileDate2, vCreateDate2) then
      begin
        if Abs(vFileDate1-vCreateDate2)>2/24/60/60 then
          bCheck := True;
      end
      else
        bCheck := True;
      if bCheck then
        case (Application.MessageBox(PChar(Format(srEzdmlTmpFileChangedFmt,[fn])),
          PChar(srEzdmlOpenFile), MB_YESNOCANCEL or MB_ICONWARNING or MB_DEFBUTTON3)) of
        IDYES: BackupTmpFile(fn);
        IDNO: fn := sfn;
        else
          Abort;
        end;
      LoadFromFile(fn);
      FCurFileName := sfn;
      FCurDmlFileName := FCurFileName;
      CheckCaption;
    end
    else if FRecentFiles.Count > 0 then
      actOpenLastFile1.Execute;
  end
  else if FRecentFiles.Count > 0 then
  begin
    //fn := FRecentFiles[0];
    //if Application.MessageBox(PChar(Format(srEzdmlOpenLastFileFmt, [fn])),
    //  PChar(srEzdmlNew), MB_OKCANCEL or MB_ICONQUESTION) = IDOK then
    actOpenLastFile1.Execute;
    if FCurFileName <> '' then
      Self.SaveIni;
  end;
end;

procedure TfrmMainDml.TryLockFile(fn: String);
var
  AMutexAttrs: SECURITY_ATTRIBUTES;
  cAppN: array[0..1024] of Char;
  sAppN: string;
begin
  if FFileLockMutex <> 0 then
  begin
    CloseHandle(FFileLockMutex);
    FFileLockMutex := 0;
  end;
  if fn='' then
    Exit;
    
  AMutexAttrs.nLength := SizeOf(SECURITY_ATTRIBUTES);
  AMutexAttrs.lpSecurityDescriptor := nil;
  AMutexAttrs.bInheritHandle := False;

  sAppN := fn;
  sAppN := StringReplace(fn,':','#58',[rfReplaceAll]);
  sAppN := StringReplace(fn,'\','#92',[rfReplaceAll]);
  if Length(sAppN) > 1024 then
    sAppN := Copy(sAppN, 0, 1024);
  StrPCopy(cAppN, sAppN);
  FFileLockMutex := CreateMutexA(@AMutexAttrs, False, cAppN);
  if FFileLockMutex = 0 then
    Exit;
  if GetLastError() = ERROR_ALREADY_EXISTS then
    raise Exception.Create(Format(srEzdmlFileAlreadyOpenedFmt, [fn]));
end;

procedure TfrmMainDml._OnDMLObjProgress(Sender: TObject;
  const Prompt: string; Cur, All: Integer; var bContinue: Boolean);
begin
  if Assigned(FWaitWnd) then
  begin
    if (Prompt = '') and (Cur = 0) then
      if FProgressAll = 0 then
      begin
        FProgressCur := 0;
        FProgressAll := All;
      end;

    if FProgressAll > 0 then
    begin
      if Sender is TCtMetaTableList then
        Inc(FProgressCur);
      FWaitWnd.SetPercentMsg(FProgressCur * 100 / FProgressAll, Prompt, True)
    end
    else
    begin
      if All > 0 then
        FWaitWnd.SetPercentMsg(Cur * 100 / All, Prompt, True)
      else
        FWaitWnd.CheckCanceled;
    end;
    if FWaitWnd.Canceled then
      bContinue := False;
  end;
end;

procedure TfrmMainDml._OnRecentFileClick(Sender: TObject);
var
  fn: string;
begin
  if Sender is TMenuItem then
  begin
    fn := TMenuItem(Sender).Hint;
    if FCurFileName = fn then
      Exit;
    PromptOpenFile(fn);
  end;
end;

procedure TfrmMainDml._WMZ_CUSTCMD(var msg: TMessage);
begin
  PromptOpenFile(FGlobeOpeningFile);
  FGlobeOpeningFile := '';
end;

procedure TfrmMainDml.MnExitWithoutSaveClick(Sender: TObject);
begin
  FCtDataModelList.Clear;
  Close;
end;

procedure TfrmMainDml.actOpenLastFile1Execute(Sender: TObject);
var
  fn: string;
begin
  if FRecentFiles.Count > 0 then
  begin
    fn := FRecentFiles[0];
    if FCurFileName = fn then
    begin
      //Exit;
    end;
    PromptOpenFile(fn);
  end;
end;

procedure TfrmMainDml.BackupTmpFile(fn: string);
var
  bfn: String;
begin
  bfn := fn+'.'+FormatDateTime('yyyymmddhhnnss',Now);
  CopyFile(PChar(fn), PChar(bfn), False);
end;

procedure TfrmMainDml.CheckCaption;
begin
  if FCurFileName = '' then
    Caption := FOrginalCaption
  else
    Caption := FOrginalCaption + ' - ' + FCurFileName;
end;

type
  TTestMethod = function (s,par1: string): string of object;
  TOnEzdmlCmdEvent2=function (p1,p2,p3,p4,p5: String): string of object;
  TOnEzdmlCmdEvent3=function (p1,p2,p3,p4,p5,p6: String): string of object;

procedure CallScriptFunctionAsMethod;
var
  Meth: TTestMethod;      
  S:String;
begin
  with TPSScript.Create(nil) do
  try
  Script.Clear;
  Script.Add('function Test(s,par1:string): string; begin Result := ''Test Results: ''+s+'' p1:''+par1;end; begin end.');
  if not Compile()then
  ShowMessage('err1');
  Meth := TTestMethod(GetProcMethod('Test'));
  if(@Meth <> nil) then
  begin
  S:=Meth('INDATA','ss');
  ShowMessage( S);
  end
  else
  ShowMessage('err2');

  finally
    Free;
  end;
end;


function TestHH(p1,p2,p3,p4,p5:String):String;
begin
  Result := 'jjjj_jj'+p1+'kkkkk_'+p2+p3;
end;
              
procedure CallScriptFunctionAsMethod2;
var
  Meth: TOnEzdmlCmdEvent2;
  S:String;
begin
  with TPSScript.Create(nil) do
  try
  Script.Clear;
  Script.Add('function OnEzdmlCmdEvent(p1,p2,p3,p4,p5: String): string;begin  Result := ''rr''+p1+p2+p3+p4+p5;end;begin end.');
  if not Compile()then
  ShowMessage('err1');
  Meth := TOnEzdmlCmdEvent2(GetProcMethod('OnEzdmlCmdEvent'));
  if(@Meth <> nil) then
  begin
  S:=Meth('ppp111','ppp222','ppp333','ppp444', 'par555');
  //S:=Meth('test',S,'abcHHHHHHHHHHHHUUUUUUUUUUZZZZZZZZZZZZZZZZ');
  ShowMessage(S);
  end
  else
  ShowMessage('err2');

  finally
    Free;
  end;
end;

procedure CallScriptFunctionAsMethod3;
var
  Meth: TOnEzdmlCmdEvent3;
  //S:String;
begin
  with TPSScript.Create(nil) do
  try
  Script.Clear;
  Script.Add('procedure OnEzdmlCmdEvent(p1,p2,p3,p4,p5,p6: String);begin  p6 := ''rr''+p1+p2+p3+p4+p5;end;begin end.');
  if not Compile()then
  ShowMessage('err1');
  Meth := TOnEzdmlCmdEvent3(GetProcMethod('OnEzdmlCmdEvent'));
  if(@Meth <> nil) then
  begin
  Meth('ppp111','ppp222','ppp333','ppp444', 'par555', 'par666');
  //S:=Meth('test',S,'abcHHHHHHHHHHHHUUUUUUUUUUZZZZZZZZZZZZZZZZ');
  ShowMessage('S');
  end
  else
  ShowMessage('err2');

  finally
    Free;
  end;
end;

procedure TfrmMainDml.CheckReloadGlobalScript;
var
  FileTxt: TStrings;
  fn, S: string;
  bSuccess: Boolean;
  ce: TPSScript;
begin
  //CallScriptFunctionAsMethod3;
  //Exit;

  GProc_OnEzdmlGenTbSqlEvent:=nil;
  GProc_OnEzdmlGenDbSqlEvent:=nil;
  GProc_OnEzdmlGenFieldTypeDescEvent:=nil;
  GProc_OnEzdmlGenAlterFieldEvent:=nil;
  GProc_OnEzdmlCmdEvent:=nil;
  if Assigned(FGlobalScriptor) then
    FreeAndNil(FGlobalScriptor);

  fn := DEF_GSCRIPT_FN;
  S := ExtractFilePath(Application.ExeName);
  if S <> '' then
    if S[Length(S)] <> '\' then
      S := S + '\';
  S := S + fn;
  if not FileExists(S) then
    Exit;

  FGlobalScriptor := TDmlLanScriptor.Create;
  FileTxt := TStringList.Create;
  try
    FileTxt.LoadFromFile(S);
    S := FileTxt.Text;
  finally
    FileTxt.Free;
  end;

  bSuccess := False;
  with TDmlLanScriptor(FGlobalScriptor) do
  try
    ce := GetPS;
    ce.UsePreProcessor := True;

    //if not Compile('PASCAL_SCRIPT', S) then
    //  raise Exception.Create(DEF_GSCRIPT_FN+' compile failed!'#13#10 + StdOutPut.Text);
    Exec('PASCAL_SCRIPT', S);
    GProc_OnEzdmlGenTbSqlEvent:= TOnEzdmlGenTbSqlEvent(ce.GetProcMethod('OnEzdmlGenTbSqlEvent'));
    GProc_OnEzdmlGenDbSqlEvent:= TOnEzdmlGenDbSqlEvent(ce.GetProcMethod('OnEzdmlGenDbSqlEvent'));
    GProc_OnEzdmlGenFieldTypeDescEvent:= TOnEzdmlGenFieldTypeDescEvent(ce.GetProcMethod('OnEzdmlGenFieldTypeDescEvent'));
    GProc_OnEzdmlGenAlterFieldEvent:= TOnEzdmlGenAlterFieldEvent(ce.GetProcMethod('OnEzdmlGenAlterFieldEvent'));
    GProc_OnEzdmlCmdEvent:= TOnEzdmlCmdEvent(ce.GetProcMethod('OnEzdmlCmdEvent'));

    bSuccess := True;
  finally
    if not bSuccess then
      FreeAndNil(FGlobalScriptor);
  end;
  
end;

procedure TfrmMainDml.MN_SaveasClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    TryLockFile(SaveDialog1.FileName);
    SaveToFile(SaveDialog1.FileName);
    TryLockFile(FCurFileName);
    FCurDmlFileName := FCurFileName;
    SetRecentFile(FCurFileName);
  end;
end;

procedure TfrmMainDml.MN_SearchFieldsClick(Sender: TObject);
begin
  // if not Assigned(Proc_ShowCtDmlSearch) then
  //   Exit;
  //Proc_ShowCtDmlSearch(FGlobeDataModelList, nil);
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actFindObject.Execute;
end;

procedure TfrmMainDml.Mn_ShowPhyViewClick(Sender: TObject);
begin
  FFrameCtTableDef.FFrameDMLGraph.FFrameCtDML.actShowPhyView.Execute;
end;

procedure TfrmMainDml.On_ZcDropFileDropFile(APoint: TPoint; AKeyState: Integer;
  bDeleteSrc: Boolean; AFiles: array of string);
var
  L: Integer;
  S: string;
begin
  L := Length(aFiles);
  if L = 0 then
    Exit;
  S := Trim(aFiles[0]);
  FGlobeOpeningFile := S;
  PostMessage(Handle, WMZ_CUSTCMD, 1, 0);
end;

procedure TfrmMainDml.PromptOpenFile(fn: string);
var
  vOldMds: TCtDataModelGraphList;
  I: Integer;
  s, ext: string;
  bCheck: Boolean;
  vFileDate1, vCreateDate1, vFileDate2, vCreateDate2: TDateTime;
begin
  if not FileExists(fn) then
  begin
    RemoveRecentFile(fn);
    raise Exception.Create(Format(srEzdmlFileNotFoundFmt, [fn]));
  end;
  TryLockFile(fn);
  FCtDataModelList.Pack;
  vOldMds := nil;
  try
    if FCtDataModelList.TableCount > 0 then
      case Application.MessageBox(PChar(ExtractFileName(fn) + ' ' + srEzdmlConfirmClearOnOpen),
        PChar(srEzdmlOpenFile), MB_YESNOCANCEL or MB_ICONWARNING) of
        IDYES: ;
        IDNO:
          begin
            vOldMds := TCtDataModelGraphList.Create;
            vOldMds.AssignFrom(FCtDataModelList);
          end
      else
        Exit;
      end;

    s := fn;
    ext := Lowercase(ExtractFileExt(s));
    if ext <> '.tmp' then
    begin
      s := s + '.tmp';
      if FileExists(s) then
      begin              
        bCheck := False;
        if GetFileAges(fn, vFileDate1, vCreateDate1)  and
          GetFileAges(s, vFileDate2, vCreateDate2) then
        begin
          if Abs(vFileDate1-vCreateDate2)>2/24/60/60 then
            bCheck := True;
        end
        else
          bCheck := True;
        if bCheck then
          case (Application.MessageBox(PChar(Format(srEzdmlTmpFileChangedFmt,[s])),
            PChar(srEzdmlOpenFile), MB_YESNOCANCEL or MB_ICONWARNING or MB_DEFBUTTON3)) of
          IDYES: BackupTmpFile(s);
          IDNO: s := fn;
          else
            Abort;
          end;
        LoadFromFile(s);
        FCurFileName := fn;
        CheckCaption;
      end
      else
        LoadFromFile(fn);
    end
    else
      LoadFromFile(fn);
    FCurDmlFileName := fn;

    if Assigned(vOldMds) then
    begin
      for I := 0 to vOldMds.Count - 1 do
        FCtDataModelList.NewModelItem.AssignFrom(vOldMds[I]);
      FFrameCtTableDef.Init(FCtDataModelList, False);
    end;

    SetRecentFile(fn);
  finally
    if Assigned(vOldMds) then
      vOldMds.Free;
  end;
end;

procedure TfrmMainDml.RecreateRecentMn;
var
  mn: TMenuItem;
  I: Integer;
  fn: string;
begin
  MN_Recentfiles.Clear;
  for I := 0 to FRecentFiles.Count - 1 do
  begin
    fn := FRecentFiles[I];
    mn := TMenuItem.Create(Self);
    mn.Caption := ExtractFileName(fn);
    mn.Hint := fn;
    mn.Tag := I;
    mn.OnClick := _OnRecentFileClick;
    MN_Recentfiles.Add(mn);
  end;
end;

procedure TfrmMainDml.RemoveRecentFile(fn: string);
var
  I: Integer;
  S: string;
begin
  S := LowerCase(fn);
  for I := 0 to FRecentFiles.Count - 1 do
    if LowerCase(FRecentFiles[I]) = S then
    begin
      FRecentFiles.Delete(I);
      SaveIni;
      RecreateRecentMn;
      Exit;
    end;
end;

initialization
  FCreateSeqForOracle := True;
  FBackupBeforeAlterColumn := False;
  Proc_CheckStringMaxLen := CheckStringMaxLen;
  Proc_CheckCustDataTypeReplaces := CheckCustDataTypeReplaces;
  Proc_OnExecAppCmd := EzdmlExecAppCmd;

end.

