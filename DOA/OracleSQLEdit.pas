// Direct Oracle Access - SQL/Command Editor unit
// Copyright 1998 - 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleSQLEdit;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion6} Variants, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Grids, ExtCtrls, Printers, Menus,
  {$IFNDEF NODATASET} OracleData, {$ENDIF}
  Oracle, OracleVarEdit, OraclePreferences, OracleQB, OracleVisual;
{$ELSE}
uses
  Variants, Types, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QButtons, QGrids, QExtCtrls, QMenus, Qt,
  {$IFNDEF NODATASET} OracleData, {$ENDIF}
  Oracle, OracleVarEdit, OraclePreferences, OracleVisual;
{$ENDIF}

type
  TSQLEditForm = class(TForm)
    StatusBar: TStatusBar;
    TopPanel: TPanel;
    LogonBtn: TSpeedButton;
    ExecuteBtn: TSpeedButton;
    DescribeBtn: TSpeedButton;
    VariablesBtn: TSpeedButton;
    SQLEditSession: TOracleSession;
    SQLEditLogon: TOracleLogon;
    SQLEditQuery: TOracleQuery;
    PBox: TPaintBox;
    RightPanel: TPanel;
    ExitBtn: TSpeedButton;
    OkayBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    OpenHelp: TOpenDialog;
    LoadBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    OpenSQL: TOpenDialog;
    SaveSQL: TSaveDialog;
    FontDialog: TFontDialog;
    PrintBtn1: TSpeedButton;
    SetupBtn: TSpeedButton;
    BreakBtn: TSpeedButton;
    PrintBtn2: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    SetupPopup: TPopupMenu;
    TextFont: TMenuItem;
    ListFont: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    N1: TMenuItem;
    PrinterSetup: TMenuItem;
    ExportBtn: TSpeedButton;
    FirstBtn: TSpeedButton;
    PrevBtn: TSpeedButton;
    NextBtn: TSpeedButton;
    LastBtn: TSpeedButton;
    InsertBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    ScriptScrollBar: TScrollBar;
    TabControl: TTabControl;
    ListView: TListView;
    SplitterPanel: TPanel;
    SQLEdit: TRichEdit;
    SQLEditScript: TOracleScript;
    CommandResults: TRichEdit;
    QBBtn: TSpeedButton;
    procedure VariablesBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LogonBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure OkayBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure DescribeBtnClick(Sender: TObject);
    procedure SQLEditSessionChange(Sender: TOracleSession);
    procedure FormCreate(Sender: TObject);
    procedure DropIt(Sender, Source: TObject; X, Y: Integer);
    procedure DragIt(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SQLEditChange(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure PrintBtn1Click(Sender: TObject);
    procedure TextFontClick(Sender: TObject);
    procedure ListFontClick(Sender: TObject);
    procedure ThreadRecord(Sender: TOracleQuery);
    procedure ThreadFinished(Sender: TOracleQuery);
    procedure ThreadError(Sender: TOracleQuery; ErrorCode: Integer; const ErrorMessage: String);
    procedure BreakBtnClick(Sender: TObject);
    procedure ThreadExecuted(Sender: TOracleQuery);
    procedure PrinterSetupClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure LastBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ScriptScrollBarChange(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure SQLEditScriptCommand(Sender: TOracleScript; var Handled: Boolean);
    procedure SQLEditScriptData(Sender: TOracleScript);
    procedure FormDestroy(Sender: TObject);
    procedure QBBtnClick(Sender: TObject);
  public
    Start: Integer;
    Drag: Boolean;
    SQLHelpFile: string;
    SQLDir, SQLFile: string;
    CommandIndex: Integer;
    AllowChange: Boolean;
    ScriptMode: Boolean;
    TextEditMode: Boolean;
    function  LoggedOn: Boolean;
    procedure GotoError;
    function  GetCursorWord: String;
    procedure PrintListView(const PrintTitle: string; Mode: Integer);
    procedure SetFonts;
    function  SelectFont(Font: TFont; Section: string): Boolean;
    {$IFNDEF LINUX}
    procedure HandlePLSQLDevMsg(wParam, lParam: integer);
    procedure DefaultHandler(var Message); override;
    {$ENDIF}
    procedure StoreCommand;
    procedure CheckCommandIndex;
    procedure DisplayCommand;
    procedure SetSQLEdit(S: string);
    function  CanDescribe: Boolean;
  end;

function ExecuteEditor(var S: string): Boolean;
function ExecuteSQLEditor(Q: TOracleQuery): Boolean;
function ExecuteCommandEditor(Q: TOracleScript): Boolean;

implementation

{$R *.dfm}

const
  Executing = 'Executing...';

const // PL/SQL Developer interface
  QueryName: string         = '';
  PLSQLDevId                = 'PLSQLDevInterface';
  PLSQLDevMsg: Cardinal     = 0;
  wm_QueryAvailable         = 1000;
  wm_PLSQLDevQueryAvailable = 1001;

var
  LeftMargin: Integer = 1;
  TopMargin:  Integer = 1;

// PL/SQL Developer interface functions

{$IFNDEF LINUX}
procedure SendToPLSQLDev(Q: TOracleQuery);
var MHandle, WHandle: THandle;
    Data: Pointer;
    Size: Integer;
    S, Exe, Param: string;
    i: Integer;
    v: Variant;
 procedure AddString(SubString: string);
 begin
   S := S + SubString + #0;
 end;
begin
//  PLSQLDevMsg := RegisterWindowMessage(PLSQLDevId);
  // Build the string that is going to get transmitted to PL/SQL Developer
  S := '';
  AddString('Direct Oracle Access');       // First an application name
  AddString(QueryName);                    // Object name
  AddString(Q.SQL.Text);                   // The query text
  for i := 0 to Q.VariableCount - 1 do     // and the variables
  begin                                    //   name, type and value
    AddString(Q.VariableName(i));
    AddString(IntToStr(Q.VariableType(i)));
    v := Q.GetVariable(Q.VariableName(i));
    if VarIsNull(v) or VarIsEmpty(v) then AddString('') else AddString(string(v));
  end;
  Size := Length(S);
  // Create a block of shared memory
  MHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, PLSQLDevId);
  if MHandle <> 0 then
  begin
    Data := MapViewOfFile(MHandle, FILE_MAP_WRITE, 0, 0, Size);
    if Data <> nil then
    begin
      Move(S[1], Data^, Size);
      // Send a message that a query is available
      WHandle := FindWindow('TPLSQLDevForm', nil);
      // If not active, try to wake him up
      if WHandle = 0 then
      begin
        Exe := ReadRegString(HKEY_CLASSES_ROOT, 'PL/SQL Developer\Shell\Open\Command', '');
        Param := Q.Session.LogonUsername + '/' + Q.Session.LogonPassword;
        if Q.Session.LogonDatabase <> '' then Param := Param + '@' + Q.Session.LogonDatabase;
        WinExec(PChar(Exe + ' userid=' + Param), SW_SHOWNORMAL);
        Sleep(500); // Give PL/SQL Developer a moment
        WHandle := FindWindow('TPLSQLDevForm', nil);
      end;
      if WHandle <> 0 then SendMessage(WHandle, PLSQLDevMsg, wm_QueryAvailable, Size);
      UnmapViewOfFile(Data);
    end;
    CloseHandle(MHandle);
  end;
end;

procedure TSQLEditForm.HandlePLSQLDevMsg(wParam, lParam: integer);
var Handle: THandle;
    Data: Pointer;
    S: string;
    Product, Title: string;
    index: Integer;
    vName, vType, vValue: string;
function GetString(const S: string): string;
begin
  Result := '';
  inc(index);
  while (index <= Length(S)) and (S[index] <> #0) do
  begin
    Result := Result + S[index];
    inc(index);
  end;
end;
begin
  if wParam = wm_PLSQLDevQueryAvailable then
  begin
    S := '';
    Handle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, lParam, PLSQLDevId);
    if (Handle <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
      Data := MapViewOfFile(Handle, FILE_MAP_READ, 0, 0, lParam);
      SetLength(S, lParam);
      Move(Data^, S[1], lParam);
      UnmapViewOfFile(Data);
    end;
    if Handle <> 0 then CloseHandle(Handle);
    if S <> '' then
    begin
      index := 0;
      Product := GetString(S);
      Title := GetString(S);
      SQLEdit.Text := GetString(S);
      SQLEditQuery.DeleteVariables;
      repeat
        vName  := GetString(S);
        vType  := GetString(S);
        vValue := GetString(S);
        if vName <> '' then
        begin
          try
            SQLEditQuery.DeclareVariable(vName, StrToInt(vType));
            if vValue <> '' then SQLEditQuery.SetVariable(vName, vValue);
          except
          end;
        end;
      until vName = '';
      Application.BringToFront;
    end;
  end;
end;

procedure TSQLEditForm.DefaultHandler(var Message);
begin
  inherited DefaultHandler(Message);
  with TMessage(Message) do
  begin
    if Msg = PLSQLDevMsg then
    begin
      HandlePLSQLDevMsg(wParam, lParam);
      Result := 0;
    end;
  end;
end;
{$ENDIF}

// Start the plain text Editor
function ExecuteEditor(var S: string): Boolean;
var SQLForm: TSQLEditForm;
    i: Integer;
    P: pointer;
begin
  Result := False;
  P := @SetDefaults;
  SetDefaults := nil;
  Application.CreateForm(TSQLEditForm, SQLForm);
  with SQLForm do
  begin
    ScriptMode := False;
    ScriptMode := True;
    for i := SQLForm.ComponentCount - 1 downto 0 do
    begin
      if Components[i].Tag < 0 then
        TControl(Components[i]).Visible := False
      else begin
        if Components[i] is TSpeedButton then
        begin
          if Components[i].Tag <> 2 then
            TSpeedButton(Components[i]).Left := TSpeedButton(Components[i]).Left - LoadBtn.Left + 4;
        end;
      end;
    end;
    SQLEdit.Align := alClient;
    {$IFDEF LINUX}
    TabControl.Tabs.Clear;
    {$ELSE}
    TabControl.Tabs.Text := '';
    {$ENDIF}
    SetFonts;
    SQLEdit.Text := S;
    Caption := 'Editor';
    OkayBtn.Enabled := False;
    if (ShowModal = mrOK) then
    begin
      S := SQLEdit.Text;
      Result := True;
    end;
    Free;
  end;
  SetDefaults := P;
end;

// Start the SQL Editor
function ExecuteSQLEditor(Q: TOracleQuery): Boolean;
var SQLForm: TSQLEditForm;
begin
  Result := False;
  Application.CreateForm(TSQLEditForm, SQLForm);
  with SQLForm do
  begin
    ScriptMode := False;
    TextEditMode := False;
    {$IFDEF LINUX}
    TabControl.Tabs.Clear;
    {$ELSE}
    TabControl.Tabs.Text := '';
    {$ENDIF}
    SetFonts;
    SQLEditQuery.SQL := Q.SQL;
    CopyVariables(Q, SQLEditQuery);
    SQLEditQuery.StringFieldsOnly := Q.StringFieldsOnly;
    // Select a helpfile if defined
    if OpenRegistry('') then
    begin
      SQLHelpFile := ReadString('SQLHelp', '');
      CloseRegistry;
    end;
    if Q.Session <> nil then
    begin
      if Q.Session.Connected then
      begin
        LogonBtn.Enabled := False;
        SQLEditQuery.Session := Q.Session;
      end else begin
        // Read logon settings from registry
        SQLEditSession.LogonUsername := Q.Session.LogonUsername;
        SQLEditSession.LogonPassword := Q.Session.LogonPassword;
        SQLEditSession.LogonDatabase := Q.Session.LogonDatabase;
      end;
      SQLEditSessionChange(Q.Session);
    end else begin
      if OpenRegistry('Logon') then
      begin
        SQLEditSession.LogonUsername := ReadString('Username', '');
        SQLEditSession.LogonPassword := ReadString('Password', '');
        SQLEditSession.LogonDatabase := ReadString('Database', '');
        CloseRegistry;
      end;
    end;
    SQLEdit.Lines := Q.SQL;
    SQLEdit.SelStart := 0;
    {$IFNDEF NODATASET}
    if Q.Owner is TOracleDataSet then
    begin
      QueryName := Q.Owner.Name;
      Caption := Q.Owner.Owner.Name + '.' + QueryName + ' SQL Editor';
    end else begin
      QueryName := Q.Name;
      Caption := Q.Owner.Name + '.' + QueryName + ' SQL Editor';
    end;
    {$ELSE}
    QueryName := Q.Name;
    Caption := Q.Owner.Name + '.' + QueryName + ' SQL Editor';
    {$ENDIF}
    OkayBtn.Enabled := False;
    if (ShowModal = mrOK) then
    begin
      Q.SQL := SQLEdit.Lines;
      CopyVariables(SQLEditQuery, Q);
      Result := True;
    end;
    Free;
  end;
end;

// Start the Script Command Editor
function ExecuteCommandEditor(Q: TOracleScript): Boolean;
var SQLForm: TSQLEditForm;
  procedure AdjustControl(C: TControl);
  var D: Integer;
  begin
    D := SQLForm.FirstBtn.Left - SQLForm.ExportBtn.Left;
    C.Visible := True;
    C.Left := C.Left - D;
  end;
begin
  Result := False;
  Application.CreateForm(TSQLEditForm, SQLForm);
  with SQLForm do
  begin
    ScriptMode := True;
    TextEditMode := False;
    ExportBtn.Visible := False;
    HelpBtn.Visible := False;
    AdjustControl(ScriptScrollBar);
    AdjustControl(PrevBtn);
    AdjustControl(NextBtn);
    AdjustControl(LastBtn);
    AdjustControl(InsertBtn);
    AdjustControl(DeleteBtn);
    AdjustControl(FirstBtn);
    TabControl.TabIndex := 0;
    CommandIndex := 0;

    CommandResults.Visible := True;
    CommandResults.Align := alClient;
    CommandResults.Clear;

    CheckCommandIndex;
    SetFonts;
    VariablesBtn.Enabled := False;
    SQLEditScript.Lines := Q.Lines;
    SQLEditScript.AutoCommit := Q.AutoCommit;
    SQLEditScript.ExitOnError := Q.ExitOnError;
    SQLEditScript.OutputOptions := Q.OutputOptions;
    // Select a helpfile if defined
    if OpenRegistry('') then
    begin
      SQLHelpFile := ReadString('SQLHelp', '');
      CloseRegistry;
    end;
    if Q.Session <> nil then
    begin
      if Q.Session.Connected then
      begin
        LogonBtn.Enabled := False;
        SQLEditQuery.Session  := Q.Session;
        SQLEditScript.Session := Q.Session;
      end else begin
        // Read logon settings from registry
        SQLEditSession.LogonUsername := Q.Session.LogonUsername;
        SQLEditSession.LogonPassword := Q.Session.LogonPassword;
        SQLEditSession.LogonDatabase := Q.Session.LogonDatabase;
      end;
      SQLEditSessionChange(Q.Session);
    end else begin
      if OpenRegistry('Logon') then
      begin
        SQLEditSession.LogonUsername := ReadString('Username', '');
        SQLEditSession.LogonPassword := ReadString('Password', '');
        SQLEditSession.LogonDatabase := ReadString('Database', '');
        CloseRegistry;
      end;
    end;
    SQLEdit.Lines := SQLEditScript.Lines;
    SQLEdit.SelStart := 0;
    QueryName := Q.Name;
    Caption := Q.Owner.Name + '.' + QueryName + ' Command Editor';
    OkayBtn.Enabled := False;
    DescribeBtn.Enabled := CanDescribe;
    if (ShowModal = mrOK) then
    begin
      if (TabControl.TabIndex = 1) then
        StoreCommand
      else
        SQLEditScript.Lines := SQLEdit.Lines;
      TabControl.TabIndex := 0;
      Q.Lines := SQLEditScript.Lines;
      Result := True;
    end;
    Free;
  end;
end;

// The SQL Editor mainform

// The session has connected or disconnected
procedure TSQLEditForm.SQLEditSessionChange(Sender: TOracleSession);
begin
  if Sender.Connected then
    StatusBar.SimpleText := 'Connected'
  else
    StatusBar.SimpleText := 'Not connected';
end;

// Read settings from HKEY_CURRENT_USER/Software/DOA/SQL Editor
procedure TSQLEditForm.FormCreate(Sender: TObject);
var h: Integer;
begin
  {$IFDEF LINUX}
  PrintBtn1.Enabled := False;
  PrintBtn2.Enabled := False;
  SetupBtn.Enabled  := False;
  ExportBtn.Enabled := False;
  HelpBtn.Enabled   := False;
  {$ELSE}
  QBCreate(Self.Handle);
  QBBtn.Enabled := CanQB;
  PLSQLDevMsg := RegisterWindowMessage(PLSQLDevId);
  ChangeGlyphInit(Self);
  ChangeGlyphs(TopPanel);
  ChangeGlyphs(RightPanel);
  ChangeGlyphClose;
  {$ENDIF}
  if OpenRegistry('SQL Editor') then
  begin
    Left := ReadInteger('Left', Left);
    Top := ReadInteger('Top', Top);
    Width := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    h := ReadInteger('Splitter', SQLEdit.Height);
    if h > Height - (TopPanel.Height * 4) then h := Height - (TopPanel.Height * 4);
    if h < 20 then h := 20;
    SQLEdit.Height := h;
    WindowState := TWindowState(ReadInteger('State', Ord(WindowState)));
    SQLDir := ReadString('SQL Files', SQLDir);
    CloseRegistry;
  end;
  Start := -1;
  Drag := False;
  AllowChange := True;
end;

procedure TSQLEditForm.FormActivate(Sender: TObject);
begin
  InitForm(Self);
end;

// Save settings on exit
procedure TSQLEditForm.FormDestroy(Sender: TObject);
begin
  SQLEditQuery.BreakThread;
  if not TextEditMode then
  begin
    if OpenRegistry('SQL Editor') then
    begin
      if WindowState = wsNormal then
      begin
        WriteInteger('Left', Left);
        WriteInteger('Top', Top);
        WriteInteger('Width', Width);
        WriteInteger('Height', Height);
      end;
      WriteInteger('State', Ord(WindowState));
      WriteInteger('Splitter', SQLEdit.Height);
      WriteString('SQL Files', SQLDir);
      CloseRegistry;
    end;
  end;
  {$IFNDEF LINUX}
  QBFree;
  {$ENDIF}
end;

// Return if the session is logged on, tries to logon if it's not
function TSQLEditForm.LoggedOn: Boolean;
begin
  if not SQLEditQuery.Session.Connected then
  begin
    InitOracleLogon(SQLEditLogon);
    SQLEditLogon.Execute;
    StoreLogonParams(SQLEditQuery.Session);
  end;
  Result := SQLEditQuery.Session.Connected
end;

// Manually LogOn or LogOff
procedure TSQLEditForm.LogonBtnClick(Sender: TObject);
begin
  SQLEditSession.LogOff;
  SQLEditLogon.Options := SQLEditLogon.Options - [ldAuto];
  LoggedOn;
end;

// Set the cursor at the Errorposition
procedure TSQLEditForm.GotoError;
{$IFNDEF LINUX}
var p: Integer;
{$ENDIF}
begin
  {$IFNDEF LINUX}
  if SQLEditQuery.ErrorLine > 0 then
  begin
    p := SQLEdit.Perform(em_LineIndex, SQLEditQuery.ErrorLine - 1, 0);
    SQLEdit.SelLength := 0;
    SQLEdit.SelStart := p + SQLEditQuery.ErrorPosition - 1;
  end;
  {$ENDIF}
end;

// Execute a query and display the results
procedure TSQLEditForm.ExecuteBtnClick(Sender: TObject);
 procedure SetExecuteMode;
 begin
   StatusBar.SimpleText := Executing;
   Screen.Cursor        := crAppStart;
   BreakBtn.Enabled     := True;
   ExecuteBtn.Enabled   := False;
   DescribeBtn.Enabled  := False;
 end;
 function Normalize(const S: string): string;
 var c: Integer;
 begin
   Result := S;
   for c := 1 to Length(S) do if S[c] < #32 then Result[c] := ' ';
 end;
begin
  if not LoggedOn then Exit;
  if not ScriptMode then
  begin
    ListView.Items.BeginUpdate;
    ListView.Columns.Clear;
    ListView.Items.Clear;
    {$IFDEF LINUX}
    ListView.Sorted := False;
    {$ELSE}
    ListView.SortType := stNone;
    {$ENDIF}
    ListView.Items.EndUpdate;
    ListView.Refresh;
    if not SQLEditQuery.ThreadIsRunning then
    begin
      SetExecuteMode;
      SQLEditQuery.SQL.Text := SQLEdit.Text;
      SQLEditQuery.Execute;
    end;
  end else begin
    StatusBar.SimpleText := '';
    SQLEditScript.Output.Clear;
    CommandResults.Clear;
    case TabControl.TabIndex of
      0 : begin
            SetExecuteMode;
            SQLEditScript.Lines := SQLEdit.Lines;
            SQLEditScript.Execute;
            ThreadFinished(nil);
          end;
      1 : begin
            if CommandIndex < SQLEditScript.Commands.Count then
            begin
              SetExecuteMode;
              StoreCommand;
              SQLEditScript.Commands[CommandIndex].Execute;
              ThreadFinished(nil);
            end;
          end;
    end;
    CommandResults.Lines.BeginUpdate;
    CommandResults.SelText := SQLEditScript.Output.Text;
    CommandResults.Lines.EndUpdate;
  end;
end;

procedure TSQLEditForm.BreakBtnClick(Sender: TObject);
begin
  if ScriptMode then
    SQLEditScript.Finished := True
  else
    SQLEditQuery.BreakThread;
end;

// Describe a query to search for errors
procedure TSQLEditForm.DescribeBtnClick(Sender: TObject);
var ft: Integer;
begin
  if not LoggedOn then Exit;
  try
    if ScriptMode and (TabControl.TabIndex = 1) then StoreCommand;
    SQLEditQuery.SQL.Text := SQLEdit.Text;
    SQLEditQuery.Describe;
    ft := SQLEditQuery.FunctionType;
    if ft <> 4 then
      StatusBar.SimpleText := 'Cannot parse this SQL statement'
    else
      StatusBar.SimpleText := 'OK';
  except
    on E:Exception do
    begin
      GotoError;
      StatusBar.SimpleText := E.Message;
      ShowMessage(StatusBar.SimpleText);
    end;
  end;
end;

// Start the variables editor
procedure TSQLEditForm.VariablesBtnClick(Sender: TObject);
begin
  SQLEditQuery.SQL.Text := SQLEdit.Text;
  if ExecuteVariablesEditor(SQLEditQuery, '') then OkayBtn.Enabled := True;
end;

// Load SQL text
procedure TSQLEditForm.LoadBtnClick(Sender: TObject);
begin
  with OpenSQL do
  begin
    InitialDir := SQLDir;
    Filename := SQLFile;
    if Execute then
    begin
      SQLDir  := ExtractFilePath(Filename);
      SQLFile := ExtractFilename(Filename);
      SQLEdit.Lines.LoadFromFile(Filename);
    end;
  end;
end;

// Save SQL text
procedure TSQLEditForm.SaveBtnClick(Sender: TObject);
begin
  with SaveSQL do
  begin
    InitialDir := SQLDir;
    Filename := SQLFile;
    if Execute then
    begin
      SQLDir  := ExtractFilePath(Filename);
      SQLFile := ExtractFilename(Filename);
      SQLEdit.Lines.SaveToFile(Filename);
    end;
  end;
end;

procedure TSQLEditForm.ExportBtnClick(Sender: TObject);
begin
  {$IFNDEF LINUX}
  SQLEditQuery.SQL.Text := SQLEdit.Text;
  SendToPLSQLDev(SQLEditQuery);
  {$ENDIF}
end;

// Return the word the cursor is on
function TSQLEditForm.GetCursorWord: String;
{$IFNDEF LINUX}
const IdentSet: set of Char = ['.', 'a'..'z', 'A'..'Z', '0'..'9', '_', '#', '$'];
var i, j, p: Integer;
    s: string;
{$ENDIF}    
begin
  {$IFNDEF LINUX}
  p := SQLEdit.Perform(em_LineFromChar, SQLEdit.SelStart, 0);
  s := SQLEdit.Lines[p];
  i := SQLEdit.SelStart - SQLEdit.Perform(em_LineIndex, p, 0) + 1;
  while (i > 1) and (s[i - 1] in IdentSet) do Dec(i);
  j := i;
  while (j <= Length(s)) and (s[j] in IdentSet) do Inc(j);
  Result := Copy(s, i, j - i);
  {$ENDIF}
end;

// Open help
procedure TSQLEditForm.HelpBtnClick(Sender: TObject);
var S: string;
begin
  if SQLHelpFile = '' then
  begin
    ShowMessage('This is the first time you use SQL help.' + #13#10 +
                'You must now select the helpfile you want to use...');
    if OpenOracleRegistry('') then
    begin
      S := ReadString('EXECUTE_SQL', '');
      if S <> '' then OpenHelp.InitialDir := ReadString(S, '');
      CloseRegistry;
    end;
    if OpenHelp.Execute then
    begin
      SQLHelpFile := OpenHelp.Filename;
      if OpenRegistry('') then
      begin
        WriteString('SQLHelp', SQLHelpFile);
        CloseRegistry;
      end;
    end;
  end;
  {$IFNDEF LINUX}
  if SQLHelpFile <> '' then
  begin
    WinHelp(Handle, PChar(SQLHelpFile), Help_Key, LongInt(PChar(GetCursorWord)));
  end;
  {$ENDIF}
end;

// Invoke help when F1 is pressed
procedure TSQLEditForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = vk_F1 then
  begin
    HelpBtnClick(nil);
    Key := 0;
  end;
  if Key = vk_F8 then
  begin
    if ExecuteBtn.Enabled then ExecuteBtnClick(nil);
    Key := 0;
  end;
  if Key = vk_Escape then
  begin
    if BreakBtn.Enabled then BreakBtnClick(nil);
    Key := 0;
  end;
  if ssCtrl in Shift then
  begin
    if Key = Ord('R') then
    begin
      if ExecuteBtn.Enabled then ExecuteBtnClick(nil);
      Key := 0;
    end;
    if Key = Ord('D') then
    begin
      if DescribeBtn.Enabled then DescribeBtnClick(nil);
      Key := 0;
    end;
    if Key = Ord('P') then
    begin
      if VariablesBtn.Enabled then VariablesBtnClick(nil);
      Key := 0;
    end;
    if Key = 13 then
    begin
      if OkayBtn.Enabled then OkayBtnClick(nil);
      Key := 0;
    end;
  end;
end;

// Two events needed for the splitter. Delphi 3 has a splitter component but
// we wanted to be Delphi 2 compatible, and this one is smooth
procedure TSQLEditForm.DropIt(Sender, Source: TObject; X, Y: Integer);
begin
  Start := -1;
  Drag := False;
end;

procedure TSQLEditForm.DragIt(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var Position: Integer;
begin
  Accept := (Source = SplitterPanel);
  if Accept then
  begin
    Drag := True;
    Position := ScreenToClient(TControl(Sender).ClientToScreen(Point(X, Y))).Y;
    if Start < 0 then Start := Position;
    if (Start - Position) <> 0 then
    begin
      LockWindowUpdate(Integer(Handle));
      StatusBar.Visible := False;
      SQLEdit.Height := SQLEdit.Height - (Start - Position);
      StatusBar.Visible := True;
      LockWindowUpdate(0);
      Refresh;
    end;
    Start := Position;
  end;
end;

procedure TSQLEditForm.SQLEditChange(Sender: TObject);
begin
  if SQLEdit.Modified and AllowChange then OkayBtn.Enabled := True;
end;

procedure TSQLEditForm.SetSQLEdit(S: string);
begin
  AllowChange := False;
  SQLEdit.Lines.BeginUpdate;
  SQLEdit.Lines.Text := S;
  SQLEdit.SelStart   := 0;
  SQLEdit.Lines.EndUpdate;
  SQLEdit.Modified   := False;
  {$IFNDEF LINUX}
  SQLEdit.Perform(EM_SCROLLCARET, 0, 0);
  {$ENDIF}
  AllowChange := True;
end;

procedure TSQLEditForm.OkayBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSQLEditForm.ExitBtnClick(Sender: TObject);
var Key: Integer;
    S: string;
begin
  if OkayBtn.Enabled then
  begin
    S := 'Save changes to ' + QueryName + '?';
    Key := Confirm(S, 'Confirm', 'YNC');
    case Key of
      IDYES : ModalResult := mrOK;
       IDNO : ModalResult := mrCancel;
    end;
  end else
    ModalResult := mrCancel;
end;

function StyleToInt(Style: TFontStyles): Integer;
begin
  Result := 0;
  if fsBold in Style then Inc(Result, 1);
  if fsItalic in Style then Inc(Result, 2);
  if fsUnderline in Style then Inc(Result, 4);
  if fsStrikeOut in Style then Inc(Result, 8);
end;

function IntToStyle(i: Integer): TFontStyles;
begin
  Result := [];
  if i and 1 <> 0 then Result := Result + [fsBold];
  if i and 2 <> 0 then Result := Result + [fsItalic];
  if i and 4 <> 0 then Result := Result + [fsUnderline];
  if i and 8 <> 0 then Result := Result + [fsStrikeOut];
end;

procedure TSQLEditForm.SetFonts;
begin
  {$IFNDEF LINUX}
  if OpenRegistry('SQL Editor\TextFont') then
  begin
    SQLEdit.Font.Name  := ReadString('Name', 'Courier New');
    SQLEdit.Font.Color := ReadInteger('Color', clWindowText);
    SQLEdit.Font.Size  := ReadInteger('Size', 8);
    SQLEdit.Font.Style := IntToStyle(ReadInteger('Style', 0));
    CloseRegistry;
  end;
  if OpenRegistry('SQL Editor\ListFont') then
  begin
    ListView.Font.Name  := ReadString('Name', 'MS Sans Serif');
    ListView.Font.Color := ReadInteger('Color', clWindowText);
    ListView.Font.Size  := ReadInteger('Size', 8);
    ListView.Font.Style := IntToStyle(ReadInteger('Style', 0));
    CommandResults.Font := ListView.Font;
    CloseRegistry;
    PBox.Font := ListView.Font;
  end;
  {$ENDIF}
end;

function TSQLEditForm.SelectFont(Font: TFont; Section: string): Boolean;
begin
  Result := False;
  FontDialog.Font := Font;
  if FontDialog.Execute then
  begin
    if OpenRegistry(Section) then
    begin
      WriteString('Name', FontDialog.Font.Name);
      WriteInteger('Color', FontDialog.Font.Color);
      WriteInteger('Size', FontDialog.Font.Size);
      WriteInteger('Style', StyleToInt(FontDialog.Font.Style));
      CloseRegistry;
    end;
    Result := True;
  end;
end;

procedure TSQLEditForm.SetupBtnClick(Sender: TObject);
var P: TPoint;
begin
  P := ClientToScreen(Point(SetupBtn.Left, SetupBtn.Top + SetupBtn.Height));
  SetupPopup.Popup(P.X, P.Y);
end;

procedure TSQLEditForm.PrintListView(const PrintTitle: string; Mode: Integer);
{$IFDEF LINUX}
begin
end;
{$ELSE}
var y, r, rh, space: Integer;
    v: Variant;
    S: string;
    XF: Double;
 procedure PrintStat(Row: Integer);
 var x, c, f: Integer;
     Rect: TRect;
     ListItem: TListItem;
 begin
   ListItem := ListView.Items[Row];
   Rect.Top := y;
   Rect.Bottom := y + rh;
   Rect.Left := LeftMargin + space;
   Rect.Right := Round(LeftMargin + XF * ListView.Columns[0].Width) + space;
   DrawText(Printer.Canvas.Handle, PChar(ListItem.Caption), -1, Rect, dt_NoPrefix);
   x := Rect.Right;
   for c := 0 to ListItem.SubItems.Count - 1 do
   begin
     Rect.Left := x + space;
     Rect.Right := x + Round(XF*ListView.Columns[c + 1].Width) + space;
     f := dt_NoPrefix;
     if ListView.Columns[c + 1].Alignment = taRightJustify then f := f + dt_Right;
     if ListView.Columns[c + 1].Alignment = taCenter then f := f + dt_Center;
     DrawText(Printer.Canvas.Handle, PChar(ListItem.SubItems[c]), -1, Rect, f);
     x := Rect.Right;
   end;
   y := Rect.Bottom;
 end;
 procedure PrintStatHeader;
 var x, c, f: Integer;
     Rect: TRect;
 begin
   Rect.Top := y;
   Rect.Bottom := y + rh;
   Rect.Left := LeftMargin;
   x := Rect.Left;
   for c := 0 to ListView.Columns.Count - 1 do
   begin
     Rect.Left := x + space;
     Rect.Right := x + Round(XF*ListView.Columns[c].Width) + space;
     f := dt_NoPrefix;
     if ListView.Columns[c].Alignment = taRightJustify then f := f + dt_Right;
     if ListView.Columns[c].Alignment = taCenter then f := f + dt_Center;
     DrawText(Printer.Canvas.Handle, PChar(ListView.Columns[c].Caption), -1, Rect, f);
     x := Rect.Right;
   end;
   y := Rect.Bottom;
 end;
 function VarType(BufType: Integer): string;
 begin
   Result := '';
   case BufType of
      otInteger: Result := 'Integer';
        otFloat: Result := 'Float';
       otString: Result := 'String';
         otDate: Result := 'Date';
         otLong: Result := 'Long';
      otLongRaw: Result := 'LongRaw';
       otCursor: Result := 'Cursor';
         otCLOB: Result := 'CLOB';
         otBLOB: Result := 'BLOB';
        otBFile: Result := 'BFile';
    otReference: Result := 'Reference';
       otObject: Result := 'Object';
  otPLSQLString: Result := 'PLSQLString';
         otChar: Result := 'Char';
        otSubst: Result := 'Subst';
    otTimestamp: Result := 'Timestamp';
  otTimestampTZ: Result := 'Timestamp with Time Zone';
 otTimestampLTZ: Result := 'Timestamp with Local Time Zone';
   end;
   if Result <> '' then Result := ' (' + Result + ')';
 end;
begin
  with Printer do
  begin
    Title := PrintTitle;
    if Mode = 0 then Orientation := poPortrait;
    if Mode = 1 then Orientation := poLandscape;
    BeginDoc;
    XF := GetDeviceCaps(Printer.Handle, LogPixelsX) / Screen.PixelsPerInch;
    y  := -1;
    Canvas.Font := SQLEdit.Font;
    rh := Canvas.TextHeight('Hj');
    for r := 0 to SQLEdit.Lines.Count - 1 do
    begin
      Canvas.TextOut(LeftMargin, y, SQLEdit.Lines[r]);
      inc(y, rh);
    end;

    if VariablesBtn.Enabled and (SQLEditQuery.VariableCount > 0) then
    begin
      inc(y, 2 * rh);
      Canvas.TextOut(LeftMargin, y, 'Variables:');
      inc(y, rh);
      for r := 0 to SQLEditQuery.VariableCount - 1 do
      begin
        with SQLEditQuery.Variables.Data(r) do
        begin
          v := SQLEditQuery.GetVariable(Name);
          if VarIsEmpty(v) or VarIsNull(v) then
            S := ''
          else
            S := string(v);
          S := Name + VarType(BufType) + ' = ' + S;
          Canvas.TextOut(LeftMargin, y, S);
          inc(y, rh);
        end;
      end;
    end;

    Canvas.Font := ListView.Font;
    rh := Canvas.TextHeight('Hj');
    space := Canvas.TextWidth(' ');
    y := -(y + rh);
    for r := 0 to ListView.Items.Count - 1 do
    begin
      if ((y < 0) or ((y + rh) > PageHeight)) then // Header
      begin
        if y < 0 then
          y := Abs(y)
        else begin
          NewPage;
          y := TopMargin;
        end;
        inc(y, 2 * rh);
        Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        PrintStatHeader;
      end;
      Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
      PrintStat(r);
    end;
    EndDoc;
  end;
end;
{$ENDIF}

procedure TSQLEditForm.PrintBtn1Click(Sender: TObject);
begin
  PrintListView('SQL Editor', TSpeedButton(Sender).Tag);
end;

procedure TSQLEditForm.TextFontClick(Sender: TObject);
begin
  if SelectFont(SQLEdit.Font, 'SQL Editor\TextFont') then SetFonts;
end;

procedure TSQLEditForm.ListFontClick(Sender: TObject);
begin
  if SelectFont(ListView.Font, 'SQL Editor\ListFont') then SetFonts;
end;

procedure TSQLEditForm.ThreadExecuted(Sender: TOracleQuery);
var i: Integer;
    C: TListColumn;
begin
  if SQLEditQuery.FunctionType <> 4 then SQLEditSession.RollBack;
//  LockWindowUpdate(Handle);
  ListView.Items.BeginUpdate;
  for i:=0 to SQLEditQuery.FieldCount - 1 do
  begin
    C := ListView.Columns.Add;
    C.Caption := SQLEditQuery.FieldName(i);
    if (SQLEditQuery.FieldType(i) = otInteger) or (SQLEditQuery.FieldType(i) = otFloat) then
      C.Alignment := taRightJustify;
    C.Width := PBox.Canvas.TextWidth(SQLEditQuery.FieldName(i) + '    ');
  end;
  ListView.Items.EndUpdate;
//  LockWindowUpdate(0);
  ListView.Refresh;
end;

procedure TSQLEditForm.ThreadRecord(Sender: TOracleQuery);
var i, cw, tw: Integer;
    L: TListItem;
    FS: String;
 function FormatField(index:Integer):String;
 var v: Variant;
 begin
   Result:='';
   if SQLEditQuery.FieldIsNull(index) then Exit;
   case SQLEditQuery.FieldType(Index) of
      otBLOB : v := '<BLOB>';
   otLongRaw : v := '<Long Raw>';
   else
     v := SQLEditQuery.Field(index);
   end;
   if VarIsNull(v) or VarIsEmpty(v) then Exit;
   if (SQLEditQuery.FieldType(index) = otInteger) or
      (SQLEditQuery.FieldType(index) = otFloat) then
   begin
     if SQLEditQuery.FieldPrecision(index) = 0 then
       Result := FloatToStr(v)
     else
       Result := FloatToStrF(v, ffFixed, SQLEditQuery.FieldPrecision(index), SQLEditQuery.FieldScale(index));
   end else
     Result := v;
 end;
begin
  L := ListView.Items.Add;
  for i := 0 to SQLEditQuery.FieldCount - 1 do
  begin
    FS := FormatField(i);
    if i = 0 then L.Caption := FS else L.SubItems.Add(FS);
    cw := ListView.Columns[i].Width;
    tw := PBox.Canvas.TextWidth(FS + '    ');
    if tw > 300 then tw := 300;
    if tw > cw then ListView.Columns[i].Width := tw;
  end;
  StatusBar.SimpleText := IntToStr(SQLEditQuery.RowCount) + ' rows processed';
end;

procedure TSQLEditForm.ThreadFinished(Sender: TOracleQuery);
begin
  BreakBtn.Enabled := False;
  ExecuteBtn.Enabled := True;
  DescribeBtn.Enabled := CanDescribe;
  Screen.Cursor := crDefault;
  if StatusBar.SimpleText = Executing then StatusBar.SimpleText := 'OK'
end;

procedure TSQLEditForm.ThreadError(Sender: TOracleQuery; ErrorCode: Integer; const ErrorMessage: String);
begin
  GotoError;
  StatusBar.SimpleText := ErrorMessage;
  ShowMessage(ErrorMessage);
end;

procedure TSQLEditForm.PrinterSetupClick(Sender: TObject);
begin
  {$IFNDEF LINUX}
  PrinterSetupDialog.Execute;
  {$ENDIF}
end;

procedure TSQLEditForm.StoreCommand;
begin
  if SQLEdit.Modified then
  begin
    if (CommandIndex < SQLEditScript.Commands.Count) then
    begin
      SQLEditScript.Commands[CommandIndex].Text := TrimRight(SQLEdit.Text);
    end;
    SQLEdit.Modified := False;
  end;
end;

procedure TSQLEditForm.DisplayCommand;
var S: string;
begin
  S := '';
  if CommandIndex >= SQLEditScript.Commands.Count then
  begin
    SQLEdit.Clear;
    SQLEdit.ReadOnly := True;
    SQLEdit.Modified := False;
  end else begin
    SQLEdit.ReadOnly := False;
    S := ' Command ' + IntToStr(CommandIndex) + ', Name = ''';
    S := S + SQLEditScript.Commands[CommandIndex].CommentProperty('NAME') + '''';
    SetSQLEdit(SQLEditScript.Commands[CommandIndex].Text);
    SQLEdit.Modified := False;
  end;
  StatusBar.SimpleText := S;
end;

procedure TSQLEditForm.CheckCommandIndex;
var OK: Boolean;
begin
  if CommandIndex >= SQLEditScript.Commands.Count then CommandIndex := SQLEditScript.Commands.Count - 1;
  if CommandIndex < 0 then CommandIndex := 0;
  if SQLEditScript.Commands.Count = 0 then
    ScriptScrollBar.Max := 0
  else
    ScriptScrollBar.Max := SQLEditScript.Commands.Count - 1;
  ScriptScrollBar.Position := CommandIndex;
  OK := CommandIndex > 0;
  FirstBtn.Enabled  := OK and (TabControl.TabIndex = 1);
  PrevBtn.Enabled   := OK and (TabControl.TabIndex = 1);
  OK := (SQLEditScript.Commands.Count > 0) and (CommandIndex < SQLEditScript.Commands.Count - 1);
  LastBtn.Enabled   := OK and (TabControl.TabIndex = 1);
  NextBtn.Enabled   := OK and (TabControl.TabIndex = 1);
  OK := (SQLEditScript.Commands.Count > 0);
  InsertBtn.Enabled := (TabControl.TabIndex = 1);
  DeleteBtn.Enabled := OK and (TabControl.TabIndex = 1);
  ScriptScrollbar.Enabled := (TabControl.TabIndex = 1);
end;

procedure TSQLEditForm.FirstBtnClick(Sender: TObject);
begin
  StoreCommand;
  CommandIndex := 0;
  CheckCommandIndex;
  DisplayCommand;
end;

procedure TSQLEditForm.PrevBtnClick(Sender: TObject);
begin
  StoreCommand;
  dec(CommandIndex);
  CheckCommandIndex;
  DisplayCommand;
end;

procedure TSQLEditForm.NextBtnClick(Sender: TObject);
begin
  StoreCommand;
  inc(CommandIndex);
  CheckCommandIndex;
  DisplayCommand;
end;

procedure TSQLEditForm.LastBtnClick(Sender: TObject);
begin
  StoreCommand;
  CommandIndex := SQLEditScript.Commands.Count - 1;
  CheckCommandIndex;
  DisplayCommand;
end;

procedure TSQLEditForm.ScriptScrollBarChange(Sender: TObject);
begin
  StoreCommand;
  CommandIndex := ScriptScrollBar.Position;
  CheckCommandIndex;
  DisplayCommand;
end;

procedure TSQLEditForm.InsertBtnClick(Sender: TObject);
var Command: TOracleCommand;
begin
  StoreCommand;
  Command := SQLEditScript.Commands.Add;
  Command.Text := '';
  CommandIndex := CommandIndex + 1;
  if CommandIndex > 1 then Command.Index := CommandIndex;
  CheckCommandIndex;
  DisplayCommand;
  OkayBtn.Enabled := True;
end;

procedure TSQLEditForm.DeleteBtnClick(Sender: TObject);
begin
  StoreCommand;
  SQLEditScript.Commands.Delete(CommandIndex);
  CheckCommandIndex;
  DisplayCommand;
  OkayBtn.Enabled := True;
end;

procedure TSQLEditForm.TabControlChange(Sender: TObject);
begin
  if TabControl.TabIndex = 0 then
  begin
    StoreCommand;
    CheckCommandIndex;
    SetSQLEdit(SQLEditScript.Lines.Text);
    SQLEdit.ReadOnly := False;
  end else begin
    if SQLEdit.Modified then SQLEditScript.Lines := SQLEdit.Lines;
    CheckCommandIndex;
    DisplayCommand;
  end;
  DescribeBtn.Enabled := CanDescribe;
end;

procedure TSQLEditForm.SQLEditScriptCommand(Sender: TOracleScript; var Handled: Boolean);
begin
  Application.ProcessMessages;
end;

procedure TSQLEditForm.SQLEditScriptData(Sender: TOracleScript);
begin
  Application.ProcessMessages;
end;

function TSQLEditForm.CanDescribe: Boolean;
begin
  {$IFDEF LINUX}
  Result := (TabControl.TabIndex = 1);
  {$ELSE}
  Result := (TabControl.Tabs.Text = '') or (TabControl.TabIndex = 1);
  {$ENDIF}
end;

procedure TSQLEditForm.QBBtnClick(Sender: TObject);
var S: string;
begin
  S := SQLEdit.Text;
  {$IFNDEF LINUX}
  QBCreate(Application.Handle);
  if not CanQB then
  begin
    QBFree;
    ShowMessage('The Query Builder is not installed,' + #13#10 +
                'you can download it for free at www.allroundautomations.com');
    Exit;
  end;
  if DoQueryBuilder(SQLEditQuery.Session, S) then
  begin
    SQLEdit.Text := S;
  end;
  {$ENDIF}
end;

end.
