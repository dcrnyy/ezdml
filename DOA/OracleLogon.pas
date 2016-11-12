// Direct Oracle Access - Logon Dialog
// Copyright 1997, 2006 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleLogon;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, Oracle, Buttons, IniFiles;
{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs, QButtons,
  QStdCtrls, QExtCtrls, QMenus, Qt, IniFiles, Libc, Types, Oracle;
{$ENDIF}
                                             
const // Allow translation of the Logon dialog
  ltLogonTitle:     string = 'Oracle Logon';    // Title of Logon dialog
  ltPasswordTitle:  string = 'Change password'; // Title of Change password dialog
  ltConfirmTitle:   string = 'Confirm';         // Title of password confirmation dialog
  ltUsername:       string = 'Username';
  ltPassword:       string = 'Password';
  ltDatabase:       string = 'Database';
  ltConnectAs:      string = 'Connect as';
  ltNewPassword:    string = 'New password';
  ltOldPassword:    string = 'Old password';
  ltVerify:         string = 'Verification';
  ltVerifyFail:     string = 'Verification failed';
  ltChangePassword: string = 'Do you wish to change your password now?';
  ltExpired:        string = 'Your password has expired';
  ltOKButton:       string = 'OK';
  ltCancelButton:   string = 'Cancel';
  ltHistoryHint:    string = 'Logon history';
  ltMainFont: TFont = nil;                      // Main Dialog font
  ltFontStyle: TFontStyles = [];                // FontStyle of dialog

type
  THistoryItem = class(TCollectionItem)
  public
    Username: string;
    Password: string;
    Database: string;
    ConnectAs: string;
    Fixed: Boolean;
    OriginalIndex: Integer;
    function DisplayString: string;
    function IsMenu: Boolean;
  end;
  THistoryItems = class(TCollection)
  public
    Size: Integer;
    WithPassword: Boolean;
    function  NonFixedCount: Integer;
    procedure Add(Username, Password, Database, ConnectAs: string; Fixed, Sort: Boolean);
    function  Load(Location: string): Boolean;
    function  Save(Location: string): Boolean;
    function  LoadIni(Filename: string): Boolean;
    function  SaveIni(Filename: string): Boolean;
    function  Item(Index: Integer): THistoryItem;
  end;
  TLogOnForm = class(TForm)
    ButtonPanel: TPanel;
    HistoryPopup: TPopupMenu;
    History: TMenuItem;
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox: TPanel;
    CAPanel: TPanel;
    Label4: TLabel;
    ConnectAsList: TComboBox;
    DBPanel: TPanel;
    Label3: TLabel;
    DatabaseEdit: TEdit;
    AliasList: TComboBox;
    Label1: TLabel;
    UsernameEdit: TEdit;
    HistoryBtn: TSpeedButton;
    PasswordEdit: TEdit;
    Label2: TLabel;
    LogonImage: TImage;
    procedure FormActivate(Sender: TObject);
    procedure HistoryBtnClick(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AlignLogonImage(Picture: TPicture);
    function  GetSortedHistory: THistoryItems;
  public
    { Public declarations }
  end;

var
  LogonHistory: THistoryItems;
  NoUserIdParam: Boolean = False;
  DialogMonitor: Integer = -1;
  FailedLogonCount: Integer = 0;
  AllowDuplicateHistory: Boolean = False;
  LogonAutoComplete: Boolean = True;      // AutoComplete (for Aliaslist)

function LogonDialog(Session: TOracleSession; Retries, DropDownCount: Integer; var Options: TLogonOptions; Picture: TPicture): Boolean;
function PasswordDialog(Session: TOracleSession; const ACaption: string; Picture: TPicture): Boolean;
function LogonEncrypt(S: string): string;
function LogonDecrypt(S: string): string;

// procedure to allow sorting of history
// You can delete or move items, but you can't add items
var LogOnHistorySort: procedure(History: THistoryItems) = nil;

implementation

uses OracleCI, OracleVisual;

{$R *.dfm}

function ControlPressed: Boolean;
begin
  {$IFNDEF LINUX}
  Result := (GetKeyState(vk_Control) and $80) <> 0;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

// Logon history

function THistoryItems.NonFixedCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if not Item(i).Fixed then inc(Result);
end;

procedure THistoryItems.Add(Username, Password, Database, ConnectAs: string; Fixed, Sort: Boolean);
var Item: THistoryItem;
    S: string;
    i{, ap, sp}: Integer;
begin
  if Username = '' then Exit;
  // Extract the password and/or database from the username if available
  DecodeConnectString('', Username, Password, Database, ConnectAs);
(*
  sp := Pos('/', Username);
  ap := Pos('@', Username);
  if ap = 0 then ap := Length(Username) + 1;
  if ap < Length(Username) then Database := Copy(Username, ap + 1, Length(Username) - ap);
  if sp > 0 then Password := Copy(Username, sp + 1, ap - sp - 1);
  if sp = 0 then sp := ap;
  UserName := Copy(Username, 1, sp - 1);
*)
  if not WithPassword then Password := '';
(*
  // Extract ConnactAs part
  S := StripConnectAs(Database);
  if S = '' then S := StripConnectAs(Password);
  if S = '' then S := StripConnectAs(Username);
  StripConnectAs(Password);
  StripConnectAs(Username);
  if S <> '' then ConnectAs := S;
*)
  // Delete any old identical entries
  S := Uppercase(Username);
  if Database <> '' then S := S + '@' + Uppercase(Database);
  if ConnectAs <> '' then S := S + ' AS ' + ConnectAs;
  if (not Fixed) then
  begin
    for i := Count - 1 downto 0 do
    begin
      if Uppercase(Self.Item(i).DisplayString) = S then
      begin
        if Self.Item(i).Fixed then
        begin
          if not AllowDuplicateHistory then Exit;
        end;
        if (not Self.Item(i).Fixed) or (not AllowDuplicateHistory) then Self.Item(i).Free;
      end;
    end;
  end;
  // Check for maximum number of items
  if Size > 0 then while NonFixedCount >= Size do Self.Item(Size - 1).Free;
  // Add the new item
  Item := THistoryItem.Create(Self);
  Item.Username  := Username;
  Item.Password  := Password;
  Item.Database  := Database;
  Item.ConnectAs := ConnectAs;
  Item.Fixed     := Fixed;
  if Sort and ((not Fixed) or AllowDuplicateHistory) then Item.Index := 0;
end;

function LogonEncrypt(S: string): string;
var i, v, x: Integer;
begin
  x := Random(1000) + 2000;
  Result := '';
  for i := 1 to Length(S) do
  begin
    v := Ord(S[i]) * 16;
    Result := Result + IntToStr(1000 + (v xor (x + i * 10)));
  end;
  Result := IntToStr(x) + Result;
end;

function LogonDecrypt(S: string): string;
var i, x: Integer;
begin
  Result := '';
  if S <> '' then
  try
    x := StrToInt(Copy(S, 1, 4));
    i := 1;
    repeat
      Delete(S, 1, 4);
      if S <> '' then Result := Result + Char(((StrToInt(Copy(S, 1, 4)) - 1000) xor (x + i * 10)) div 16);
      inc(i);
    until S = '';
  except
  end;
end;

function THistoryItems.LoadIni(Filename: string): Boolean;
var INI: TINIFile;
    i: Integer;
    S: string;
begin
  Result := True;
  if Filename = '' then Exit;
{$IFDEF LINUX}
  Filename := FullINIPath(Filename);
{$ENDIF}
  INI := TINIFile.Create(Filename);
  try
    i := 0;
    repeat
      S := INI.ReadString('Logon', IntToStr(i), '');
      if S <> '' then
      begin
        S := LogonDecrypt(S);
        if (Size = 0) or (i < Size) then LogonHistory.Add(S, '', '', '', False, False);
      end;
      inc(i);
    until S = '';
  except
    Result := False;
  end;
  INI.Free;
end;

function THistoryItems.Load(Location: string): Boolean;
{$IFNDEF LINUX}
var Handle: HKey;
    Buffer: Array[0..256] of Char;
    i, BufSize, DataType: Integer;
    S: string;
{$ENDIF}
begin
  Result := True;
  if Location = '' then Exit;
{$IFDEF LINUX}
  LoadIni(Location);
{$ELSE}
  if (Location <> '') and (location[1] = '\') then System.Delete(Location, 1, 1);
  if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Location), 0, KEY_READ, Handle) = ERROR_SUCCESS then
  begin
    DataType := reg_sz;
    i := 0;
    repeat
      S := '';
      BufSize := SizeOf(Buffer);
      if RegQueryValueEx(Handle, PChar('Logon' + IntToStr(i)), nil, @DataType, @Buffer, @BufSize) = ERROR_SUCCESS then
      begin
        S := LogonDecrypt(Buffer);
        if (Size = 0) or (i < Size) then LogonHistory.Add(S, '', '', '', False, False);
      end;
      inc(i);
    until S = '';
    RegCloseKey(Handle);
  end;
{$ENDIF}
end;

function THistoryItems.SaveIni(Filename: string): Boolean;
var INI: TINIFile;
    i: Integer;
    S: string;
begin
  Result := True;
  if Filename = '' then Exit;
  Randomize;
{$IFDEF LINUX}
  Filename := FullINIPath(Filename);
{$ENDIF}
  INI := TINIFile.Create(Filename);
  try
    INI.EraseSection('Logon');
    for i := 0 to NonFixedCount - 1 do with Item(i) do
    begin
      S := Username + '/' + Password + '@' + Database;
      if ConnectAs <> '' then S := S + ' as ' + ConnectAs;
      S := LogonEncrypt(S);
      INI.WriteString('Logon', IntToStr(i), S);
    end;
{$IFDEF LINUX}
    INI.UpdateFile;
{$ENDIF}
  except
    Result := False;
  end;
  INI.Free;
end;

function THistoryItems.Save(Location: string): Boolean;
{$IFNDEF LINUX}
var Handle: HKey;
    i, Disposition: Integer;
    S: string;
{$ENDIF}
begin
  Result := True;
  if Location = '' then Exit;
  Randomize;
{$IFDEF LINUX}
  SaveINI(Location);
{$ELSE}
  if (Location <> '') and (location[1] = '\') then System.Delete(Location, 1, 1);
  if RegCreateKeyEx(HKEY_CURRENT_USER, PChar(Location), 0, nil, REG_OPTION_NON_VOLATILE,
       KEY_ALL_ACCESS, nil, Handle, @Disposition) = ERROR_SUCCESS then
  begin
    // First delete any old entries
    i := 0;
    while RegDeleteValue(Handle, PChar('Logon' + IntToStr(i))) = ERROR_SUCCESS do inc(i);
    // Store history
    for i := 0 to NonFixedCount - 1 do with Item(i) do
    begin
      S := Username + '/' + Password + '@' + Database;
      if ConnectAs <> '' then S := S + ' as ' + ConnectAs;
      S := LogonEncrypt(S);
      if not RegSetValueEx(Handle, PChar('Logon' + IntToStr(i)), 0, reg_sz, @S[1], Length(S)) = ERROR_SUCCESS then Result := False;
    end;
    RegCloseKey(Handle);
  end;
{$ENDIF}
end;

function THistoryItems.Item(Index: Integer): THistoryItem;
begin
  Result := Self.Items[Index] as THistoryItem;
end;

function THistoryItem.DisplayString: string;
begin
  Result := Username;
  if Database <> '' then Result := Result + '@' + Database;
  if ConnectAs <> '' then Result := Result + ' as ' + ConnectAs;
  if IsMenu then Delete(Result, 1, 1);
end;

function THistoryItem.IsMenu: Boolean;
begin
  Result := Copy(Username, 1, 1) = '>';
end;

function GetParamString(Name:String):String; { Search a commandline parameter }
var i: Integer;
begin
  for i := 1 to ParamCount do
  begin
    if Pos(UpperCase(Name) + '=', UpperCase(ParamStr(i))) > 0 then
    begin
      Result := Copy(ParamStr(i), Length(Name) + 2, 255);
      Exit;
    end;
  end;
  Result := '';
end;

// Main functions

function ChangePassword(Session: TOracleSession; const ACaption: string; Picture: TPicture): Boolean;
var dr: Integer;
    Done: Boolean;
begin
  Result := False;
  if Session = nil then raise Exception.Create('Session undefined');
  with TLogonForm.Create(nil) do
  begin
    // Adjust dialog
    if ltMainFont <> nil then Font.Assign(ltMainFont);
    AlignLogonImage(Picture);
    Font.Style := ltFontStyle;
    OKBtn.Caption := ltOKButton;
    CancelBtn.Caption := ltCancelButton;
    Caption := ACaption;
    Label1.Caption := ltNewPassword;
    Label2.Caption := ltVerify;
    {$IFDEF LINUX}

    {$ELSE}
    UsernameEdit.PasswordChar := '*';
    {$ENDIF}
    UserNameEdit.Text := '';
    PasswordEdit.Text := '';
    GroupBox.Height := GroupBox.Height - DBPanel.Height - CAPanel.Height;
    Height := Height - DBPanel.Height - CAPanel.Height;
    DBPanel.Visible := False;
    CAPanel.Visible := False;
    repeat
      Done := True;
      dr := ShowModal;
      if dr = mrOK then
      begin
        if AnsiUpperCase(UserNameEdit.Text) <> AnsiUpperCase(PasswordEdit.Text) then
        begin
          ShowMessage(ltVerifyFail);
          Done := False;
        end else try
          Session.SetPassword(PasswordEdit.Text);
          if not Session.Connected then Session.LogOn;
          Result := True;
        except on E:EOracleError do
          begin
            ShowMessage(E.Message);
            Done := False;
          end;
        end;
      end;
    until Done;
    Free;
  end;
end;

function PasswordDialog(Session: TOracleSession; const ACaption: string; Picture: TPicture): Boolean;
var dr: Integer;
    Done: Boolean;
begin
  Result := False;
  if Session = nil then raise Exception.Create('Session undefined');
  with TLogonForm.Create(nil) do
  begin
    // Adjust dialog
    if ltMainFont <> nil then Font.Assign(ltMainFont);
    AlignLogonImage(Picture);
    Font.Style := ltFontStyle;
    OKBtn.Caption := ltOKButton;
    CancelBtn.Caption := ltCancelButton;
    Caption := ACaption;
    Label1.Caption := ltOldPassword;
    Label2.Caption := ltNewPassword;
    Label3.Caption := ltVerify;
    {$IFDEF LINUX}

    {$ELSE}
    UsernameEdit.PasswordChar := '*';
    PasswordEdit.PasswordChar := '*';
    DatabaseEdit.PasswordChar := '*';
    {$ENDIF}
    GroupBox.Height := GroupBox.Height - CAPanel.Height;
    Height := Height -  CAPanel.Height;
    CAPanel.Visible := False;
    repeat
      Done := True;
      dr := ShowModal;
      if dr = mrOK then
      begin
        if (AnsiUpperCase(UsernameEdit.Text) <> AnsiUpperCase(Session.LogonPassword)) or
           (AnsiUpperCase(PasswordEdit.Text) <> AnsiUpperCase(DatabaseEdit.Text)) then
        begin
          ShowMessage(ltVerifyFail);
          Done := False;
        end else try
          Session.SetPassword(PasswordEdit.Text);
          if not Session.Connected then Session.LogOn;
          Result := True;
        except on E:EOracleError do
          begin
            ShowMessage(E.Message);
            Done := False;
          end;
        end;
      end;
    until Done;
    Free;
  end;
end;

function LogonDialog(Session: TOracleSession; Retries, DropDownCount: Integer; var Options: TLogonOptions; Picture: TPicture): Boolean;
var i, dr, LogCount: Integer;
    s: string;
    NoHistory: Boolean;
begin
  FailedLogonCount := 0;
  Result := False;
  NoHistory := False;
  if Session = nil then raise Exception.Create('Session undefined');
  with TLogonForm.Create(nil) do
  begin
    try
      // Adjust Dialog
      if ltMainFont <> nil then Font.Assign(ltMainFont);
    except
      ltMainFont := nil;
    end;
    AlignLogonImage(Picture);
    Font.Style := ltFontStyle;
    OKBtn.Caption := ltOKButton;
    CancelBtn.Caption := ltCancelButton;
    Caption := ltLogonTitle;
    Label1.Caption := ltUsername;
    Label2.Caption := ltPassword;
    Label3.Caption := ltDatabase;
    Label4.Caption := ltConnectAs;
    {$IFDEF CompilerVersion6}
    AliasList.AutoComplete := LogonAutoComplete;
    {$ENDIF}
    UsernameEdit.Text := Session.LogonUsername;
    PasswordEdit.Text := Session.LogonPassword;
    if not (ldDatabaseList in Options) then
      DatabaseEdit.Text := Session.LogonDatabase
    else begin
      AliasList.DropDownCount := DropDownCount;
      AliasList.Visible := True;
      AliasList.Left  := DatabaseEdit.Left;
      AliasList.Width := DatabaseEdit.Width;
      if OracleAliasList <> nil then for i := 0 to OracleAliasList.Count - 1 do AliasList.Items.Add(OracleAliasList[i]);
      DatabaseEdit.Visible := False;
      AliasList.Text := Session.LogonDatabase;
    end;
    case Session.ConnectAs of
       caSYSDBA: ConnectAsList.ItemIndex := 1;
      caSYSOPER: ConnectAsList.ItemIndex := 2;
    else
      ConnectAsList.ItemIndex := 0;
    end;
    if ldLogonHistory in Options then
    begin
      HistoryBtn.Visible := True;
      HistoryBtn.Enabled := LogonHistory.Count > 0;
      HistoryBtn.Hint    := ltHistoryHint;
    end;
    if not (ldDatabase in Options) then
    begin
      // Adjust dialog for database edit
      GroupBox.Height := GroupBox.Height - DBPanel.Height;
      Height := Height - DBPanel.Height;
      DBPanel.Visible := False;
    end;
    if (not (ldConnectAs in Options)) or
       (not OCI80Detected) or
       (Session.Preferences.UseOCI7 or ForceOCI7) then
    begin
      // Adjust dialog for connect as list
      GroupBox.Height := GroupBox.Height - CAPanel.Height;
      Height := Height - CAPanel.Height;
      CAPanel.Visible := False;
    end;
    if ldPasswordOnly in Options then
    begin
      if HistoryBtn.Visible then HistoryBtn.Enabled := False;
      UsernameEdit.Enabled := UsernameEdit.Text = '';
      Label1.Enabled := UsernameEdit.Enabled;
      if DatabaseEdit.Visible then
      begin
        DatabaseEdit.Enabled := False;
        Label3.Enabled := False;
      end;
      if AliasList.Visible then AliasList.Enabled := False;
      if ConnectAsList.Visible then
      begin
        ConnectAsList.Enabled := False;
        Label4.Enabled := False;
      end;
    end;
    LogCount := 0;
    repeat
      // Direct logon or show dialog first ?
      dr := mrCancel;
      if (ldAuto in Options) and (LogCount = 0) then
      begin
        Options := Options - [ldAuto];
        if not NoUserIdParam then s := GetParamString('UserId') else s := '';
        if s <> '' then
        begin
          UsernameEdit.Text := s;
          PasswordEdit.Text := '';
          if (ldDatabaseList in Options) then
            AliasList.Text := ''
          else
            DatabaseEdit.Text := '';
          dr := mrOK;
        end else begin
          if (Session.LogonUsername <> '') and (Session.LogonPassword <> '') then
            dr := mrOK
        end;
      end;
      if dr <> mrOK then
      begin
        dr := ShowModal;
        if dr = mrOK then NoHistory := ControlPressed;
      end;
      inc(LogCount);
      if dr = mrOK then
      begin
        Result := True;
        Session.LogonUsername := UsernameEdit.Text;
        Session.LogonPassword := PasswordEdit.Text;
        if (ldDatabaseList in Options) then
          Session.LogonDatabase := AliasList.Text
        else
          Session.LogonDatabase := DatabaseEdit.Text;
        Session.ConnectAs := caNormal;
        if (ldConnectAs in Options) then
        begin
          if ConnectAsList.ItemIndex = 1 then Session.ConnectAs := caSYSDBA;
          if ConnectAsList.ItemIndex = 2 then Session.ConnectAs := caSYSOPER;
        end;
        try
          if Session.POLite and (ldDatabaseList in Options) then
          begin
            Session.LogOff;
            DLLExit;
          end;
          Session.Logon;
          if Session.ExpirationMessage <> '' then
          begin
            s := Session.ExpirationMessage + #13#10 + ltChangePassword;
            if Confirm(s, ltConfirmTitle, 'YN') = idYes then ChangePassword(Session, ltExpired, Picture);
          end;
          if (ldLogonHistory in Options) and (not NoHistory) then
          begin
            LogonHistory.Add(Session.LogonUsername, Session.LogonPassword, Session.LogonDatabase, ConnectAsToString(Session.ConnectAs), False, True);
          end;
        except
          on E:EOracleError do
          begin
            if (E.ErrorCode <> 28001) or not OCI80 then
              ShowMessage(E.Message)
            else
              ChangePassword(Session, ltExpired, Picture);
          end;
          on E:Exception do ShowMessage(E.Message)
        end;
        UsernameEdit.Text := Session.LogonUsername;
        PasswordEdit.Text := Session.LogonPassword;
        if (ldDatabaseList in Options) then
          AliasList.Text := Session.LogonDatabase
        else
          DatabaseEdit.Text := Session.LogonDatabase;
        if not Session.Connected then inc(FailedLogonCount);
      end;
    until Session.Connected or (dr <> mrOK) or (LogCount > Retries);
    Free;
  end;
end;

procedure TLogOnForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF LINUX}
  {$IFDEF CompilerVersion6}
  if (DialogMonitor >= 0) and (DialogMonitor < Screen.MonitorCount) then
  begin
    DefaultMonitor := dmDesktop;
    Left := Screen.Monitors[DialogMonitor].Left + ((Screen.Monitors[DialogMonitor].Width - Width) div 2);
    Top := Screen.Monitors[DialogMonitor].Top + ((Screen.Monitors[DialogMonitor].Height - Height) div 2);
  end else begin
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TLogOnForm.FormActivate(Sender: TObject);
begin
  {$IFDEF LINUX}
  if AliasList.Visible then
  begin
    AliasList.Left  := DatabaseEdit.Left;
    AliasList.Width := DatabaseEdit.Width;
  end;
  {$ENDIF}
  InitForm(Self);
end;

procedure TLogOnForm.FormShow(Sender: TObject);
begin
  if (USernameEdit.Enabled and (UsernameEdit.Text = '')) then UsernameEdit.Setfocus else PasswordEdit.Setfocus;
end;

procedure TLogOnForm.AlignLogonImage(Picture: TPicture);
var Offset, w: Integer;
begin
  LogonImage.Picture.Assign(Picture);
  if LogonImage.Picture.Width < 1 then
    LogonImage.Visible := False
  else begin
    OffSet := GroupBox.Left;
    w := LogonImage.Picture.Width;
    LogonImage.Width := LogonImage.Picture.Width + 4 * Offset;
    Width := Width + w + 3 * Offset;
    GroupBox.Left := 4 * Offset + w;
    OKBtn.Left := w + 4 * Offset;
    CancelBtn.Left := OKBtn.Left + OKBtn.Width + Offset;
    LogonImage.Visible := True;
  end;
end;

procedure TLogOnForm.HistoryClick(Sender: TObject);
var Index: Integer;
begin
  Index := TMenuItem(Sender).Tag;
  UsernameEdit.Text := LogonHistory.Item(Index).Username;
  if LogonHistory.WithPassword or LogonHistory.Item(Index).Fixed then
    PasswordEdit.Text := LogonHistory.Item(Index).Password
  else
    PasswordEdit.Text := '';
  if DatabaseEdit.Visible then DatabaseEdit.Text := LogonHistory.Item(Index).Database;
  if AliasList.Visible then AliasList.Text := LogonHistory.Item(Index).Database;
  if ConnectAsList.Visible then
  begin
    case StringToConnectAs(LogonHistory.Item(Index).ConnectAs) of
       caSYSDBA: ConnectAsList.ItemIndex := 1;
      caSYSOPER: ConnectAsList.ItemIndex := 2;
    else
      ConnectAsList.ItemIndex := 0;
    end;
  end;
  if (UsernameEdit.Text <> '') and (PasswordEdit.Text <> '') and (not ControlPressed) then ModalResult := mrOK;
end;

function TLogOnForm.GetSortedHistory: THistoryItems;
var i: Integer;
    H: THistoryItem;
begin
  Result := THistoryItems.Create(THistoryItem);
  for i := 0 to LogonHistory.Count - 1 do
  begin
    H := THistoryItem.Create(Result);
    H.Username := THistoryItem(LogonHistory.Items[i]).Username;
    H.Password  := THistoryItem(LogonHistory.Items[i]).Password;
    H.Database := THistoryItem(LogonHistory.Items[i]).Database;
    H.ConnectAs := THistoryItem(LogonHistory.Items[i]).ConnectAs;
    H.Fixed := THistoryItem(LogonHistory.Items[i]).Fixed;
    H.OriginalIndex := LogonHistory.Items[i].Index;
  end;
  if Assigned(LogOnHistorySort) then LogOnHistorySort(Result)
end;

procedure TLogOnForm.HistoryBtnClick(Sender: TObject);
var P: TPoint;
    i: Integer;
    SortedHistory: THistoryItems;
    MM: TMenuItem;
 procedure AddPopup(S: string; Index: Integer);
 var Item: TMenuItem;
 begin
   if (HistoryPopup.Items.Count = 0) and (S = '-') then Exit;
   Item := TMenuItem.Create(HistoryPopup);
   Item.Caption := S;
   Item.Tag := Index;
   Item.OnClick := HistoryClick;
   if MM = nil then HistoryPopup.Items.Add(Item) else MM.Add(Item);
 end;
 procedure AddSubPopup(S: string);
 begin
   MM := TMenuItem.Create(HistoryPopup);
   MM.Caption := S;
   HistoryPopup.Items.Add(MM);
 end;
begin
  MM := nil;
  for i := 0 to HistoryPopup.Items.Count - 1 do HistoryPopup.Items.Delete(0);
  SortedHistory := GetSortedHistory;
  for i := 0 to SortedHistory.Count - 1 do
  begin
    if SortedHistory.Item(i).IsMenu then
    begin
      if SortedHistory.Item(i).DisplayString = '' then
        MM := nil
      else
        AddSubPopup(SortedHistory.Item(i).DisplayString);
    end else begin
      AddPopup(SortedHistory.Item(i).DisplayString, SortedHistory.Item(i).OriginalIndex);
    end;
  end;
  P.X := HistoryBtn.Left;
  P.Y := HistoryBtn.Top + HistoryBtn.Height;
  P := GroupBox.ClientToScreen(P);
  HistoryPopup.Popup(P.X, P.Y);
  SortedHistory.Free;
end;

procedure TLogOnForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EnterKey(Key) and (ssCtrl in Shift) then
  begin
    Key := 0;
    ModalResult := mrOK;
  end;
end;

initialization
  LogonHistory := THistoryItems.Create(THistoryItem);

finalization
  LogonHistory.Free;

end.
