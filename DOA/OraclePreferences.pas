// Direct Oracle Access - DesignTime Preferences
// Copyright 2000, 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OraclePreferences;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Registry, Grids, Buttons,
  {$IFNDEF NODATASET} Db, OracleData, {$ENDIF}
  Oracle, OracleLogon, OracleCI, OracleVisual, checklst, Mask;
{$ELSE}
uses
  SysUtils, Classes, Libc, QGraphics, QControls, QForms, QDialogs, Qt,
  QStdCtrls, QComCtrls, QExtCtrls, QGrids, QButtons, QCheckLst,
  {$IFNDEF NODATASET} Db, OracleData, {$ENDIF}
  IniFiles, Oracle, OracleLogon, OracleCI, OracleVisual;
{$ENDIF}

type
  TPreferenceForm = class(TForm)
    ButtonPanel: TPanel;
    PageControl: TPageControl;
    GeneralSheet: TTabSheet;
    LogonSheet: TTabSheet;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    HistoryGroupBox: TGroupBox;
    HistoryEnabledCheck: TCheckBox;
    HistoryPasswordCheck: TCheckBox;
    Label1: TLabel;
    HistorySizeEdit: TEdit;
    LogonGroupBox: TGroupBox;
    LogonAlwaysCheck: TRadioButton;
    LogonPreferenceCheck: TRadioButton;
    Label2: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    Label3: TLabel;
    DatabaseEdit: TEdit;
    Label4: TLabel;
    MenuEnabledCheck: TCheckBox;
    WizardsEnabledCheck: TCheckBox;
    ExceptionSheet: TTabSheet;
    OCIDLLEdit: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    BrowseDLLBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    Label7: TLabel;
    OracleHomeEdit: TComboBox;
    ExceptionList: TCheckListBox;
    ExceptionEdit: TEdit;
    Label8: TLabel;
    ErrorAddBtn: TBitBtn;
    ErrorRemoveBtn: TBitBtn;
    Label9: TLabel;
    Label10: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure BrowseDLLBtnClick(Sender: TObject);
    procedure ErrorAddBtnClick(Sender: TObject);
    procedure ErrorRemoveBtnClick(Sender: TObject);
    procedure ExceptionEditKeyPress(Sender: TObject; var Key: Char);
    procedure ExceptionEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExceptionEditEnter(Sender: TObject);
    procedure ExceptionEditExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure FillOCIList;
    procedure FillHomeList;
  public
    { Public declarations }
  end;

procedure DoOraclePreferences;
procedure InitOracleLogon(Logon: TOracleLogon);
procedure StoreLogonParams(Session: TOracleSession);
procedure InitOraclePreferences;

{$IFDEF LINUX}
function  ReadRegString(const Key, Name: String): string;
{$ELSE}
function  ReadRegString(Root:HKEY; const Key, Name: String): string;
{$ENDIF}
function  OpenRegistry(Section: string): Boolean;
function  OpenOracleRegistry(Section: string): Boolean;
function  OpenDelphiRegistry(Section: string): Boolean;
procedure CloseRegistry;
procedure ClearSection;
function  ReadSection: TStringList;
procedure WriteInteger(Key: string; Value: Integer);
procedure WriteString(Key: string; Value: string);
procedure WriteBool(Key: string; Value: Boolean);
function  ReadInteger(Key: string; Default: Integer): Integer;
function  ReadString(Key: string; Default: string): string;
function  ReadBool(Key: string; Default: Boolean): Boolean;

implementation

{$R *.dfm}

var
  {$IFDEF LINUX}
  INI: TINIFile;
  INISection: string;
  {$ELSE}
  Reg: TRegistry;
  RegSection: string;
  {$ENDIF}

function ORA(S: string): string;
begin
  Result := S;
  while Length(Result) < 5 do Result := '0' + Result;
  Result := 'ORA-' + Result;
end;

// Registry functions

{$IFDEF LINUX}

function ReadRegString(const Key, Name: String): string;
var I: TINIFile;
begin
  I := TINIFile.Create(FullINIPath('.doa'));
  try
    Result := I.ReadString(Key, Name, '');
  except
    Result := '';
  end;
  I.Free;
end;

function OpenRegistry(Section: string): Boolean;
begin
  CloseRegistry;
  try
    INI := TINIFile.Create(FullINIPath('.doa'));
    INISection := Section;
  except
    CloseRegistry;
  end;
  Result := (INI <> nil);
end;

function OpenOracleRegistry(Section: string): Boolean;
begin
  CloseRegistry;
  Result := False;
{  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  RegSection := 'SOFTWARE\Oracle\' + Section;
  Result := Reg.OpenKey(RegSection, False);
  if not Result then CloseRegistry;}
end;

function OpenDelphiRegistry(Section: string): Boolean;
//var S: string;
begin
  CloseRegistry;
  Result := False;
{  S := GetCompilerName + '\' + IntToStr(GetCompilerVersion) + '.0\';
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  RegSection := 'SOFTWARE\Borland\' + S + Section;
  Result := Reg.OpenKey(RegSection, False);
  if not Result then CloseRegistry;}
end;

procedure CloseRegistry;
begin
  if INI <> nil then
  begin
    INI.UpdateFile;
    INI.Free;
    INI := nil;
  end;
end;

function ReadSection: TStringList;
begin
  Result := TStringList.Create;
 // if Reg <> nil then INI.GetValueNames(Result);
end;

procedure ClearSection;
begin
  if INI <> nil then
  begin
    INI.EraseSection(INISection);
  end;
end;

procedure WriteInteger(Key: string; Value: Integer);
begin
  if INI <> nil then INI.WriteInteger(INISection, Key, Value);
end;

procedure WriteString(Key: string; Value: string);
begin
  if INI <> nil then INI.WriteString(INISection, Key, Value);
end;

procedure WriteBool(Key: string; Value: Boolean);
begin
  if INI <> nil then INI.WriteBool(INISection, Key, Value);
end;

function ReadInteger(Key: string; Default: Integer): Integer;
begin
  Result := Default;
  if INI <> nil then Result := INI.ReadInteger(INISection, Key, Default);
end;

function ReadString(Key: string; Default: string): string;
begin
  Result := Default;
  if INI <> nil then Result := INI.ReadString(INISection, Key, Default);
end;

function ReadBool(Key: string; Default: Boolean): Boolean;
begin
  Result := Default;
  if INI <> nil then Result := INI.ReadBool(INISection, Key, Default);
end;

{$ELSE}

function ReadRegString(Root:HKEY; const Key, Name: String): string;
var Handle: HKey;
    Buffer: Array[0..256] of Char;
    BufSize: Integer;
    DataType: Integer;
begin
  Result := '';
  if RegOpenKeyEx(Root, PChar(Key), 0, KEY_READ,Handle) = ERROR_SUCCESS then
  begin
    BufSize := SizeOf(Buffer);
    DataType := reg_sz;
    if RegQueryValueEx(Handle, PChar(Name), nil, @DataType, @Buffer, @BufSize) = ERROR_SUCCESS then Result := Buffer;
    RegCloseKey(Handle);
  end;
end;

function OpenRegistry(Section: string): Boolean;
begin
  CloseRegistry;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  RegSection := 'Software\Allround Automations\DOA\' + Section;
  Result := Reg.OpenKey(RegSection, True);
  if not Result then CloseRegistry;
end;

function OpenOracleRegistry(Section: string): Boolean;
begin
  CloseRegistry;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  RegSection := 'SOFTWARE\Oracle\' + Section;
  Result := Reg.OpenKey(RegSection, False);
  if not Result then CloseRegistry;
end;

function OpenDelphiRegistry(Section: string): Boolean;
var S: string;
begin
  CloseRegistry;
  S := GetCompilerName + '\' + IntToStr(GetCompilerVersion) + '.0\';
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  RegSection := 'SOFTWARE\Borland\' + S + Section;
  Result := Reg.OpenKey(RegSection, False);
  if not Result then CloseRegistry;
end;

procedure CloseRegistry;
begin
  if Reg <> nil then
  begin
    Reg.CloseKey;
    Reg.Free;
    Reg := nil;
  end;
end;

function ReadSection: TStringList;
begin
  Result := TStringList.Create;
  if Reg <> nil then Reg.GetValueNames(Result);
end;

procedure ClearSection;
begin
  if Reg <> nil then
  begin
    Reg.CloseKey;
    Reg.DeleteKey(RegSection);
    Reg.OpenKey(RegSection, True);
  end;
end;

procedure WriteInteger(Key: string; Value: Integer);
begin
  if Reg <> nil then Reg.WriteInteger(Key, Value);
end;

procedure WriteString(Key: string; Value: string);
begin
  if Reg <> nil then Reg.WriteString(Key, Value);
end;

procedure WriteBool(Key: string; Value: Boolean);
begin
  if Reg <> nil then Reg.WriteBool(Key, Value);
end;

function ReadInteger(Key: string; Default: Integer): Integer;
begin
  Result := Default;
  if (Reg <> nil) and Reg.ValueExists(Key) then Result := Reg.ReadInteger(Key);
end;

function ReadString(Key: string; Default: string): string;
begin
  Result := Default;
  if (Reg <> nil) and Reg.ValueExists(Key) then Result := Reg.ReadString(Key);
end;

function ReadBool(Key: string; Default: Boolean): Boolean;
begin
  Result := Default;
  if (Reg <> nil) and Reg.ValueExists(Key) then Result := Reg.ReadBool(Key);
end;

{$ENDIF}

procedure InitOracleLogon(Logon: TOracleLogon);
begin
  if OpenRegistry('Preferences') then
  begin
    if ReadBool('HistoryEnabled', True) then
      Logon.Options := Logon.Options + [ldLogonHistory]
    else
      Logon.Options := Logon.Options - [ldLogonHistory];
    Logon.HistoryWithPassword := ReadBool('HistoryWithPassword', True);
    try
      Logon.HistorySize := StrToInt(ReadString('HistorySize', '8'));
    except
      // Error coverting value, set it to default
      Logon.HistorySize := 8;
      WriteString('HistorySize', '8');
    end;
    if Logon.Session <> nil then
    begin
      if ReadBool('UseLogonPreference', False) then
      begin
        // Set preference values if requested
        Logon.Session.LogonUsername := ReadString('Username', '');
        Logon.Session.LogonPassword := LogonDecrypt(ReadString('Password', ''));
        Logon.Session.LogonDatabase := ReadString('Database', '');
      end else begin
        if Logon.Session.LogonUsername = '' then
        begin
          // Use last used values
          if OpenRegistry('Logon') then
          begin
            Logon.Session.LogonUsername := ReadString('Username', Logon.Session.LogonUsername);
            Logon.Session.LogonPassword := ReadString('Password', Logon.Session.LogonPassword);
            Logon.Session.LogonDatabase := ReadString('Database', Logon.Session.LogonDatabase);
          end;
        end;
        if Logon.Session.LogonUsername = '' then
        begin
          // if still 'empty', use preference values
          OpenRegistry('Preferences');
          Logon.Session.LogonUsername := ReadString('Username', '');
          Logon.Session.LogonPassword := LogonDecrypt(ReadString('Password', ''));
          Logon.Session.LogonDatabase := ReadString('Database', '');
        end;
      end;
    end;
    CloseRegistry;
  end;
end;

procedure StoreLogonParams(Session: TOracleSession);
begin
  if OpenRegistry('Logon') then
  begin
    if ReadString('Password', '') = '' then
    begin
      WriteString('Username', Session.LogonUsername);
      WriteString('Database', Session.LogonDatabase);
    end;
    CloseRegistry;
  end;
end;

procedure InitOraclePreferences;
begin
  if OpenRegistry('Preferences') then
  begin
    OCIDLL := ReadString('OCIDLL', '');
    OracleHomeName := ReadString('OracleHome', '');
    CloseRegistry;
  end;
end;

// Preference Dialog

procedure DoOraclePreferences;
var i: Integer;
    S: string;
begin
  with TPreferenceForm.Create(nil) do
  begin
    PageControl.ActivePage := PageControl.Pages[0];
    FillOCIList;
    FillHomeList;
    if OpenRegistry('Preferences') then
    begin
      UsernameEdit.Text := ReadString('Username', '');
      PasswordEdit.Text := LogonDecrypt(ReadString('Password', ''));
      DatabaseEdit.Text := ReadString('Database', '');
      LogonAlwaysCheck.Checked := ReadBool('UseLogonPreference', False);
      LogonPreferenceCheck.Checked := not LogonAlwaysCheck.Checked;
      HistoryEnabledCheck.Checked := ReadBool('HistoryEnabled', True);
      HistoryPasswordCheck.Checked := ReadBool('HistoryWithPassword', True);
      HistorySizeEdit.Text := ReadString('HistorySize', '8');

      MenuEnabledCheck.Checked := ReadBool('OracleMenu', True);
      WizardsEnabledCheck.Checked := ReadBool('OracleWizards', True);
      OCIDLLEdit.Text := ReadString('OCIDLL', '');
      OracleHomeEdit.Text := ReadString('OracleHome', '');

      Openregistry('Warnings');
      ExceptionList.Items.Clear;
      i := 1;
      repeat
        S := ReadString(IntToStr(i), '');
        if S <> '' then
        begin
          if S[1] = '*' then
            ExceptionList.Items.Add(ORA(Copy(S, 2, Length(S))))
          else
            ExceptionList.Checked[ExceptionList.Items.Add(ORA(S))] := True;
        end;
        inc(i);
      until S = '';

      CloseRegistry;
    end;
    if ShowModal = mrOK then
    begin
      if OpenRegistry('Preferences') then
      begin
        WriteString('Username', UsernameEdit.Text);
        WriteString('Password', LogonEncrypt(PasswordEdit.Text));
        WriteString('Database', DatabaseEdit.Text);
        WriteBool('UseLogonPreference', LogonAlwaysCheck.Checked);
        WriteBool('HistoryEnabled', HistoryEnabledCheck.Checked);
        WriteBool('HistoryWithPassword', HistoryPasswordCheck.Checked);
        WriteString('HistorySize', HistorySizeEdit.Text);

        WriteBool('OracleMenu', MenuEnabledCheck.Checked);
        WriteBool('OracleWizards', WizardsEnabledCheck.Checked);
        WriteString('OCIDLL', OCIDLLEdit.Text);
        WriteString('OracleHome', OracleHomeEdit.Text);

        OpenRegistry('Warnings');
        ClearSection;
        for i := 0 to ExceptionList.Items.Count - 1 do
        begin
          S := Copy(ExceptionList.Items[i], 5, 10);
          if not ExceptionList.Checked[i] then S := '*' + S;
          WriteString(IntToStr(i + 1), S);
        end;

        CloseRegistry;
        LoadOracleWarnings;
        OCIDLL := OCIDLLEdit.Text;
        OracleHomeName := OracleHomeEdit.Text;
      end;
    end;
    Free;
  end;
end;

procedure TPreferenceForm.FormActivate(Sender: TObject);
begin
  BrowseDLLBtn.Height := OCIDLLEdit.Height;
  BrowseDLLBtn.Width  := OCIDLLEdit.Height;
  InitForm(Self);
end;

procedure TPreferenceForm.FillOCIList;
var i: Integer;
    List: TStringList;
begin
  OCIDLLEdit.Clear;
  List := GetOCIDLLList(False);
  for i := 0 to List.Count - 1 do OCIDLLEdit.Items.Add(List[i]);
  List.Free;
end;

procedure TPreferenceForm.FillHomeList;
var i: Integer;
    List: TStringList;
begin
  OracleHomeEdit.Clear;
  List := OracleHomeList;
  for i := 0 to List.Count - 1 do OracleHomeEdit.Items.Add(List[i]);
end;

procedure TPreferenceForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'Direct Oracle Access Preferences');
end;

procedure TPreferenceForm.BrowseDLLBtnClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := ExtractFilePath(OCIDLLEDit.Text);
    if Execute then
    begin
      OCIDLLEdit.Text := FileName;
    end;
  end;
end;

procedure TPreferenceForm.ErrorAddBtnClick(Sender: TObject);
var S: string;
    Index: Integer;
begin
  S := ORA(IntToStr(StrToInt(Trim(ExceptionEdit.Text))));
  Index := ExceptionList.Items.IndexOf(S);
  if Index < 0 then
    ExceptionList.Checked[ExceptionList.Items.Add(S)] := True
  else begin
    ExceptionList.ItemIndex := Index;
    ExceptionList.Checked[Index] := True
  end;
  ExceptionEdit.SetFocus;
end;

procedure TPreferenceForm.ErrorRemoveBtnClick(Sender: TObject);
var Index: Integer;
begin
  Index := ExceptionList.ItemIndex;
  if Index >= 0 then
  begin
    ExceptionList.Items.Delete(Index);
    if Index >= ExceptionList.Items.Count then dec(Index);
    ExceptionList.ItemIndex := Index;
  end;
end;

procedure TPreferenceForm.ExceptionEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0;
end;

procedure TPreferenceForm.ExceptionEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EnterKey(Key) then
  begin
    ErrorAddBtnClick(nil);
    Key := 0;
  end;
end;

procedure TPreferenceForm.ExceptionEditEnter(Sender: TObject);
begin
  OKBtn.Default := False;
end;

procedure TPreferenceForm.ExceptionEditExit(Sender: TObject);
begin
  OKBtn.Default := True;
end;

initialization
  InitOraclePreferences;

finalization
  CloseRegistry;

end.
