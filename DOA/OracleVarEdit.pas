// Direct Oracle Access - Variables property editor form
// Copyright 1997 - 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleVarEdit;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion6} Variants, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OracleTypes, Oracle, ExtCtrls, ClipBrd, OraclePreferences,
  Buttons, OracleVisual;
{$ELSE}
uses
  Variants, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, OracleTypes, Oracle, OraclePreferences, QExtCtrls, OracleVisual,
  QClipBrd;
{$ENDIF}

type
  TVariablesForm = class(TForm)
    ButtonPanel: TPanel;
    MainPanel: TPanel;
    HelpPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    MainGroupBox: TGroupBox;
    VarPanel: TPanel;
    VarTopPanel: TPanel;
    VarList: TListBox;
    Label1: TLabel;
    EditPanel: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PLSQLTablePanel: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    TableSizeEdit: TEdit;
    StringSizeEdit: TEdit;
    TypeList: TComboBox;
    NameEdit: TEdit;
    NewBtn: TButton;
    DeleteBtn: TButton;
    ScanBtn: TButton;
    ValueEdit: TEdit;
    TableCheck: TCheckBox;
    MemoryPanel: TPanel;
    MemoryLabel: TLabel;
    CopyBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VarListClick(Sender: TObject);
    procedure VarChange(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ScanBtnClick(Sender: TObject);
    procedure ValueEditExit(Sender: TObject);
    procedure TableSizeEditExit(Sender: TObject);
    procedure StringSizeEditExit(Sender: TObject);
    procedure TableCheckClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TypeListChange(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    Silent: Boolean;
    WorkQuery: TOracleQuery;
    QueryName: string;
    procedure BuildVarList;
    procedure EnableButtons;
    procedure AddVariable(AName: string);
    procedure FindVariables;
    procedure DimPLSQLTable;
    procedure EnableTableFields;
    procedure SetMemoryPanel;
    procedure SelectVariable(Index: Integer);
    procedure FormActivate;
  public
    Changed: Boolean;
    CopiedVariables: TVariables;
    CopiedQuery: TOracleQuery;
  end;

function  ExecuteVariablesEditor(Q: TOracleQuery; ComponentName: string): Boolean;
procedure CopyVariables(Src, Dst: TOracleQuery);

implementation

{$R *.dfm}

// Execute the VariablesEditor, return true if something changed
function ExecuteVariablesEditor(Q: TOracleQuery; ComponentName: string): Boolean;
begin
  if ComponentName = '' then ComponentName := Q.Name;
  with TVariablesForm.Create(nil) do
  begin
    QueryName := ComponentName;
    CopiedVariables := Q.Variables;
    Caption := Q.Owner.Name + '.' + Q.Name + ' Variables';
    FormActivate;
    Result :=(ShowModal = mrOK) and Changed;
    Free;
  end;
end;

// General functions & Form

function MakeVarName(const AName: string): string;
begin
  if (AName <> '') and (AName[1] <> ':') then
    Result := ':' + AnsiUpperCase(AName)
  else
    Result := AnsiUpperCase(AName);
end;

procedure CopyVariables(Src, Dst: TOracleQuery);
begin
  Dst.Variables.Assign(Src.Variables);
end;

function TypeString(T: Integer): String;
begin
  case T of
      otString : Result := 'String';
        otDate : Result := 'Date';
     otInteger : Result := 'Integer';
       otFloat : Result := 'Float';
        otLong : Result := 'Long';
     otLongRaw : Result := 'Long Raw';
      otCursor : Result := 'Cursor';
        otCLOB : Result := 'CLOB';
        otBLOB : Result := 'BLOB';
       otBFile : Result := 'BFile';
   otReference : Result := 'Reference';
      otObject : Result := 'Object';
 otPLSQLString : Result := 'PL/SQL String';
        otChar : Result := 'Char';
       otSubst : Result := 'Substitution';
   otTimestamp : Result := 'Timestamp';
 otTimestampTZ : Result := 'Timestamp with Time Zone';
otTimestampLTZ : Result := 'Timestamp with Local Time Zone';
    else
    Result := '?';
  end;
end;

procedure TVariablesForm.BuildVarList;
var i: Integer;
begin
  VarList.Clear;
  for i := 0 to WorkQuery.Variables.Count - 1 do
    VarList.Items.Add(WorkQuery.Variables.Data(i).Name + ' (' +
           TypeString(WorkQuery.Variables.Data(i).BufType) + ')' );
  CopyBtn.Enabled := WorkQuery.Variables.Count > 0;
end;

procedure TVariablesForm.FormCreate(Sender: TObject);
begin
  {$IFDEF CompilerVersion4} {$IFNDEF CompilerVersion5}
  VarList.MultiSelect := True;
  {$ENDIF} {$ENDIF}
  Silent  := True;
  Changed := False;
  // First Calculate default Width & Height
  Width  := EditPanel.Width + 30 + 160;
  Height := ButtonPanel.Height * 8 + 10;
  if OpenRegistry('Variables Editor') then
  begin
    Left   := ReadInteger('Left', Left);
    Top    := ReadInteger('Top', Top);
    Width  := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    CloseRegistry;
  end;
  WorkQuery := TOracleQuery.Create(nil);
  EnableButtons;
end;

procedure TVariablesForm.FormActivate;
begin
  {$IFDEF CompilerVersion4}
  Constraints.MinHeight := 10 + Height - (EditPanel.Height - (NewBtn.Top + NewBtn.Height));
  Constraints.MinWidth  := 10 + Width - VarList.Width;
  {$ENDIF}
  CopiedQuery := (CopiedVariables.Owner as TOracleQuery);
  CopyVariables(CopiedQuery, WorkQuery);
  BuildVarList;
  if CopiedVariables.Count > 0 then SelectVariable(0) else SelectVariable(-1);
  VarListClick(nil);
  Silent := False;
  InitForm(Self);
end;

procedure TVariablesForm.FormDestroy(Sender: TObject);
begin
  if OpenRegistry('Variables Editor') then
  begin
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
    CloseRegistry;
  end;
  WorkQuery.Free;
end;

procedure TVariablesForm.EnableButtons;
begin
  DeleteBtn.Enabled := (VarList.ItemIndex >= 0);
  NameEdit.Enabled  := (VarList.ItemIndex >= 0);
  TypeList.Enabled  := (VarList.ItemIndex >= 0);
  ValueEdit.Enabled := (VarList.ItemIndex >= 0) and
                       (TypeList.ItemIndex in [0..3, 12..14]) and
                       (not TableCheck.Checked);
end;

procedure TVariablesForm.VarListClick(Sender: TObject);
var v: Variant;
begin
  Silent := True;
  if (VarList.ItemIndex < 0) or (VarList.Items.Count = 0) then
  begin
    NameEdit.Text := '';
    TypeList.ItemIndex := -1;
    ValueEdit.Text := '';
    Silent := False;
    EnableButtons;
    Exit;
  end;
  with WorkQuery.Variables.Data(VarList.ItemIndex) do
  begin
    NameEdit.Text  := Name;
    v := WorkQuery.GetVariable(Name);
    if VarIsEmpty(v) or VarIsNull(v) then
      ValueEdit.Text := ''
    else
      ValueEdit.Text := v;
    case BufType of
      otInteger: TypeList.ItemIndex := 0;
        otFloat: TypeList.ItemIndex := 1;
       otString: TypeList.ItemIndex := 2;
         otDate: TypeList.ItemIndex := 3;
         otLong: TypeList.ItemIndex := 4;
      otLongRaw: TypeList.ItemIndex := 5;
       otCursor: TypeList.ItemIndex := 6;
         otCLOB: TypeList.ItemIndex := 7;
         otBLOB: TypeList.ItemIndex := 8;
        otBFile: TypeList.ItemIndex := 9;
    otReference: TypeList.ItemIndex := 10;
       otObject: TypeList.ItemIndex := 11;
  otPLSQLString: TypeList.ItemIndex := 12;
         otChar: TypeList.ItemIndex := 13;
        otSubst: TypeList.ItemIndex := 14;
    otTimestamp: TypeList.ItemIndex := 15;
  otTimestampTZ: TypeList.ItemIndex := 16;
 otTimestampLTZ: TypeList.ItemIndex := 17;
      else begin
        SHowMessage('Unknown type:' + IntToStr(BufType));;
        TypeList.ItemIndex := -1;
      end;
    end;
    TableCheck.Checked := IsPLSQLTable;
    if IsPLSQLTable then
    begin
      TableSizeEdit.Text := IntToStr(ArraySize);
      if BufType = otString then StringSizeEdit.Text := IntToStr(BufSize - 1);
    end;
    EnableTableFields;
    SetMemoryPanel;
  end;
  EnableButtons;
  Silent := False;
end;

procedure TVariablesForm.AddVariable(AName: string);
var i: Integer;
begin
  if AName = '' then exit;
  AName := MakeVarName(AName);
  for i := 0 to WorkQuery.Variables.Count - 1 do
    if AName = MakeVarName(WorkQuery.VariableName(i)) then Exit;
  Changed := True;
  WorkQuery.DeclareVariable(AName, otString);
  BuildVarList;
  SelectVariable(-1);
  VarListClick(nil);
  VarList.SetFocus;
end;

procedure TVariablesForm.FindVariables;
var i, j, Index: Integer;
    mr: Word;
    Found: Boolean;
    VarList: TStringList;
begin
  VarList := Oracle.FindVariables(CopiedQuery.SQL.Text, False);
  for i := 0 to VarList.Count - 1 do AddVariable(VarList[i]);
  // Check for unused variables
  mr := mrNone;
  i := 0;
  while i < WorkQuery.Variables.Count do
//  for i :=  downto 0 do
  begin
    Found := False;
    for j := 0 to VarList.Count - 1 do
    begin
      if MakeVarName(VarList[j]) = MakeVarName(WorkQuery.VariableName(i)) then
      begin
        Found := True;
        Break;
      end;
    end;
    if not Found then
    begin

      SelectVariable(i);
      VarListClick(nil);

      if mr <> mrAll then
        mr := Confirm('Variable ' + WorkQuery.VariableName(i) + ' is not used in the SQL text.' + #13#10#13#10 +
                      'Delete it?', 'Confirm', 'YNA');
      if (mr = mrYes) or (mr = mrAll) then
      begin
        WorkQuery.DeleteVariable(WorkQuery.VariableName(i));
        BuildVarList;
        Index := i;
        if Index >= WorkQuery.VariableCount then Index := WorkQuery.VariableCount - 1;
        SelectVariable(Index);
        VarListClick(nil);
        Changed := True;
        dec(i);
      end;
    end;
    inc(i);
  end;
  VarList.Free;
end;

procedure TVariablesForm.VarChange(Sender: TObject);
var i, NewType: Integer;
begin
  if Silent then Exit;
  i := VarList.ItemIndex;
  if i < 0 then Exit;
  Changed := True;
  with WorkQuery.Variables.Data(i) do
  begin
    Name := MakeVarName(NameEdit.Text);
    NewType := 0;
    case TypeList.ItemIndex of
      0: NewType := otInteger;
      1: NewType := otFloat;
      2: NewType := otString;
      3: NewType := otDate;
      4: NewType := otLong;
      5: NewType := otLongRaw;
      6: NewType := otCursor;
      7: NewType := otCLOB;
      8: NewType := otBLOB;
      9: NewType := otBFile;
     10: NewType := otReference;
     11: NewType := otObject;
     12: NewType := otPLSQLString;
     13: NewType := otChar;
     14: NewType := otSubst;
     15: NewType := otTimestamp;
     16: NewType := otTimestampTZ;
     17: NewType := otTimestampLTZ;
    end;
    VarList.Items[i] := Name + ' (' + TypeString(NewType) + ')';
    SelectVariable(i);
    if BufType <> NewType then
    begin
      WorkQuery.DeclareVariable(Name, NewType);
      ValueEdit.Text := '';
      if TableCheck.Checked then DimPLSQLTable;
    end;
  end;
  EnableButtons;
end;

procedure TVariablesForm.NewBtnClick(Sender: TObject);
var i: Integer;
begin
  for i := 1 to 100 do
  begin
    if WorkQuery.VariableIndex('var' + IntToStr(i)) < 0 then
    begin
      WorkQuery.DeclareVariable('var' + IntToStr(i), otString);
      Break;
    end;
  end;
  BuildVarList;
  SelectVariable(VarList.Items.Count - 1);
  VarListClick(nil);
  NameEdit.SetFocus;
  Changed := True;
end;

procedure TVariablesForm.SelectVariable(Index: Integer);
begin
  VarList.ItemIndex := Index;
  if VarList.MultiSelect and (Index >= 0) then VarList.Selected[Index] := True;
end;

procedure TVariablesForm.DeleteBtnClick(Sender: TObject);
var Index: Integer;
begin
  Index := VarList.ItemIndex;
  WorkQuery.Variables.Data(VarList.ItemIndex).Free;
  BuildVarList;
  if Index >= VarList.Items.Count then Index := VarList.Items.Count - 1;
  SelectVariable(Index);
  VarListClick(nil);
  Changed := True;
end;

procedure TVariablesForm.ScanBtnClick(Sender: TObject);
begin
  FindVariables;
end;

procedure TVariablesForm.OKBtnClick(Sender: TObject);
begin
  if Screen.ActiveControl is TEdit then
  begin
    if Assigned(TEdit(Screen.ActiveControl).OnExit) then
      TEdit(Screen.ActiveControl).OnExit(Sender);
  end;
  CopyVariables(WorkQuery, CopiedQuery);
end;

procedure TVariablesForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'Variables property editor');
end;

procedure TVariablesForm.ValueEditExit(Sender: TObject);
begin
  if (ActiveControl = CancelBtn) or (ActiveControl = DeleteBtn) then Exit;
  try
    if ValueEdit.Text = '' then
      WorkQuery.SetVariable(NameEdit.Text, Null)
    else
      WorkQuery.SetVariable(NameEdit.Text, ValueEdit.Text);
    Changed := True;
  except
    on E:Exception do
    begin
      ShowMessage('Error converting value');
      ValueEdit.SetFocus;
    end;
  end;
end;

procedure TVariablesForm.DimPLSQLTable;
var i, TableSize, StringSize, VarType: Integer;
    VarName: string;
begin
  i := VarList.ItemIndex;
  if i < 0 then Exit;
  Changed := True;
  with WorkQuery.Variables.Data(i) do
  begin
    VarName := Name;
    VarType := BufType;
  end;
  if not TableCheck.Checked then
    WorkQuery.DeclareVariable(VarName, VarType)
  else begin
    if TableSizeEdit.Text = '' then
      TableSize := 0
    else
      TableSize := StrToInt(TableSizeEdit.Text);
    if StringSizeEdit.Text = '' then
      StringSize := 0
    else
      StringSize := StrToInt(StringSizeEdit.Text);
    WorkQuery.DeclareVariable(VarName, VarType);
    WorkQuery.DimPLSQLTable(VarName, TableSize, StringSize);
  end;
  SetMemoryPanel;
end;

procedure TVariablesForm.TableSizeEditExit(Sender: TObject);
begin
  if (ActiveControl = CancelBtn) or (ActiveControl = DeleteBtn) then Exit;
  try
    DimPLSQLTable;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      TableSizeEdit.SetFocus;
    end;
  end;
end;

procedure TVariablesForm.StringSizeEditExit(Sender: TObject);
begin
  if (ActiveControl = CancelBtn) or (ActiveControl = DeleteBtn) then Exit;
  try
    DimPLSQLTable;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      StringSizeEdit.SetFocus;
    end;
  end;
end;

procedure TVariablesForm.TableCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  Silent := True;
  try
    if TableCheck.Checked then
    begin
      EnableTableFields;
      TableSizeEdit.Text := '25';
      if TypeList.ItemIndex = 2 then StringSizeEdit.Text := '40';
    end;
    EnableTableFields;
    DimPLSQLTable;
  finally
    Silent := False;
  end;
end;

procedure TVariablesForm.TypeListChange(Sender: TObject);
begin
  Silent := True;
  try
    if TypeList.ItemIndex in [0, 1, 2, 3] then
    begin
      TableCheck.Enabled := True;
      EnableTableFields;
    end else begin
      TableCheck.Enabled := False;
      TableCheck.Checked := False;
      EnableTableFields;
    end;
  finally
    Silent := False;
  end;
  VarChange(Sender);
end;

procedure TVariablesForm.EnableTableFields;
var OldSilent: Boolean;
begin
  OldSilent := Silent;
  Silent := False;
  try
    TableCheck.Enabled := TypeList.ItemIndex in [0, 1, 2, 3];
    if TableCheck.Checked then
    begin
      TableSizeEdit.Enabled := True;
      if TableSizeEdit.Text = '' then TableSizeEdit.Text := '25';
      if TypeList.ItemIndex <> 2 then
      begin
        StringSizeEdit.Enabled := False;
        StringSizeEdit.Text := '';
      end else begin
        StringSizeEdit.Enabled := True;
        if StringSizeEdit.Text = '' then StringSizeEdit.Text := '40';
      end;
      ValueEdit.Enabled := False;
      ValueEdit.Text := '';
    end else begin
      TableSizeEdit.Text := '';
      TableSizeEdit.Enabled := False;
      StringSizeEdit.Text := '';
      StringSizeEdit.Enabled := False;
      ValueEdit.Enabled := True;
    end;
  finally
    Silent := OldSilent;
  end;
end;

procedure TVariablesForm.SetMemoryPanel;
var OldWidth, ElementSize, Memory: Integer;
    s: string;
begin
  if TableCheck.Checked then
  begin
    MemoryPanel.Visible := True;
    ElementSize := 0;
    case TypeList.ItemIndex of
      0: ElementSize := 22;
      1: ElementSize := 22;
      2: ElementSize := StrToInt(StringSizeEdit.Text);
      3: ElementSize := 7;
    end;
    Memory := ElementSize * StrToInt(TableSizeEdit.Text);
    if Memory > 32512 then
    begin
      MemoryLabel.Font.Color := clRed;
      s := ' bytes (Oracle8 only!) '
    end else begin
      MemoryLabel.Font.Color := clWindowText;
      s := ' bytes ';
    end;
    MemoryLabel.Caption := ' ' + IntToStr(Memory) + s;
    OldWidth := MemoryPanel.Width;
    MemoryPanel.Width := MemoryLabel.Width;
    MemoryPanel.Left := MemoryPanel.Left - (MemoryPanel.Width - OldWidth);
  end else begin
    MemoryPanel.Visible := False;
  end;
end;

procedure TVariablesForm.CopyBtnClick(Sender: TObject);
var i: Integer;
    S: string;
 function VarName(Name: string): string;
 begin
   Result := Name;
   if (Result <> '') and (Result[1] = ':') then Delete(Result, 1, 1);
 end;
begin
  S := '';
  for i := 0 to WorkQuery.Variables.Count - 1 do
  begin
    S := S + '  ' + QueryName + '.SetVariable(''' + VarName(WorkQuery.Variables.Data(i).Name) + ''', );' + #13#10;
  end;
  if S <> '' then ClipBoard.AsText := S;
end;

procedure TVariablesForm.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = vk_down) and (Shift = []) then
  begin
    key := 0;
    if VarList.ItemIndex < VarList.Items.Count-1 then
    begin
      VarList.ItemIndex := VarList.ItemIndex + 1;
      ValueEditExit(Nil);
      VarListClick(Nil);
      if Sender is TEdit then TEdit(Sender).SelectAll;
    end;
  end;
  if (key = vk_up) and (Shift = []) then
  begin
    key := 0;
    if VarList.ItemIndex > 0 then
    begin
      VarList.ItemIndex := VarList.ItemIndex - 1;
      ValueEditExit(Nil);
      VarListClick(Nil);
      if Sender is TEdit then TEdit(Sender).SelectAll;
    end;
  end;
end;

end.
