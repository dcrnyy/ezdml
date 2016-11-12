// Direct Oracle Access - DirectPathLoader Columns Editor unit
// Copyright 2000, 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleDPColumnsEdit;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Oracle, StdCtrls, checklst, ExtCtrls, Buttons, OraclePreferences, OracleVisual;
{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  Oracle, QStdCtrls, QExtCtrls, QButtons, OraclePreferences, OracleVisual;
{$ENDIF}

type
  TDirectPathColumnsForm = class(TForm)
    ButtonPanel: TPanel;
    HelpPanel: TPanel;
    MainPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ColumnsGroupBox: TGroupBox;
    ListPanel: TPanel;
    EditPanel: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    NewBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    ButtonBevel: TBevel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    Label1: TLabel;
    Label6: TLabel;
    NameEdit: TEdit;
    DataSizeEdit: TEdit;
    DataTypeComboBox: TComboBox;
    DateFormatEdit: TEdit;
    DefColBtn: TButton;
    ColumnListBox: TListBox;
    procedure FormDestroy(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure ColumnListBoxClick(Sender: TObject);
    procedure ColumnChange(Sender: TObject);
    procedure ColumnEditExit(Sender: TObject);
    procedure DataTypeComboBoxChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DefColBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    WorkColumns: TDirectPathColumns;
    WorkLoader: TOracleDirectPathLoader;
    Changed: Boolean;
    Silent: Boolean;
    procedure EnableButtons;
    procedure ColumnDataToEdit;
    procedure ColumnEditToData(DoBuild: Boolean);
    procedure BuildColumnList;
    function  CreateNewColumn: TDirectPathColumn;
    function  Validate(AnEdit: TEdit; RaiseException: Boolean): Boolean;
    function  SelectedColumn: TDirectPathColumn;
  end;

function ExecuteDirectPathColumnEditor(L: TOracleDirectPathLoader): Boolean;

implementation

{$R *.dfm}

function ExecuteDirectPathColumnEditor(L: TOracleDirectPathLoader): Boolean;
begin
  with TDirectPathColumnsForm.Create(nil) do
  begin
    WorkLoader := TOracleDirectPathLoader.Create(nil);
    WorkLoader.Columns   := L.Columns;
    WorkLoader.Session   := L.Session;
    WorkLoader.TableName := L.TableName;
    WorkColumns := WorkLoader.Columns;
    Caption := L.Owner.Name + '.' + L.Name + ' Columns';
    BuildColumnList;
    ColumnDataToEdit;
    Result :=(ShowModal = mrOK) and Changed;
    if Result then L.Columns := WorkLoader.Columns;
    Free;
  end;
end;

procedure TDirectPathColumnsForm.FormCreate(Sender: TObject);
begin
  // First Calculate default Width & Height
  Width  := EditPanel.Width + 30 + 200;
  Height := ButtonPanel.Height * 7 + 10;
  if OpenRegistry('DPColumns Editor') then
  begin
    Left   := ReadInteger('Left', Left);
    Top    := ReadInteger('Top', Top);
    Width  := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    CloseRegistry;
  end;
end;

procedure TDirectPathColumnsForm.FormActivate(Sender: TObject);
begin
  {$IFDEF CompilerVersion4}
  Constraints.MinHeight := 10 + Height - (EditPanel.Height - (DefColBtn.Top + DefColBtn.Height));
  Constraints.MinWidth  := 10 + Width - ColumnListBox.Width;
  {$ENDIF}
  InitForm(Self);
end;

procedure TDirectPathColumnsForm.FormDestroy(Sender: TObject);
begin
  if OpenRegistry('DPColumns Editor') then
  begin
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
    CloseRegistry;
  end;
  WorkLoader.Free;
end;

procedure TDirectPathColumnsForm.EnableButtons;
var oi: Integer;
begin
  oi := ColumnListBox.ItemIndex;
  DeleteBtn.Enabled        := (oi >= 0);
  UpBtn.Enabled            := (oi >  0);
  DownBtn.Enabled          := (oi <  WorkColumns.Count - 1);
  NameEdit.Enabled         := (oi >= 0);
  DataTypeComboBox.Enabled := NameEdit.Enabled;
  DataSizeEdit.Enabled     := NameEdit.Enabled and
                              not (WorkColumns[oi].DataType in [dpInteger, dpFloat]);
  DateFormatEdit.Enabled   := NameEdit.Enabled and
                              (WorkColumns[oi].DataType = dpString);
end;

procedure TDirectPathColumnsForm.ColumnDataToEdit;
var Index: Integer;
begin
  Silent := True;
  try
    Index := ColumnListBox.ItemIndex;
    if Index < 0 then
    begin
      NameEdit.Text       := '';
      DataTypeComboBox.ItemIndex := -1;
      DataSizeEdit.Text   := '';
      DateFormatEdit.Text := '';
    end else with WorkColumns[Index] do
    begin
      NameEdit.Text       := Name;
      if Ord(DataType) >= DataTypeComboBox.Items.Count then
        DataTypeComboBox.ItemIndex := DataTypeComboBox.Items.Count - 1
      else
        DataTypeComboBox.ItemIndex := Ord(DataType);
      DataSizeEdit.Text   := IntToStr(DataSize);
      DateFormatEdit.Text := DateFormat;
    end;
  finally
    Silent := False;
  end;
end;

function TDirectPathColumnsForm.Validate(AnEdit: TEdit; RaiseException: Boolean): Boolean;
var Value, Code: Integer;
begin
  Val(AnEdit.Text, Value, Code);
  Result := (Code = 0) and (Value >= 0);
  if (not Result) and RaiseException then
  begin
    AnEdit.SetFocus;
    raise Exception.Create('Invalid value: "' + AnEdit.Text + '"');
  end;
end;

procedure TDirectPathColumnsForm.ColumnEditToData(DoBuild: Boolean);
var Col: TDirectPathColumn;
begin
  if not Validate(DataSizeEdit, False) then Exit;
  Col := SelectedColumn;
  if Col = nil then Exit;
  Col.Name       := NameEdit.Text;
  Col.DataType   := TDirectPathColumnType(DataTypeComboBox.ItemIndex);
  Col.DataSize   := StrToInt(DataSizeEdit.Text);
  Col.DateFormat := DateFormatEdit.Text;
  if DoBuild then BuildColumnList;
end;

procedure TDirectPathColumnsForm.BuildColumnList;
var i, oi: Integer;
begin
  LockWindowUpdate(Integer(Handle));
  oi := ColumnListBox.ItemIndex;
  ColumnListBox.Clear;
  for i := 0 to WorkColumns.Count - 1 do
    ColumnListBox.Items.Add(WorkColumns[i].Name);
  if (WorkColumns.Count > 0) and (oi < 0) then oi := 0;
  ColumnListBox.ItemIndex := oi;
  EnableButtons;
  LockWindowUpdate(0);
end;

function TDirectPathColumnsForm.CreateNewColumn: TDirectPathColumn;
begin
  Result := WorkColumns.Add('NEW');
  with Result do
  begin
    DataType := dpString;
    DataSize := 10;
  end;
end;

function TDirectPathColumnsForm.SelectedColumn: TDirectPathColumn;
begin
  if ColumnListBox.ItemIndex >= 0 then
    Result := WorkColumns.Items[ColumnListBox.ItemIndex]
  else
    Result := nil;
end;

procedure TDirectPathColumnsForm.NewBtnClick(Sender: TObject);
var Item: TDirectPathColumn;
begin
  Item := CreateNewColumn;
  if ColumnListBox.ItemIndex >= 0 then
    Item.Index := ColumnListBox.ItemIndex + 1;
  BuildColumnList;
  ColumnListBox.ItemIndex := Item.Index;
  ColumnDataToEdit;
  EnableButtons;
  Changed := True;
end;

procedure TDirectPathColumnsForm.DeleteBtnClick(Sender: TObject);
var Index: Integer;
begin
  Index := ColumnListBox.ItemIndex;
  if Index < 0 then Exit;
  WorkColumns[Index].Free;
  if Index > WorkColumns.Count - 1 then ColumnListBox.ItemIndex := Index - 1;
  BuildColumnList;
  ColumnDataToEdit;
  Changed := True;
end;

procedure TDirectPathColumnsForm.UpBtnClick(Sender: TObject);
var Index: Integer;
begin
  Index := ColumnListBox.ItemIndex;
  if Index < 1 then Exit;
  WorkColumns[Index].Index := Index - 1;
  ColumnListBox.ItemIndex := Index - 1;
  BuildColumnList;
  Changed := True;
end;

procedure TDirectPathColumnsForm.DownBtnClick(Sender: TObject);
var Index: Integer;
begin
  Index := ColumnListBox.ItemIndex;
  if (Index < 0) or (Index >= WorkColumns.Count - 1) then Exit;
  WorkColumns[Index].Index := Index + 1;
  ColumnListBox.ItemIndex := Index + 1;
  BuildColumnList;
  Changed := True;
end;

procedure TDirectPathColumnsForm.ColumnListBoxClick(Sender: TObject);
begin
  ColumnDataToEdit;
  EnableButtons;
end;

procedure TDirectPathColumnsForm.ColumnChange(Sender: TObject);
begin
  if not Silent then
  begin
    ColumnEditToData(True);
    Changed := True;
  end;
end;

procedure TDirectPathColumnsForm.ColumnEditExit(Sender: TObject);
begin
  Validate(TEdit(Sender), True);
end;

procedure TDirectPathColumnsForm.DataTypeComboBoxChange(Sender: TObject);
begin
  if not Silent then
  begin
    ColumnChange(Sender);
    Silent := True;
    try
      ColumnDataToEdit;
    finally
      Silent := False;
    end;
    EnableButtons;
  end;
end;

procedure TDirectPathColumnsForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'Direct Path Columns property editor');
end;

procedure TDirectPathColumnsForm.DefColBtnClick(Sender: TObject);
var Btn: Integer;
    StringsOnly: Boolean;
begin
  if (WorkLoader.Session = nil) or (not WorkLoader.Session.Connected) then
  begin
    ShowMessage('Session must be connected to determine default columns');
    Exit;
  end;
  if Trim(WorkLoader.TableName) = '' then
  begin
    ShowMessage('TableName must be defined to determine default columns');
    Exit;
  end;
  if (WorkColumns.Count > 0) and
     (Confirm('Replace current Direct Path Columns with default columns?',
       'Confirm', 'YN') <> idYes) then Exit;
  Btn := Confirm('Load integer and float columns as strings?',
          'Confirm', 'YNC');
  if Btn = idCancel then Exit;
  StringsOnly := (Btn = idYes);
  try
    WorkLoader.GetDefaultColumns(StringsOnly);
    Changed := True;
  finally
    ColumnListBox.ItemIndex := -1;
    BuildColumnList;
    ColumnDataToEdit;
  end;
end;

end.
