// Direct Oracle Access - QBE Definition property editor form
// Copyright 1998, 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleQBEEdit;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion6} Variants, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OracleData, ExtCtrls, OraclePreferences, Buttons, OracleVisual;
{$ELSE}
uses
  Types, Variants, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, OraclePreferences, OracleData, QExtCtrls, QButtons, OracleVisual;
{$ENDIF}

type
  TLocalQBEDefinition = class(TQBEDefinition);
  
type
  TQBEEditForm = class(TForm)
    ButtonPanel: TPanel;
    HelpPanel: TPanel;
    MainPanel: TPanel;
    TopPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    FieldBox: TGroupBox;
    FieldPanel: TPanel;
    EditPanel: TPanel;
    FieldList: TListBox;
    QueryableCheck: TCheckBox;
    AutoPartialMatchCheck: TCheckBox;
    CaseInsensitiveCheck: TCheckBox;
    TestBtn: TButton;
    IgnoreTimeCheck: TCheckBox;
    AutoContainsCheck: TCheckBox;
    GeneralGroupBox: TGroupBox;
    SaveQBEValuesCheck: TCheckBox;
    AllowFileWildCardsCheck: TCheckBox;
    QBEFontColorList: TComboBox;
    QBEBkgColorList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ColorBevel: TBevel;
    PickFontColorBtn: TSpeedButton;
    PickBkgColorBtn: TSpeedButton;
    ColorDialog: TColorDialog;
    AllowOperatorsCheck: TCheckBox;
    AutoSoundexCheck: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure FieldListClick(Sender: TObject);
    procedure QueryableCheckClick(Sender: TObject);
    procedure AutoPartialMatchCheckClick(Sender: TObject);
    procedure CaseInsensitiveCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure IgnoreTimeCheckClick(Sender: TObject);
    procedure AutoContainsCheckClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure QBEColorListDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure PickBkgColorBtnClick(Sender: TObject);
    procedure PickFontColorBtnClick(Sender: TObject);
    procedure AutoSoundexCheckClick(Sender: TObject);
  private
  public
    Changed: Boolean;
    Silent: Boolean;
    DataSet: TOracleDataSet;
    WorkQBEDefinition: TLocalQBEDefinition;
    FieldsDisabled: Boolean;
    procedure RefreshFields;
    procedure UpdateFieldList;
    procedure UpdateFieldDef;
    function  GetListColor(L: TComboBox): TColor;
    procedure SelectListColor(L: TComboBox; C: TColor);
    procedure GetColorProc(const S: string);
  end;

function ExecuteQBEDefinitionEditor(ADataSet: TOracleDataSet): Boolean;

implementation

uses DB, Oracle;

{$R *.dfm}

procedure CopyQBEDefinition(Source, Dest: TQBEDEfinition);
begin
  Dest.Assign(Source);
end;

function ExecuteQBEDefinitionEditor(ADataSet: TOracleDataSet): Boolean;
begin
  Result := False;
  with TQBEEditForm.Create(nil) do
  begin
    DataSet := ADataSet;
    try
      DataSet.FieldDefs.Update;
      FieldsDisabled := False;
    except
      on E: exception do
      begin
        QueryableCheck.Enabled := False;
        AutoPartialMatchCheck.Enabled := False;
        AutoContainsCheck.Enabled := False;
        AutoSoundexCheck.Enabled := False;
        CaseInsensitiveCheck.Enabled := False;
        IgnoreTimeCheck.Enabled := False;
        TestBtn.Enabled := False;
        FieldsDisabled := True;
        ShowMessage('Field definitions cannot be accessed for the following reason:'#13#10#13#10 +
                     E.Message + #13#10#13#10'As a result, the QBE Field properties are disabled.');
      end;
    end;
    Silent := True;
    SaveQBEValuesCheck.Checked := DataSet.QBEDefinition.SaveQBEValues;
    AllowFileWildcardsCheck.Checked := DataSet.QBEDefinition.AllowFileWildCards;
    AllowOperatorsCheck.Checked := DataSet.QBEDefinition.AllowOperators;
    SelectListColor(QBEFontColorList, DataSet.QBEDefinition.QBEFontColor);
    SelectListColor(QBEBkgColorList, DataSet.QBEDefinition.QBEBackgroundColor);
    Silent := False;
    WorkQBEDefinition := TLocalQBEDefinition.Create(ADataSet);
    CopyQBEDefinition(DataSet.QBEDefinition, WorkQBEDefinition);
    RefreshFields;
    UpdateFieldList;
    if FieldList.Items.Count > 0 then FieldList.ItemIndex := 0;
    UpdateFieldDef;
    Caption := DataSet.Owner.Name + '.' + DataSet.Name + ' QBE Definition';
    if (ShowModal = mrOK) and Changed then
    begin
      WorkQBEDefinition.SaveQBEValues      := SaveQBEValuesCheck.Checked;
      WorkQBEDefinition.AllowFileWildCards := AllowFileWildcardsCheck.Checked;
      WorkQBEDefinition.AllowOperators     := AllowOperatorsCheck.Checked;
      WorkQBEDefinition.QBEFontColor       := GetListColor(QBEFontColorList);
      WorkQBEDefinition.QBEBackgroundColor := GetListColor(QBEBkgColorList);
      CopyQBEDefinition(WorkQBEDefinition, DataSet.QBEDefinition);
      Result := True;
    end;
    WorkQBEDefinition.Free;
    Free;
  end;
end;

function TQBEEditForm.GetListColor(L: TComboBox): TColor;
begin
  if L.ItemIndex < 0 then
    Result := clNone
  else begin
    try
      if L.Items[L.ItemIndex] = 'Other' then
        Result := L.Tag
      else
        Result := StringToColor(L.Items[L.ItemIndex]);
    except
      Result := clNone;
    end;
  end;
end;

procedure TQBEEditForm.SelectListColor(L: TComboBox; C: TColor);
var i: Integer;
    S: string;
begin
  try
    S := ColorToString(C);
    if Copy(S, 1, 2) <> 'cl' then
    begin
      S := 'Other';
      L.Tag := C;
    end;
  except
    S := 'clNone';
  end;
  for i := 0 to L.Items.Count - 1 do
  begin
    if S = L.Items[i] then
    begin
      L.ItemIndex := i;
      Exit;
    end;
  end;
end;

procedure TQBEEditForm.GetColorProc(const S: string);
begin
  QBEFontColorList.Items.Add(S);
  QBEBkgColorList.Items.Add(S);
end;

procedure TQBEEditForm.FormCreate(Sender: TObject);
var MinHeight: Integer;
begin
  Changed := False;
  FieldList.Clear;
  // First Calculate default Width & Height
  Width  := EditPanel.Width + 30 + 220;
  Height := ButtonPanel.Height * 10 + 10;
  if OpenRegistry('QBE Editor') then
  begin
    Left   := ReadInteger('Left', Left);
    Top    := ReadInteger('Top', Top);
    Width  := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    MinHeight := TestBtn.Top + TestBtn.Height + 4;
    if EditPanel.Height < MinHeight then
      Height := Height + MinHeight - EditPanel.Height;
    CloseRegistry;
  end;
  GetColorValues(GetColorProc);
  QBEFontColorList.Items.Add('Other');
  QBEBkgColorList.Items.Add('Other');
end;

procedure TQBEEditForm.FormActivate(Sender: TObject);
begin
  PickBkgColorBtn.Height  := QBEBkgColorList.Height;
  PickBkgColorBtn.Width   := QBEBkgColorList.Height;
  PickFontColorBtn.Height := QBEFontColorList.Height;
  PickFontColorBtn.Width  := QBEFontColorList.Height;
  TopPanel.Height := ColorBevel.Height + 2 * ColorBevel.Top + 10;
  {$IFDEF CompilerVersion4}
  Constraints.MinHeight := 10 + Height - (EditPanel.Height - (TestBtn.Top + TestBtn.Height));
  Constraints.MinWidth  := 10 + Width - FieldList.Width;
  {$ENDIF}
  InitForm(Self);
end;

procedure TQBEEditForm.FormDestroy(Sender: TObject);
begin
  if OpenRegistry('QBE Editor') then
  begin
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
    CloseRegistry;
  end;
end;

procedure TQBEEditForm.UpdateFieldList;
var i: Integer;
    OldIndex: Integer;
    s: string;
begin
  LockWindowUpdate(Integer(Handle));
  OldIndex := FieldList.ItemIndex;
  FieldList.Clear;
  for i := 0 to WorkQBEDefinition.FieldCount - 1 do
  begin
    with WorkQBEDefinition.Fields[i] do
    begin
      s := '';
      if Queryable then s := s + 'Q';
      if AutoPartialMatch then s := s + 'P';
      if AutoContains then s := s + 'C';
      if AutoSoundex then s := s + 'S';
      if CaseInsensitive then s := s + 'U';
      if IgnoreTime then s := s + 'I';
      s := s + #9 + FieldName;
      FieldList.Items.Add(s);
    end;
  end;
  if OldIndex < FieldList.Items.Count then FieldList.ItemIndex := OldIndex;
  LockWindowUpdate(0);
end;

procedure TQBEEditForm.UpdateFieldDef;
var QBEField: TQBEField;
    fi: Integer;
begin
  Silent := True;
  LockWindowUpdate(Integer(Handle));
  if FieldList.ItemIndex >= 0 then
  begin
    QBEField := WorkQBEDefinition.Fields[FieldList.ItemIndex];
    QueryableCheck.Checked := QBEField.Queryable;
    AutoPartialMatchCheck.Checked := QBEField.AutoPartialMatch;
    AutoContainsCheck.Checked := QBEField.AutoContains;
    AutoSoundexCheck.Checked := QBEField.AutoSoundex;
    CaseInsensitiveCheck.Checked := QBEField.CaseInsensitive;
    IgnoreTimeCheck.Checked := QBEField.IgnoreTime;
    if not FieldsDisabled then
    begin
      QueryableCheck.Enabled := False;
      AutoPartialMatchCheck.Enabled := False;
      AutoContainsCheck.Enabled := False;
      AutoSoundexCheck.Enabled := False;
      CaseInsensitiveCheck.Enabled := False;
      IgnoreTimeCheck.Enabled := False;
      fi := DataSet.FieldDefs.IndexOf(QBEField.FieldName);
      if (fi >= 0) and not (DataSet.FieldDefs[fi].DataType in [ftBlob, ftGraphic, ftMemo]) then
      begin
        QueryableCheck.Enabled := True;
        if DataSet.FieldDefs[fi].DataType in [ftString, ftMemo] then
        begin
          AutoPartialMatchCheck.Enabled := True;
          AutoContainsCheck.Enabled := True;
          AutoSoundexCheck.Enabled := True;
          CaseInsensitiveCheck.Enabled  := True;
        end;
        if DataSet.FieldDefs[fi].DataType in [ftDate, ftDateTime] then
        begin
          IgnoreTimeCheck.Enabled  := True;
        end;
      end;
    end;
  end else begin
    QueryableCheck.Checked := False;
    AutoPartialMatchCheck.Checked := False;
    AutoContainsCheck.Checked := False;
    AutoSoundexCheck.Checked := False;
    CaseInsensitiveCheck.Checked  := False;
    IgnoreTimeCheck.Checked := False;
    QueryableCheck.Enabled := False;
    AutoPartialMatchCheck.Enabled := False;
    AutoContainsCheck.Enabled := False;
    AutoSoundexCheck.Enabled := False;
    CaseInsensitiveCheck.Enabled := False;
    IgnoreTimeCheck.Enabled := False;
  end;
  LockWindowUpdate(0);
  Silent := False;
end;

procedure TQBEEditForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'QBE Definition property editor');
end;

procedure TQBEEditForm.RefreshFields;
begin
  if FieldsDisabled then Exit;
  if WorkQBEDEfinition.Update then Changed := True;
end;

procedure TQBEEditForm.FieldListClick(Sender: TObject);
begin
  UpdateFieldDef;
end;

procedure TQBEEditForm.QueryableCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].Queryable :=
    QueryableCheck.Checked;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.AutoPartialMatchCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].AutoPartialMatch :=
    AutoPartialMatchCheck.Checked;
  if AutoPartialMatchCheck.Checked then AutoSoundexCheck.Checked := False;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.AutoContainsCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].AutoContains :=
    AutoContainsCheck.Checked;
  if AutoContainsCheck.Checked then AutoSoundexCheck.Checked := False;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.AutoSoundexCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].AutoSoundex :=
    AutoSoundexCheck.Checked;
  if AutoSoundexCheck.Checked then
  begin
    CaseInsensitiveCheck.Checked := False;
    AutoContainsCheck.Checked := False;
    AutoPartialMatchCheck.Checked := False;
  end;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.CaseInsensitiveCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].CaseInsensitive :=
    CaseInsensitiveCheck.Checked;
  if CaseInsensitiveCheck.Checked then AutoSoundexCheck.Checked := False;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.IgnoreTimeCheckClick(Sender: TObject);
begin
  if Silent then Exit;
  WorkQBEDEfinition.Fields[FieldList.ItemIndex].IgnoreTime :=
    IgnoreTimeCheck.Checked;
  UpdateFieldList;
  Changed := True;
end;

procedure TQBEEditForm.Change(Sender: TObject);
begin
  if Silent then Exit;
  Changed := True;
end;

procedure TQBEEditForm.TestBtnClick(Sender: TObject);
var i: Integer;
    QBEField: TQBEField;
    Field: TField;
    OldQBEMode, OldActive, OldDebug: Boolean;
    Err: string;
begin
  Err := '';
  DataSet.DisableControls;
  OldQBEMode := DataSet.QBEMode;
  OldActive := DataSet.Active;
  OldDebug := DataSet.Debug;
  try
    DataSet.Debug := False;
    DataSet.QBEMode := True;
    DataSet.Active := True;
    for i := 0 to WorkQBEDefinition.FieldCount - 1 do
    begin
      QBEField := WorkQBEDefinition.Fields[i];
      if QBEField.Queryable then
      begin
        Field := DataSet.FieldByName(QBEField.FieldName);
        DataSet.DescribeQBE;
        if Field.DataType in [ftDate, ftDateTime] then
          Field.Value := Date
        else
          Field.Value := '0';
        try
          DataSet.DescribeQBE
        except
          on E: EOracleError do
          begin
            if Err = '' then Err := 'An error occurred for the following fields:'#13#10;
            Err := Err + #13#10 + Field.FieldName + ' - ' + E.Message;
            if E.ErrorCode = 918 then
            begin
              Err := Err + '    This indicates that this column name exists in more than one table'#13#10;
              Err := Err + '    Set the Origin property of this field to <table>.<column>'#13#10;
            end;
            if E.ErrorCode = 904 then
            begin
              Err := Err + '    This indicates that the column may have an alias'#13#10;
              Err := Err + '    Set the Origin property of this field to <table>.<column>'#13#10;
            end;
          end;
        end;
        Field.Value := Null;
      end;
    end;
    DataSet.DescribeQBE;
  finally
    DataSet.QBEMode := OldQBEMode;
    DataSet.Active := OldActive;
    DataSet.Debug := OldDebug;
    if DataSet.ControlsDisabled then DataSet.EnableControls;
  end;
  if Err <> '' then ShowMessage(Err) else ShowMessage('Query test passed successfully.');
end;

procedure TQBEEditForm.QBEColorListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Col, OldBrushColor: Integer;
begin
  with Control as TComboBox do
  begin
    OldBrushColor := Brush.Color;
    Canvas.FillRect(Rect);
    {$IFNDEF LINUX}
    SetTextAlign(Canvas.Handle, TA_UPDATECP);
    {$ENDIF}
    Canvas.MoveTo(Rect.Left + ItemHeight, Rect.Top);
    Canvas.TextOut(0, 0, Items[Index]);
    Canvas.MoveTo(Rect.Left + 1, Rect.Top);
    if Items[Index] = 'Other' then
      Canvas.Brush.Color := Control.Tag
    else begin
      if IdentToColor(Items[Index], Col) then
        Canvas.Brush.Color := Col
      else
        Canvas.Brush.Color := clWindow;
    end;
    Canvas.Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + 14, Rect.Top + 14);
    Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure TQBEEditForm.PickBkgColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := QBEBkgColorList.Tag;
  if ColorDialog.Execute then
  begin
    Changed := True;
    QBEBkgColorList.Tag := ColorDialog.Color;
    SelectListColor(QBEBkgColorList, ColorDialog.Color);
  end;
end;

procedure TQBEEditForm.PickFontColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := QBEFontColorList.Tag;
  if ColorDialog.Execute then
  begin
    Changed := True;
    QBEFontColorList.Tag := ColorDialog.Color;
    SelectListColor(QBEFontColorList, ColorDialog.Color);
  end;
end;



end.
