// Direct Oracle Access - Constraint property editor form
// Copyright 1998, 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleConstraintsEdit;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, OracleData, OracleVisual;
{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QComCtrls, QStdCtrls, OracleData, QImgList, OracleVisual;
{$ENDIF}

type
  TConstraintForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ListView: TListView;
    ImageList: TImageList;
    procedure HelpBtnClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  end;

function ExecuteDisabledConstraintsEditor(D: TOracleDataSet): Boolean;

implementation

{$R *.dfm}

uses
  Oracle;

type
  // Trick to access the 'protected' part of TOracleDataSet
  TTempOracleDataSet = class(TOracleDataSet)
  end;


function ExecuteDisabledConstraintsEditor(D: TOracleDataSet): Boolean;
var TD: TTempOracleDataSet;
    TableInfo: TOracleTableInfo;
    ConstraintInfo: TOracleConstraintInfo;
    i, j: Integer;
    Item: TListItem;
begin
  TD := TTempOracleDataSet(D);
  TableInfo := nil;
  if (TD.Session = nil) or (not TD.Session.Connected) then
    ShowMessage('Session not connected, cannot read constraints')
  else
    TableInfo := TD.GetOracleTableInfo;
  with TConstraintForm.Create(nil) do
  begin
    Caption := D.Owner.Name + '.' + D.Name + ' Disabled Constraints';
//  ListView.Items.Clear; DOESN'T WORK IN DELPHI 4!!!!!!!!!
    // Place constraints from database dictionary in list
    if TableInfo <> nil then
    begin
      TableInfo.GetConstraints(D);
      for i := 0 to TableInfo.OracleConstraintInfoList.Count - 1 do
      begin
        ConstraintInfo := TOracleConstraintInfo(TableInfo.OracleConstraintInfoList.Items[i]);
        Item := ListView.Items.Add;
        Item.Caption := ConstraintInfo.ConstraintName;
        ConstraintInfo.ReadFromDictionary(D);
        case ConstraintInfo.ConstraintType of
          ctPrimaryKey: Item.SubItems.Add('Primary key');
           ctUniqueKey: Item.SubItems.Add('Unique key');
          ctForeignKey: Item.SubItems.Add('Foreign key');
               ctCheck: Item.SubItems.Add('Check');
        else
          Item.SubItems.Add('Unknown');
        end;
        if ConstraintInfo.Enabled then
          Item.SubItems.Add('Enabled')
        else
          Item.SubItems.Add('Disabled');
      end;
    end;
    // Mark disabled constraints
    for i := 0 to TD.OracleDictionary.DisabledConstraints.Count - 1 do
    begin
      Item := nil;
      for j := 0 to ListView.Items.Count - 1 do
      begin
        if ListView.Items[j].Caption = TD.OracleDictionary.DisabledConstraints[i] then
          Item := ListView.Items[j];
      end;
      if Item <> nil then
        Item.ImageIndex := 1
      else begin
        Item := ListView.Items.Add;
        Item.ImageIndex := 1;
        Item.Caption := TD.OracleDictionary.DisabledConstraints[i];
        Item.SubItems.Add('?');
        Item.SubItems.Add('?');
      end;
    end;
    Result := ShowModal = mrOK;
    if Result then
    begin
      TD.OracleDictionary.DisabledConstraints.Clear;
      for i := 0 to ListView.Items.Count - 1 do
      begin
        if ListView.Items[i].ImageIndex = 1 then
          TD.OracleDictionary.DisabledConstraints.Add(ListView.Items[i].Caption);
      end;
    end;
    Free;
  end;
end;

procedure TConstraintForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'Disabled Constraints property editor');
end;

procedure TConstraintForm.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Item: TListItem;
begin
  if X < 20 then
  begin
    Item := ListView.Selected;
    if Item <> nil then Item.ImageIndex := Item.ImageIndex xor 1;
  end;
end;

procedure TConstraintForm.ListViewDblClick(Sender: TObject);
var Item: TListItem;
begin
  Item := ListView.Selected;
  if Item <> nil then Item.ImageIndex := Item.ImageIndex xor 1;
end;

procedure TConstraintForm.FormActivate(Sender: TObject);
begin
  InitForm(Self);
end;

end.
