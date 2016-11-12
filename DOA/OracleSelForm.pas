// Direct Oracle Access - General selection form
// Copyright 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleSelForm;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;
{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QExtCtrls, QComCtrls, QStdCtrls;
{$ENDIF}

type
  TOracleSelectForm = class(TForm)
    BottomPanel: TPanel;
    PageControl: TPageControl;
    CollListBox: TListBox;
    CollSheet: TTabSheet;
    CollReadOnlyCheck: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure CollListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectCollectionForDataSet(const Collections: TStringList;
  out SelectedCollection: Integer; out ReadOnlyDataSet: Boolean): Boolean;

implementation

{$R *.dfm}

function SelectCollectionForDataSet(const Collections: TStringList;
  out SelectedCollection: Integer; out ReadOnlyDataSet: Boolean): Boolean;
var Form: TOracleSelectForm;
begin
  SelectedCollection := -1;
  ReadOnlyDataSet := False;
  Application.CreateForm(TOracleSelectForm, Form);
  with Form do
  begin
    Caption := 'Select Master Collection Column for Detail Dataset';
    PageControl.ActivePage := CollSheet;
    CollListBox.Items.Assign(Collections);
    CollListBox.ItemIndex := 0;
    Result := (ShowModal = mrOK);
    if Result then
    begin
      SelectedCollection := CollListBox.ItemIndex;
      ReadOnlyDataSet := CollReadOnlyCheck.Checked;
    end;
    Free;
  end;
end;

procedure TOracleSelectForm.CollListBoxDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.
