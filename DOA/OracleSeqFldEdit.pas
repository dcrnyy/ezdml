// Direct Oracle Access - SequenceField property editor form
// Copyright 1998, 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleSeqFldEdit;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Oracle, OracleData, OracleVisual;
{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, Oracle, OracleData, OracleVisual;
{$ENDIF}

type
  TSequenceFieldForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    SequenceLogon: TOracleLogon;
    SequenceQuery: TOracleQuery;
    SequenceSession: TOracleSession;
    Label1: TLabel;
    SequenceListBox: TComboBox;
    Label2: TLabel;
    FieldListBox: TComboBox;
    ApplyGroup: TRadioGroup;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DropDownList(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    Changed: Boolean;
    Dropped: Boolean;
  end;

implementation

{$R *.dfm}

procedure TSequenceFieldForm.FormCreate(Sender: TObject);
begin
  Changed := False;
  Dropped := False;
end;

procedure TSequenceFieldForm.FormActivate(Sender: TObject);
begin
  InitForm(Self);
end;

procedure TSequenceFieldForm.DropDownList(Sender: TObject);
var s: string;
begin
  if Dropped then Exit;
  SequenceLogon.Execute;
  if SequenceSession.Connected then with SequenceQuery do
  begin
    Execute;
    while not Eof do
    begin
      if not FieldIsNull(0) then s := Field(0) + '.' else s := '';
      s := s + Field(1);
      SequenceListBox.Items.Add(s);
      Next;
    end;
  end;
  Dropped := True;
end;

procedure TSequenceFieldForm.EditChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TSequenceFieldForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'SequenceField property editor');
end;

end.
