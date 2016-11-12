unit wDmlHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, StdActns;

type
  TfrmHelpAbout = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    ActionList1: TActionList;
    EditSelectAll1: TEditSelectAll;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmHelpAbout: TfrmHelpAbout;

implementation

uses ezdmlstrs, dmlstrs;

{$R *.dfm}

procedure TfrmHelpAbout.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn := ExtractFilePath(Application.ExeName);
  if Copy(fn, Length(fn), 1) <> '\' then
    fn := fn + '\';
  if srIsEnglish then
    fn := fn + 'readme.txt'
  else
    fn := fn + 'readme_CHS.txt';
  //Memo1.Lines[0] := Format(Memo1.Lines[0], [srEzdmlVersionNum]);
  if FileExists(fn) then
    Memo1.Lines.LoadFromFile(fn);
end;

procedure TfrmHelpAbout.FormShow(Sender: TObject);
begin
  Memo1.WordWrap := True;
end;

procedure TfrmHelpAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

