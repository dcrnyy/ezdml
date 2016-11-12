object frmPostgreSQLDBConfig: TfrmPostgreSQLDBConfig
  Left = 447
  Top = 474
  BorderStyle = bsDialog
  Caption = 'PostgreSQL Connection'
  ClientHeight = 152
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 28
    Top = 24
    Width = 39
    Height = 13
    Caption = 'Host IP:'
  end
  object Label2: TLabel
    Left = 236
    Top = 24
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label3: TLabel
    Left = 28
    Top = 54
    Width = 50
    Height = 13
    Caption = 'Database:'
  end
  object edtIP: TEdit
    Left = 102
    Top = 20
    Width = 126
    Height = 21
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 271
    Top = 20
    Width = 53
    Height = 21
    TabOrder = 1
    Text = '1521'
  end
  object edtDbName: TEdit
    Left = 102
    Top = 50
    Width = 222
    Height = 21
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 102
    Top = 105
    Width = 94
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 206
    Top = 105
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
