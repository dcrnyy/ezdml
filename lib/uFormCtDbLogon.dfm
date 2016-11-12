object frmLogonCtDB: TfrmLogonCtDB
  Left = 412
  Top = 258
  BorderStyle = bsDialog
  Caption = 'Database connection'
  ClientHeight = 198
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 22
    Width = 59
    Height = 13
    Alignment = taRightJustify
    Caption = 'Engine Type'
  end
  object Label2: TLabel
    Left = 52
    Top = 82
    Width = 22
    Height = 13
    Alignment = taRightJustify
    Caption = 'User'
  end
  object Label_Pwd: TLabel
    Left = 28
    Top = 113
    Width = 46
    Height = 13
    Alignment = taRightJustify
    Caption = 'Password'
    OnDblClick = Label_PwdDblClick
  end
  object Label4: TLabel
    Left = 16
    Top = 52
    Width = 58
    Height = 13
    Alignment = taRightJustify
    Caption = 'Data source'
  end
  object combDbType: TComboBox
    Left = 82
    Top = 17
    Width = 192
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'ORACLE'
    OnChange = combDbTypeChange
    Items.Strings = (
      'ORACLE')
  end
  object edtUserName: TEdit
    Left = 82
    Top = 78
    Width = 192
    Height = 21
    TabOrder = 3
  end
  object edtPassword: TEdit
    Left = 82
    Top = 108
    Width = 192
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object combDBName: TComboBox
    Left = 82
    Top = 48
    Width = 192
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 82
    Top = 167
    Width = 87
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 180
    Top = 167
    Width = 87
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object btnDBCfg: TButton
    Left = 277
    Top = 48
    Width = 19
    Height = 22
    Caption = '..'
    TabOrder = 2
    OnClick = btnDBCfgClick
  end
  object ckbSavePwd: TCheckBox
    Left = 82
    Top = 135
    Width = 135
    Height = 17
    Caption = 'Save password'
    TabOrder = 7
  end
end
