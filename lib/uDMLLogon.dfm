object frmLogonDB: TfrmLogonDB
  Left = 403
  Top = 254
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #36830#25509#25968#25454#24211
  ClientHeight = 191
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    292
    191)
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 28
    Top = 28
    Width = 48
    Height = 12
    Caption = #36830#25509#31867#22411
  end
  object Label2: TLabel
    Left = 40
    Top = 84
    Width = 36
    Height = 12
    Caption = #29992#25143#21517
  end
  object Label3: TLabel
    Left = 52
    Top = 112
    Width = 24
    Height = 12
    Caption = #23494#30721
  end
  object Label4: TLabel
    Left = 28
    Top = 56
    Width = 48
    Height = 12
    Caption = #25968#25454#24211#21517
  end
  object combDbType: TComboBox
    Left = 84
    Top = 24
    Width = 177
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    ItemIndex = 0
    TabOrder = 0
    Text = 'ORACLE'
    OnChange = combDbTypeChange
    Items.Strings = (
      'ORACLE')
  end
  object edtUserName: TEdit
    Left = 84
    Top = 80
    Width = 177
    Height = 20
    TabOrder = 2
  end
  object edtPassword: TEdit
    Left = 84
    Top = 108
    Width = 177
    Height = 20
    PasswordChar = '*'
    TabOrder = 3
  end
  object combDBName: TComboBox
    Left = 84
    Top = 52
    Width = 177
    Height = 20
    ItemHeight = 12
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 120
    Top = 159
    Width = 71
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 198
    Top = 159
    Width = 71
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 5
  end
end
