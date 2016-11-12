object frmAddTbLink: TfrmAddTbLink
  Left = 412
  Top = 258
  BorderStyle = bsDialog
  Caption = 'Add Link'
  ClientHeight = 167
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 13
    Top = 89
    Width = 47
    Height = 13
    Caption = 'Link type:'
  end
  object Label4: TLabel
    Left = 13
    Top = 52
    Width = 60
    Height = 13
    Caption = 'Master field:'
  end
  object Label2: TLabel
    Left = 13
    Top = 24
    Width = 64
    Height = 13
    Caption = 'Master table:'
  end
  object Label3: TLabel
    Left = 282
    Top = 52
    Width = 58
    Height = 13
    Caption = 'Relate field:'
  end
  object Label5: TLabel
    Left = 282
    Top = 24
    Width = 58
    Height = 13
    Caption = 'Detail table:'
  end
  object combLinkType: TComboBox
    Left = 78
    Top = 86
    Width = 92
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'Foreign key'
    OnChange = combLinkTypeChange
    Items.Strings = (
      'Foreign key'
      'Direct'
      'Line')
  end
  object combMasterField: TComboBox
    Left = 78
    Top = 49
    Width = 170
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 182
    Top = 129
    Width = 87
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 280
    Top = 129
    Width = 86
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object edtMasterTb: TEdit
    Left = 78
    Top = 21
    Width = 170
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 4
  end
  object combRelateField: TComboBox
    Left = 347
    Top = 49
    Width = 170
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
  end
  object edtDetailTb: TEdit
    Left = 347
    Top = 21
    Width = 170
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
end
