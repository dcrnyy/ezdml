object frmFindHex: TfrmFindHex
  Left = 369
  Top = 305
  BorderStyle = bsDialog
  Caption = 'Char Code Tool'
  ClientHeight = 156
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbText: TLabel
    Left = 4
    Top = 12
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object lbNumber: TLabel
    Left = 4
    Top = 36
    Width = 40
    Height = 13
    Caption = 'Number:'
  end
  object lbHex: TLabel
    Left = 4
    Top = 92
    Width = 22
    Height = 13
    Caption = 'Hex:'
  end
  object edtText: TEdit
    Left = 56
    Top = 8
    Width = 220
    Height = 21
    TabOrder = 0
    OnChange = edtTextChange
  end
  object MemoNum: TMemo
    Left = 56
    Top = 36
    Width = 220
    Height = 49
    TabOrder = 1
    OnChange = MemoNumChange
  end
  object MemoHex: TMemo
    Left = 56
    Top = 92
    Width = 220
    Height = 53
    TabOrder = 2
    OnChange = MemoHexChange
  end
  object btnOK: TButton
    Left = 288
    Top = 8
    Width = 75
    Height = 21
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 3
  end
  object combNumDig: TComboBox
    Left = 288
    Top = 36
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = '8 bit int'
    OnChange = ckbUnicodeClick
    Items.Strings = (
      '8 bit int'
      '16 bit int'
      '32 bit int'
      '32 bit float'
      '64 bit int'
      '64 bit float')
  end
  object combCharset: TComboBox
    Left = 288
    Top = 63
    Width = 77
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'ANSI'
    OnChange = ckbUnicodeClick
    Items.Strings = (
      'ANSI'
      'Unicode'
      'UTF-8')
  end
  object ActionList1: TActionList
    Left = 12
    Top = 112
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
  end
end
