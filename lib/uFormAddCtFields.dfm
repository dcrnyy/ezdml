object frmAddCtFields: TfrmAddCtFields
  Left = 324
  Top = 269
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add system fields'
  ClientHeight = 326
  ClientWidth = 444
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
  object Label2: TLabel
    Left = 17
    Top = 11
    Width = 102
    Height = 13
    Caption = 'Choose fields to add:'
  end
  object btnOK: TButton
    Left = 244
    Top = 283
    Width = 86
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 339
    Top = 283
    Width = 87
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cklbFields: TCheckListBox
    Left = 17
    Top = 39
    Width = 409
    Height = 226
    Columns = 3
    ItemHeight = 16
    Style = lbOwnerDrawFixed
    TabOrder = 2
  end
  object ckbSelAll: TCheckBox
    Left = 17
    Top = 282
    Width = 62
    Height = 18
    Caption = 'Select all'
    TabOrder = 3
    OnClick = ckbSelAllClick
  end
  object rdbFNameTp1: TRadioButton
    Left = 299
    Top = 9
    Width = 62
    Height = 18
    Caption = 'English'
    TabOrder = 4
    OnClick = rdgFNameTpClick
  end
  object rdbFNameTp2: TRadioButton
    Left = 368
    Top = 9
    Width = 62
    Height = 18
    Caption = 'Local'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = rdgFNameTpClick
  end
end
