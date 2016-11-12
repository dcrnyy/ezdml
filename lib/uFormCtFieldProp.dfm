object frmCtMetaFieldProp: TfrmCtMetaFieldProp
  Left = 542
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Field properties'
  ClientHeight = 489
  ClientWidth = 472
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 447
    Width = 472
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TButton
      Left = 290
      Top = 9
      Width = 82
      Height = 27
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 381
      Top = 9
      Width = 82
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 28
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
  end
end
