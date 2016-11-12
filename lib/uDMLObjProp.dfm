object frmDMLObjProp: TfrmDMLObjProp
  Left = 485
  Top = 232
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #23545#35937#23646#24615
  ClientHeight = 268
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    409
    268)
  PixelsPerInch = 96
  TextHeight = 12
  object Label2: TLabel
    Left = 23
    Top = 20
    Width = 24
    Height = 12
    Caption = #21517#31216
  end
  object Label1: TLabel
    Left = 24
    Top = 52
    Width = 24
    Height = 12
    Caption = #31867#22411
  end
  object Label3: TLabel
    Left = 24
    Top = 79
    Width = 24
    Height = 12
    Caption = #20869#23481
  end
  object edtName: TEdit
    Left = 60
    Top = 16
    Width = 330
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 237
    Top = 236
    Width = 71
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 315
    Top = 236
    Width = 71
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 2
  end
  object memoDes: TMemo
    Left = 60
    Top = 80
    Width = 330
    Height = 144
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ComboBox1: TComboBox
    Left = 60
    Top = 48
    Width = 125
    Height = 20
    ItemHeight = 12
    TabOrder = 4
    Text = 'ComboBox1'
    Items.Strings = (
      #30452#32447
      #22806#38190
      #31661#22836)
  end
  object ActionList1: TActionList
    Left = 20
    Top = 80
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
  end
end
