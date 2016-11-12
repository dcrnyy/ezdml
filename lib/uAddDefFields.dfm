object frmAddDefFields: TfrmAddDefFields
  Left = 435
  Top = 317
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = #28155#21152#40664#35748#23383#27573
  ClientHeight = 230
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 30
    Height = 12
    Caption = #34920#21517':'
  end
  object Label2: TLabel
    Left = 16
    Top = 44
    Width = 78
    Height = 12
    Caption = #35201#28155#21152#30340#23383#27573':'
  end
  object btnOK: TButton
    Left = 297
    Top = 17
    Width = 80
    Height = 21
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 297
    Top = 45
    Width = 80
    Height = 21
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 1
  end
  object edtTbName: TEdit
    Left = 96
    Top = 12
    Width = 177
    Height = 20
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object cklbFields: TCheckListBox
    Left = 96
    Top = 40
    Width = 177
    Height = 173
    ItemHeight = 16
    Style = lbOwnerDrawFixed
    TabOrder = 3
  end
  object rdgFNameTp: TRadioGroup
    Left = 296
    Top = 124
    Width = 81
    Height = 89
    Caption = #23383#27573#21517
    Items.Strings = (
      #31532#19968#31867
      #31532#20108#31867
      #31532#19977#31867)
    TabOrder = 4
    OnClick = rdgFNameTpClick
  end
  object ckbSelAll: TCheckBox
    Left = 20
    Top = 196
    Width = 57
    Height = 17
    Caption = #20840#36873
    TabOrder = 5
    OnClick = ckbSelAllClick
  end
end
