object frmDMLImport: TfrmDMLImport
  Left = 373
  Top = 170
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #23548#20837#25968#25454#24211#23545#35937
  ClientHeight = 351
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    532
    351)
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 60
    Height = 12
    Caption = #36830#25509#25968#25454#24211
  end
  object Label2: TLabel
    Left = 20
    Top = 48
    Width = 48
    Height = 12
    Caption = #36873#25321#29992#25143
  end
  object Label3: TLabel
    Left = 20
    Top = 100
    Width = 72
    Height = 12
    Caption = #36873#25321#23548#20837#23545#35937
  end
  object Label4: TLabel
    Left = 20
    Top = 289
    Width = 48
    Height = 12
    Anchors = [akLeft, akBottom]
    Caption = #23548#20837#36827#24230
  end
  object Label5: TLabel
    Left = 20
    Top = 76
    Width = 48
    Height = 12
    Caption = #36807#28388#26465#20214
  end
  object LabelProg: TLabel
    Left = 96
    Top = 314
    Width = 18
    Height = 12
    Anchors = [akLeft, akBottom]
    Caption = '0/0'
  end
  object btnOK: TButton
    Left = 342
    Top = 319
    Width = 81
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = #23548#20837
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 430
    Top = 319
    Width = 81
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #20851#38381
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object edtDBLinkInfo: TEdit
    Left = 96
    Top = 16
    Width = 401
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object btnDBLogon: TButton
    Left = 496
    Top = 16
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnDBLogonClick
  end
  object combDBUser: TComboBox
    Left = 96
    Top = 44
    Width = 421
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 12
    TabOrder = 4
    OnChange = combDBUserChange
  end
  object cklbDbObjs: TCheckListBox
    Left = 96
    Top = 100
    Width = 421
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 3
    ItemHeight = 12
    PopupMenu = PopupMenu1
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 96
    Top = 289
    Width = 421
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 6
  end
  object combObjFilter: TComboBox
    Left = 96
    Top = 72
    Width = 421
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ItemHeight = 12
    TabOrder = 7
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 152
    Top = 164
    object MN_CheckAll: TMenuItem
      AutoCheck = True
      Caption = #20840#36873'/'#20840#19981#36873
      OnClick = MN_CheckAllClick
    end
    object MN_CheckSelected: TMenuItem
      AutoCheck = True
      Caption = #25171#21246'/'#21462#28040
      OnClick = MN_CheckSelectedClick
    end
    object MN_InverseSel: TMenuItem
      Caption = #21453#36873
      OnClick = MN_InverseSelClick
    end
  end
end
