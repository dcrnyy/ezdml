object frmImportCtTable: TfrmImportCtTable
  Left = 357
  Top = 168
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Import Database'
  ClientHeight = 346
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    582
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 22
    Top = 22
    Width = 40
    Height = 13
    Caption = 'Connect'
  end
  object Label2: TLabel
    Left = 22
    Top = 52
    Width = 76
    Height = 13
    Caption = 'Choose Schema'
  end
  object Label3: TLabel
    Left = 22
    Top = 108
    Width = 41
    Height = 13
    Caption = 'Objects:'
  end
  object Label4: TLabel
    Left = 22
    Top = 284
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Progress:'
    ExplicitTop = 311
  end
  object Label5: TLabel
    Left = 22
    Top = 82
    Width = 24
    Height = 13
    Caption = 'Filter'
  end
  object LabelProg: TLabel
    Left = 104
    Top = 311
    Width = 16
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '0/0'
    ExplicitTop = 338
  end
  object btnOK: TButton
    Left = 376
    Top = 316
    Width = 88
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 471
    Top = 316
    Width = 88
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object edtDBLinkInfo: TEdit
    Left = 104
    Top = 17
    Width = 440
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object btnDBLogon: TButton
    Left = 543
    Top = 17
    Width = 23
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnDBLogonClick
  end
  object combDBUser: TComboBox
    Left = 104
    Top = 48
    Width = 462
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    OnChange = combDBUserChange
  end
  object cklbDbObjs: TCheckListBox
    Left = 104
    Top = 108
    Width = 462
    Height = 148
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 3
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 104
    Top = 284
    Width = 462
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 8
  end
  object combObjFilter: TComboBox
    Left = 104
    Top = 78
    Width = 462
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 9
    OnChange = combObjFilterChange
  end
  object ckbAutoCapitalize: TCheckBox
    Left = 264
    Top = 259
    Width = 129
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Auto capitalize'
    TabOrder = 6
  end
  object ckbComments2DisplayName: TCheckBox
    Left = 399
    Top = 259
    Width = 154
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Comments as Logic-Name'
    TabOrder = 7
  end
  object ckbImportDbTypeNames: TCheckBox
    Left = 104
    Top = 259
    Width = 154
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Import data-type names'
    TabOrder = 10
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 152
    Top = 164
    object MN_CheckAll: TMenuItem
      AutoCheck = True
      Caption = 'All select/unselect'
      OnClick = MN_CheckAllClick
    end
    object MN_CheckSelected: TMenuItem
      AutoCheck = True
      Caption = 'Check/uncheck'
      OnClick = MN_CheckSelectedClick
    end
    object MN_InverseSel: TMenuItem
      Caption = 'Inverse selection'
      OnClick = MN_InverseSelClick
    end
  end
  object TimerInit: TTimer
    Interval = 500
    OnTimer = TimerInitTimer
    Left = 180
    Top = 164
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'dbk'
    Filter = 'Database backup files(*.dbk)|*.dbk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 208
    Top = 164
  end
end
