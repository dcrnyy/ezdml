object frmDMLGenSQL: TfrmDMLGenSQL
  Left = 395
  Top = 178
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #29983#25104'SQL'
  ClientHeight = 438
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 11
    Top = 18
    Width = 60
    Height = 12
    Caption = #36830#25509#25968#25454#24211
  end
  object Label2: TLabel
    Left = 329
    Top = 18
    Width = 48
    Height = 12
    Caption = #36873#25321#29992#25143
  end
  object Label3: TLabel
    Left = 9
    Top = 47
    Width = 72
    Height = 12
    Caption = #36873#25321#23548#20837#23545#35937
  end
  object LabelProg: TLabel
    Left = 66
    Top = 423
    Width = 437
    Height = 11
    AutoSize = False
    Caption = '0/0'
  end
  object Label4: TLabel
    Left = 11
    Top = 401
    Width = 48
    Height = 12
    Caption = #21160#20316#36827#24230
  end
  object ProgressBar1: TProgressBar
    Left = 66
    Top = 400
    Width = 437
    Height = 15
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 500
    Top = 368
    Width = 75
    Height = 20
    Cancel = True
    Caption = #20851#38381
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object edtDBLinkInfo: TEdit
    Left = 81
    Top = 15
    Width = 186
    Height = 20
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object btnDBLogon: TButton
    Left = 271
    Top = 15
    Width = 20
    Height = 19
    Caption = '...'
    TabOrder = 2
    OnClick = btnDBLogonClick
  end
  object combDBUser: TComboBox
    Left = 377
    Top = 15
    Width = 185
    Height = 20
    ItemHeight = 12
    TabOrder = 3
  end
  object cklbDbObjs: TCheckListBox
    Left = 7
    Top = 62
    Width = 193
    Height = 323
    ItemHeight = 12
    PopupMenu = PopupMenu1
    TabOrder = 4
  end
  object btnBuildSQL: TButton
    Left = 207
    Top = 368
    Width = 75
    Height = 20
    Caption = #29983#25104'SQL(&B)'
    TabOrder = 6
    OnClick = btnBuildSQLClick
  end
  object tpgAction: TPageControl
    Left = 207
    Top = 62
    Width = 370
    Height = 304
    ActivePage = tbsResSQL
    MultiLine = True
    TabOrder = 7
    object tbsResSQL: TTabSheet
      Caption = #32467#26524
      object memSQL: TMemo
        Left = 0
        Top = 0
        Width = 362
        Height = 277
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbsExecSQL: TTabSheet
      Caption = #25191#34892
      ImageIndex = 1
      object memExecSQL: TMemo
        Left = 0
        Top = 0
        Width = 363
        Height = 278
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbsResError: TTabSheet
      Caption = #38169#35823
      ImageIndex = 2
      object memError: TMemo
        Left = 0
        Top = 0
        Width = 363
        Height = 278
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object btnExecSQL: TButton
    Left = 295
    Top = 368
    Width = 75
    Height = 20
    Caption = #25191#34892'(&E)'
    TabOrder = 8
    OnClick = btnExecSQLClick
  end
  object btnResum: TButton
    Left = 384
    Top = 368
    Width = 75
    Height = 20
    Caption = #24573#30053'(&I)'
    Enabled = False
    TabOrder = 9
    OnClick = btnResumClick
  end
  object btnOK: TButton
    Left = 508
    Top = 420
    Width = 74
    Height = 19
    Caption = #23548#20837
    TabOrder = 10
    Visible = False
  end
  object chkPause: TCheckBox
    Left = 487
    Top = 62
    Width = 90
    Height = 16
    Caption = #36935#21040#38169#35823#26242#20572
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 112
    Top = 92
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
    object N1: TMenuItem
      Caption = '-'
    end
    object N2: TMenuItem
      Caption = #26174#31034#29289#29702#34920#21517
      OnClick = N2Click
    end
  end
  object OracleScript1: TOracleScript
    OnCommand = OracleScript1Command
    AfterCommand = OracleScript1AfterCommand
    OnError = OracleScript1Error
    OnOutput = OracleScript1Output
    Left = 16
    Top = 448
  end
  object OracleQuery1: TOracleQuery
    Left = 152
    Top = 456
  end
end
