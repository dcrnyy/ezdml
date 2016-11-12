object frmCtGenSQL: TfrmCtGenSQL
  Left = 319
  Top = 170
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Generate Database'
  ClientHeight = 385
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    640
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 12
    Width = 501
    Height = 66
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 28
    Width = 40
    Height = 13
    Caption = 'Connect'
  end
  object Label2: TLabel
    Left = 32
    Top = 51
    Width = 22
    Height = 13
    Caption = 'User'
  end
  object Label3: TLabel
    Left = 8
    Top = 99
    Width = 37
    Height = 13
    Caption = 'Objects'
  end
  object Panel2: TPanel
    Left = 223
    Top = 94
    Width = 409
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    DesignSize = (
      409
      233)
    object tpgAction: TPageControl
      Left = 0
      Top = 0
      Width = 409
      Height = 233
      ActivePage = tbsResSQL
      Align = alClient
      MultiLine = True
      TabOrder = 0
      object tbsResSQL: TTabSheet
        Caption = 'Result'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memSQL: TMemo
          Left = 0
          Top = 0
          Width = 401
          Height = 205
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tbsExecSQL: TTabSheet
        Caption = 'Execute'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memExecSQL: TMemo
          Left = 0
          Top = 0
          Width = 401
          Height = 205
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tbsResError: TTabSheet
        Caption = 'Error'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memError: TMemo
          Left = 0
          Top = 0
          Width = 401
          Height = 205
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
    object chkPause: TCheckBox
      Left = 302
      Top = 1
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Stop on error'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object edtDBLinkInfo: TEdit
    Left = 96
    Top = 24
    Width = 365
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object btnDBLogon: TButton
    Left = 466
    Top = 24
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnDBLogonClick
  end
  object combDBUser: TComboBox
    Left = 96
    Top = 47
    Width = 365
    Height = 21
    ItemHeight = 0
    TabOrder = 2
  end
  object cklbDbObjs: TCheckListBox
    Left = 8
    Top = 117
    Width = 209
    Height = 210
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 333
    Width = 640
    Height = 52
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      640
      52)
    object LabelProg: TLabel
      Left = 66
      Top = 30
      Width = 333
      Height = 12
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '0/0'
      ExplicitWidth = 419
    end
    object Label4: TLabel
      Left = 6
      Top = 6
      Width = 42
      Height = 13
      Caption = 'Progress'
    end
    object ProgressBar1: TProgressBar
      Left = 66
      Top = 7
      Width = 321
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 573
      Top = 5
      Width = 47
      Height = 21
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnBuildSQL: TButton
      Left = 393
      Top = 5
      Width = 72
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Gen SQL(&B)'
      TabOrder = 2
      OnClick = btnBuildSQLClick
    end
    object btnExecSQL: TButton
      Left = 465
      Top = 5
      Width = 53
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Execute'
      TabOrder = 3
      OnClick = btnExecSQLClick
    end
    object btnResum: TButton
      Left = 518
      Top = 5
      Width = 52
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Ignore'
      Enabled = False
      TabOrder = 4
      OnClick = btnResumClick
    end
  end
  object btnSelDbType: TButton
    Left = 488
    Top = 24
    Width = 12
    Height = 21
    Caption = 'v'
    TabOrder = 6
    OnClick = btnSelDbTypeClick
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 80
    Top = 144
    object MN_CheckAll: TMenuItem
      AutoCheck = True
      Caption = 'Select/unselect all'
      OnClick = MN_CheckAllClick
    end
    object MN_CheckSelected: TMenuItem
      AutoCheck = True
      Caption = 'Check/uncheck'
      OnClick = MN_CheckSelectedClick
    end
    object MN_InverseSel: TMenuItem
      Caption = 'Inverse select'
      OnClick = MN_InverseSelClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MnShowPhyName: TMenuItem
      Caption = 'Show physical name'
      OnClick = MnShowPhyNameClick
    end
  end
  object ActionList1: TActionList
    Left = 112
    Top = 144
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
  end
  object TimerInit: TTimer
    Interval = 500
    OnTimer = TimerInitTimer
    Left = 144
    Top = 144
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dbk'
    Filter = 'Database backup files(*.dbk)|*.dbk'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 172
    Top = 144
  end
  object PopupMenuSelDefDbType: TPopupMenu
    Left = 512
    Top = 24
    object MN_DefDbType_ORACLE: TMenuItem
      Caption = 'ORACLE'
      OnClick = MN_DefDbType_StandardClick
    end
    object MN_DefDbType_MYSQL: TMenuItem
      Caption = 'MYSQL'
      OnClick = MN_DefDbType_StandardClick
    end
    object MN_DefDbType_SQLSERVER: TMenuItem
      Caption = 'SQLSERVER'
      OnClick = MN_DefDbType_StandardClick
    end
    object MN_DefDbType_Standard: TMenuItem
      Caption = 'STANDARD'
      OnClick = MN_DefDbType_StandardClick
    end
  end
end
