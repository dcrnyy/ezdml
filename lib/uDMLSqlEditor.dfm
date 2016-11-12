object frmDmlSqlEditor: TfrmDmlSqlEditor
  Left = 250
  Top = 286
  BorderIcons = [biSystemMenu, biMaximize]
  BorderWidth = 2
  Caption = 'SQL Tool'
  ClientHeight = 381
  ClientWidth = 593
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 450
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 12
  object Splitter1: TSplitter
    Left = 0
    Top = 225
    Width = 593
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    OnMoved = Splitter1Moved
  end
  object PanelRes: TPanel
    Left = 0
    Top = 228
    Width = 593
    Height = 153
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      593
      153)
    object Bevel3: TBevel
      Left = 12
      Top = 0
      Width = 567
      Height = 5
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
    end
    object Label1: TLabel
      Left = 0
      Top = 4
      Width = 593
      Height = 12
      Align = alTop
      Caption = '&Result:'
      FocusControl = DBGridRes
      ExplicitWidth = 42
    end
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 593
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object DBGridRes: TDBGrid
      Left = 0
      Top = 16
      Width = 593
      Height = 137
      Align = alClient
      Ctl3D = True
      DataSource = DataSourceRes
      ParentCtl3D = False
      TabOrder = 0
      TitleFont.Charset = GB2312_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = #23435#20307
      TitleFont.Style = []
      OnMouseUp = DBGridResMouseUp
    end
  end
  object PanelSql: TPanel
    Left = 0
    Top = 0
    Width = 593
    Height = 225
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 593
      Height = 225
      Align = alClient
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 593
        Height = 14
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object LabelSql: TLabel
          Left = 0
          Top = 0
          Width = 24
          Height = 12
          Cursor = crHandPoint
          Caption = 'S&QL:'
          Font.Charset = GB2312_CHARSET
          Font.Color = clHotLight
          Font.Height = -12
          Font.Name = #23435#20307
          Font.Style = []
          ParentFont = False
          OnMouseDown = LabelSqlMouseDown
        end
        object sbtnSqlPrev: TSpeedButton
          Left = 46
          Top = 1
          Width = 21
          Height = 14
          Caption = '<='
          Flat = True
          OnClick = sbtnSqlPrevClick
        end
        object sbtnSqlNext: TSpeedButton
          Left = 70
          Top = 1
          Width = 21
          Height = 14
          Caption = '=>'
          Flat = True
          OnClick = sbtnSqlNextClick
        end
        object sbtnRun: TSpeedButton
          Left = 183
          Top = 0
          Width = 82
          Height = 14
          Action = actExec
          Flat = True
        end
        object sbtnClearAll: TSpeedButton
          Left = 137
          Top = 0
          Width = 45
          Height = 14
          Hint = 'Clear editor'
          Caption = 'Clear'
          Flat = True
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnClearAllClick
        end
        object sbtnConnectDb: TSpeedButton
          Left = 91
          Top = 0
          Width = 45
          Height = 14
          Hint = 'Connect database'
          Caption = 'Connect'
          Flat = True
          ParentShowHint = False
          ShowHint = True
          OnClick = sbtnConnectDbClick
        end
        object lbStatus: TLabel
          Left = 304
          Top = 1
          Width = 84
          Height = 12
          Caption = 'Status message'
        end
      end
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 68
    object actEditSelall: TAction
      Category = 'Edit'
      Caption = 'actEditSelall'
      OnExecute = actEditSelallExecute
    end
    object actExec: TAction
      Category = 'Sql'
      Caption = 'Execute(F8)'
      ShortCut = 119
      OnExecute = actExecExecute
      OnUpdate = actExecUpdate
    end
    object actCopyRec: TAction
      Category = 'Edit'
      Caption = 'Copy record'
      OnExecute = actCopyRecExecute
    end
    object actCopyAll: TAction
      Category = 'Edit'
      Caption = 'Copy all'
      OnExecute = actCopyAllExecute
    end
    object actRollback: TAction
      Category = 'Sql'
      Caption = 'rollback'
    end
    object actCommit: TAction
      Category = 'Sql'
      Caption = 'commit'
    end
  end
  object DataSourceRes: TDataSource
    Left = 48
    Top = 248
  end
  object PopupMenuOldSql: TPopupMenu
    OnPopup = PopupMenuOldSqlPopup
    Left = 48
    Top = 68
  end
  object PopupMenuGrid: TPopupMenu
    OnPopup = PopupMenuGridPopup
    Left = 80
    Top = 248
    object N5: TMenuItem
      Action = actCopyRec
    end
    object N1: TMenuItem
      Action = actCopyAll
    end
  end
  object TimerInit: TTimer
    Interval = 500
    OnTimer = TimerInitTimer
    Left = 108
    Top = 76
  end
end
