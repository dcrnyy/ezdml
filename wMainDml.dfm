object frmMainDml: TfrmMainDml
  Left = 239
  Top = 190
  Caption = 'EZDML'
  ClientHeight = 545
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 526
    Width = 698
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MemoGScript: TMemo
    Left = 296
    Top = 152
    Width = 185
    Height = 89
    Lines.Strings = (
      '//EZDML'#20840#23616#20107#20214#33050#26412
      ''
      
        '//'#29983#25104#21333#20010#34920#30340'SQL'#65292#20256#20837#34920#23545#35937#12289#26159#21542#29983#25104#21019#24314#34920'SQL'#12289#26159#21542#29983#25104#21019#24314#32422#26463'SQL'#12289#40664#35748#29983#25104#30340'SQL'#32467#26524#12289#25968#25454#24211#31867#22411#12289#36873#39033#65292#36820#22238#33258 +
        #23450#20041#32467#26524'SQL'
      
        'function OnEzdmlGenTbSqlEvent(tb: TCtMetaTable; bCreateTb, bCrea' +
        'teConstrains: Boolean; defRes, dbType, options: String): string;'
      'begin'
      '  Result := defRes;'
      'end;'
      ''
      '//'#29983#25104#25968#25454#24211#30340#26356#26032'SQL'#65292#20256#20837#26032#26087#34920#23545#35937#12289#40664#35748#29983#25104#30340'SQL'#32467#26524#12289#25968#25454#24211#31867#22411#12289#36873#39033#65292#36820#22238#33258#23450#20041#32467#26524'SQL'
      
        'function OnEzdmlGenDbSqlEvent(designTb, dbTable: TCtMetaTable; d' +
        'efRes, dbType, options: String): string;'
      'begin'
      '  Result := defRes;'
      'end;'
      ''
      
        '//'#29983#25104#21333#20010#23383#27573#30340#31867#22411#65288'varchar(255) nullable'#65289#65292#20256#20837#34920#23545#35937#12289#23383#27573#23545#35937#12289#40664#35748#29983#25104#30340#32467#26524#12289#25968#25454#24211#31867#22411#12289#36873#39033#65292#36820 +
        #22238#33258#23450#20041#32467#26524
      
        'function OnEzdmlGenFieldTypeDescEvent(tb: TCtMetaTable; fd: TCtM' +
        'etaField; defRes, dbType, options: String): string;'
      'begin'
      '  Result := defRes;'
      'end;'
      ''
      
        '//'#29983#25104#22686#21024#25913#21333#20010#23383#27573#30340'SQL'#65288'alter table add xxx'#65289#65292#20256#20837#35201#25191#34892#30340#25805#20316'(alter/add/drop)'#12289#34920#23545 +
        #35937#12289#26032#26087#23383#27573#23545#35937#12289#40664#35748#29983#25104#30340#32467#26524#12289#25968#25454#24211#31867#22411#12289#36873#39033#65292#36820#22238#33258#23450#20041#32467#26524
      
        'function OnEzdmlGenAlterFieldEvent(action: String; tb: TCtMetaTa' +
        'ble; designField, dbField: TCtMetaField; defRes, dbType, options' +
        ': String): string;'
      'begin'
      '  Result := defRes;'
      'end;'
      ''
      '//'#33258#23450#20041#21629#20196#20107#20214
      
        'function OnEzdmlCmdEvent(cmd, param1, param2: String; parobj1, p' +
        'arobj2: TObject): string;'
      'begin'
      '  Result := '#39#39';'
      'end;'
      ''
      'begin'
      'end.')
    TabOrder = 1
    WordWrap = False
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 108
    object Mn_File: TMenuItem
      Caption = '&File'
      object MnClear: TMenuItem
        Caption = 'New File'
        ShortCut = 16462
        OnClick = MnClearClick
      end
      object MnOpen1: TMenuItem
        Caption = 'Open'
        ShortCut = 16463
        OnClick = MnOpen1Click
      end
      object MnSave1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = MnSave1Click
      end
      object MN_Saveas: TMenuItem
        Caption = 'Save as'
        OnClick = MN_SaveasClick
      end
      object MN_LocInExplorer: TMenuItem
        Caption = 'Locate in Explorer'
        OnClick = MN_LocInExplorerClick
      end
      object Openlastfile1: TMenuItem
        Action = actOpenLastFile1
      end
      object MN_Recentfiles: TMenuItem
        Caption = 'Recent files'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnExitWithoutSave: TMenuItem
        Caption = 'Exit without save'
        OnClick = MnExitWithoutSaveClick
      end
      object MnExit1: TMenuItem
        Caption = 'Exit'
        OnClick = MnExit1Click
      end
    end
    object MN_Modal: TMenuItem
      Caption = 'Modal'
      object MNNewTb: TMenuItem
        Caption = 'New Table'
        OnClick = MNNewTbClick
      end
      object Mn_NewModal: TMenuItem
        Caption = 'New Modal'
        OnClick = Mn_NewModalClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnImportDB: TMenuItem
        Caption = 'Import'
        OnClick = MnImportDBClick
      end
      object MnGenSql: TMenuItem
        Caption = 'Generate'
        OnClick = MnGenSqlClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Mn_ShowPhyView: TMenuItem
        Caption = 'Physics/Logic View'
        OnClick = Mn_ShowPhyViewClick
      end
      object MN_ColorStyles: TMenuItem
        Caption = 'Color and Styles'
        OnClick = MN_ColorStylesClick
      end
      object MN_ExportXls: TMenuItem
        Caption = 'Export...'
        OnClick = MN_ExportXlsClick
      end
      object MN_ExecScript: TMenuItem
        Caption = 'Exec Script'
        OnClick = MN_ExecScriptClick
      end
      object MN_SearchFields: TMenuItem
        Caption = 'Find Objects'
        ShortCut = 16454
        OnClick = MN_SearchFieldsClick
      end
    end
    object MnTools1: TMenuItem
      Caption = 'Tools'
      object MN_EditINIfile: TMenuItem
        Caption = 'Edit INI file'
        OnClick = MN_EditINIfileClick
      end
      object MN_EditMyDict: TMenuItem
        Caption = 'Edit MyDict.txt'
        OnClick = MN_EditMyDictClick
      end
      object MN_editGlobalScript: TMenuItem
        Caption = 'Edit Global Script'
        OnClick = MN_editGlobalScriptClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MnBackupDatabase: TMenuItem
        Caption = 'Backup Database'
        OnClick = MnBackupDatabaseClick
      end
      object MnRestoreDatabase: TMenuItem
        Caption = 'Restore Database'
        OnClick = MnRestoreDatabaseClick
      end
      object MNSqlTool: TMenuItem
        Caption = 'Sql Tool'
        OnClick = MNSqlToolClick
      end
      object MN_FindHex: TMenuItem
        Caption = 'Char Code Tool'
        OnClick = MN_FindHexClick
      end
    end
    object Mn_Help: TMenuItem
      Caption = 'Help'
      object MnAbout1: TMenuItem
        Caption = 'Short Help'
        ShortCut = 112
        OnClick = MnAbout1Click
      end
      object mn_EzdmlHomePage: TMenuItem
        Caption = 'EZDML Home Page'
        OnClick = mn_EzdmlHomePageClick
      end
      object mn_EZDMLforum: TMenuItem
        Caption = 'EZDML Forum'
        OnClick = mn_EZDMLforumClick
      end
      object Mn_About: TMenuItem
        Caption = 'About'
        OnClick = Mn_AboutClick
      end
    end
  end
  object TimerInit: TTimer
    Interval = 500
    OnTimer = TimerInitTimer
    Left = 44
    Top = 80
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xml'
    Filter = 
      'All supported files(*.dmx;*.dmh)|*.dmx;*.dmh|XML files(*.dmx)|*.' +
      'dmx|Binary files(*.dmh)|*.dmh'
    Left = 72
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files(*.dmx)|*.dmx|Binary files(*.dmh)|*.dmh'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 44
    Top = 136
  end
  object XPManifest1: TXPManifest
    Left = 44
    Top = 108
  end
  object ActionList1: TActionList
    Left = 72
    Top = 136
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object actOpenLastFile1: TAction
      Caption = 'Open last file'
      ShortCut = 32817
      OnExecute = actOpenLastFile1Execute
    end
  end
  object TimerAutoSave: TTimer
    Interval = 60000
    OnTimer = TimerAutoSaveTimer
    Left = 44
    Top = 168
  end
end
