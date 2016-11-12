object frmColorStyles: TfrmColorStyles
  Left = 405
  Top = 170
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Model Options'
  ClientHeight = 422
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 208
    Top = 376
    Width = 108
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 323
    Top = 376
    Width = 84
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 9
    Top = 9
    Width = 404
    Height = 361
    ActivePage = TabSheet2
    TabOrder = 2
    object TabSheet2: TTabSheet
      Caption = 'Objects'
      ImageIndex = 1
      object Label15: TLabel
        Left = 16
        Top = 191
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Label16: TLabel
        Left = 9
        Top = 167
        Width = 56
        Height = 13
        Caption = 'Foreign key'
      end
      object Label9: TLabel
        Left = 9
        Top = 63
        Width = 37
        Height = 13
        Caption = 'Caption'
      end
      object Label3: TLabel
        Left = 16
        Top = 87
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Bevel1: TBevel
        Left = 87
        Top = 17
        Width = 276
        Height = 3
        Shape = bsTopLine
      end
      object Bevel3: TBevel
        Left = 86
        Top = 68
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Bevel4: TBevel
        Left = 86
        Top = 172
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label12: TLabel
        Left = 16
        Top = 36
        Width = 60
        Height = 13
        Caption = 'Background:'
      end
      object Label18: TLabel
        Left = 9
        Top = 12
        Width = 26
        Height = 13
        Caption = 'Table'
      end
      object Label4: TLabel
        Left = 9
        Top = 219
        Width = 32
        Height = 13
        Caption = 'Border'
      end
      object Label5: TLabel
        Left = 16
        Top = 243
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Bevel6: TBevel
        Left = 86
        Top = 224
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label19: TLabel
        Left = 16
        Top = 139
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Label20: TLabel
        Left = 9
        Top = 115
        Width = 56
        Height = 13
        Caption = 'Primary key'
      end
      object Bevel10: TBevel
        Left = 86
        Top = 120
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label21: TLabel
        Left = 16
        Top = 295
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Label22: TLabel
        Left = 9
        Top = 271
        Width = 19
        Height = 13
        Caption = 'Line'
      end
      object Bevel11: TBevel
        Left = 86
        Top = 276
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object clbExKey: TColorBox
        Left = 89
        Top = 186
        Width = 227
        Height = 22
        DefaultColorColor = clBlue
        NoneColorColor = clBlue
        Selected = clBlue
        ItemHeight = 16
        TabOrder = 6
      end
      object btnExKey: TButton
        Left = 322
        Top = 186
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 7
        OnClick = btnExKeyClick
      end
      object clbTitle: TColorBox
        Left = 89
        Top = 82
        Width = 227
        Height = 22
        DefaultColorColor = clRed
        NoneColorColor = clRed
        Selected = clRed
        ItemHeight = 16
        TabOrder = 2
      end
      object btnTitle: TButton
        Left = 322
        Top = 82
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 3
        OnClick = btnTitleClick
      end
      object clbFill: TColorBox
        Left = 90
        Top = 31
        Width = 227
        Height = 22
        DefaultColorColor = clWhite
        NoneColorColor = clWhite
        Selected = clWhite
        ItemHeight = 16
        TabOrder = 0
      end
      object btnFill: TButton
        Left = 323
        Top = 31
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 1
        OnClick = btnFillClick
      end
      object clbBorderColor: TColorBox
        Left = 89
        Top = 238
        Width = 227
        Height = 22
        DefaultColorColor = clSkyBlue
        NoneColorColor = clSkyBlue
        Selected = clSkyBlue
        ItemHeight = 16
        TabOrder = 8
      end
      object btnBorderColor: TButton
        Left = 322
        Top = 238
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 9
        OnClick = btnBorderColorClick
      end
      object clbPrimaryKey: TColorBox
        Left = 89
        Top = 134
        Width = 227
        Height = 22
        DefaultColorColor = clPurple
        NoneColorColor = clPurple
        Selected = clPurple
        ItemHeight = 16
        TabOrder = 4
      end
      object btnPK: TButton
        Left = 322
        Top = 134
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 5
        OnClick = btnPKClick
      end
      object clbLineColor: TColorBox
        Left = 89
        Top = 290
        Width = 227
        Height = 22
        DefaultColorColor = clBlue
        NoneColorColor = clBlue
        Selected = clBlue
        ItemHeight = 16
        TabOrder = 10
      end
      object btnLinkLine: TButton
        Left = 322
        Top = 290
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 11
        OnClick = btnLinkLineClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Working Area'
      ImageIndex = 1
      object Bevel2: TBevel
        Left = 87
        Top = 75
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label14: TLabel
        Left = 17
        Top = 93
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Label13: TLabel
        Left = 10
        Top = 69
        Width = 43
        Height = 13
        Caption = 'Selection'
      end
      object Label1: TLabel
        Left = 16
        Top = 151
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label2: TLabel
        Left = 17
        Top = 176
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object Label11: TLabel
        Left = 16
        Top = 36
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object Label17: TLabel
        Left = 10
        Top = 11
        Width = 56
        Height = 13
        Caption = 'Background'
      end
      object Bevel5: TBevel
        Left = 87
        Top = 15
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label6: TLabel
        Left = 10
        Top = 126
        Width = 19
        Height = 13
        Caption = 'Size'
      end
      object Bevel7: TBevel
        Left = 87
        Top = 130
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Bevel8: TBevel
        Left = 87
        Top = 208
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label7: TLabel
        Left = 10
        Top = 204
        Width = 33
        Height = 13
        Caption = 'Others'
      end
      object btnSelect: TButton
        Left = 323
        Top = 89
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 3
        OnClick = btnSelectClick
      end
      object clbSelect: TColorBox
        Left = 90
        Top = 89
        Width = 227
        Height = 22
        DefaultColorColor = clSkyBlue
        NoneColorColor = clSkyBlue
        Selected = clSkyBlue
        ItemHeight = 16
        TabOrder = 2
      end
      object edtWorkspaceWidth: TEdit
        Left = 89
        Top = 146
        Width = 227
        Height = 21
        TabOrder = 4
        OnChange = edtWorkspaceWidthChange
      end
      object edtWorkspaceHeight: TEdit
        Left = 89
        Top = 172
        Width = 227
        Height = 21
        TabOrder = 5
        OnChange = edtWorkspaceHeightChange
      end
      object chlShowFieldType: TCheckBox
        Left = 222
        Top = 224
        Width = 127
        Height = 18
        Caption = 'Show field type'
        TabOrder = 7
      end
      object clbBackGround: TColorBox
        Left = 90
        Top = 31
        Width = 227
        Height = 22
        DefaultColorColor = clWhite
        NoneColorColor = clWhite
        Selected = clWhite
        ItemHeight = 16
        TabOrder = 0
      end
      object btnBackGround: TButton
        Left = 323
        Top = 31
        Width = 18
        Height = 24
        Caption = '..'
        TabOrder = 1
        OnClick = btnBackGroundClick
      end
      object chlShowFieldIcon: TCheckBox
        Left = 90
        Top = 223
        Width = 127
        Height = 18
        Caption = 'Show field icon'
        TabOrder = 6
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Database'
      ImageIndex = 2
      object Label10: TLabel
        Left = 10
        Top = 11
        Width = 46
        Height = 13
        Caption = 'Database'
      end
      object Bevel9: TBevel
        Left = 87
        Top = 15
        Width = 276
        Height = 2
        Shape = bsTopLine
      end
      object Label8: TLabel
        Left = 21
        Top = 36
        Width = 28
        Height = 13
        Caption = 'Type:'
      end
      object combDbEngine: TComboBox
        Left = 94
        Top = 32
        Width = 227
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 0
        Items.Strings = (
          'ORACLE'
          'MYSQL'
          'SQLSERVER'
          'STANDARD')
      end
      object ckbGenFKIndexesSQL: TCheckBox
        Left = 94
        Top = 68
        Width = 203
        Height = 17
        Caption = 'Generate Indexes SQL for FK Fields'
        TabOrder = 1
      end
    end
  end
end
