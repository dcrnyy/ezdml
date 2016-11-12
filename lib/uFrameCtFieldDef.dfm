object FrameCtFieldDef: TFrameCtFieldDef
  Left = 0
  Top = 0
  Width = 431
  Height = 437
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  DesignSize = (
    431
    437)
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 413
    Height = 421
    ActivePage = TabSheet2
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Field defination'
      object Label3: TLabel
        Left = 8
        Top = 168
        Width = 48
        Height = 13
        Caption = 'Data type'
      end
      object Label4: TLabel
        Left = 8
        Top = 196
        Width = 44
        Height = 13
        Caption = 'Field kind'
      end
      object Label6: TLabel
        Left = 8
        Top = 96
        Width = 50
        Height = 13
        Caption = 'Comments'
      end
      object Label9: TLabel
        Left = 204
        Top = 196
        Width = 19
        Height = 13
        Caption = 'Size'
      end
      object Label8: TLabel
        Left = 8
        Top = 296
        Width = 53
        Height = 13
        Caption = 'Index type'
      end
      object Label21: TLabel
        Left = 8
        Top = 224
        Width = 64
        Height = 13
        Caption = 'Default value'
      end
      object Label1: TLabel
        Left = 8
        Top = 320
        Width = 58
        Height = 13
        Caption = 'Relate table'
      end
      object Bevel1: TBevel
        Left = 4
        Top = 152
        Width = 393
        Height = 5
        Shape = bsTopLine
      end
      object Bevel2: TBevel
        Left = 4
        Top = 276
        Width = 393
        Height = 5
        Shape = bsTopLine
      end
      object Label18: TLabel
        Left = 8
        Top = 72
        Width = 14
        Height = 13
        Caption = 'Tip'
      end
      object Label19: TLabel
        Left = 8
        Top = 48
        Width = 53
        Height = 13
        Caption = 'Logic name'
      end
      object Label7: TLabel
        Left = 8
        Top = 20
        Width = 51
        Height = 13
        Caption = 'Field name'
      end
      object Label15: TLabel
        Left = 204
        Top = 168
        Width = 53
        Height = 13
        Caption = 'Type name'
      end
      object Label29: TLabel
        Left = 8
        Top = 344
        Width = 54
        Height = 13
        Caption = 'Relate field'
      end
      object sbtnSearchFields: TSpeedButton
        Left = 368
        Top = 15
        Width = 21
        Height = 22
        Hint = 'Import from exists table'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        OnClick = sbtnSearchFieldsClick
      end
      object edtDataLength: TEdit
        Left = 260
        Top = 192
        Width = 129
        Height = 21
        TabOrder = 7
        OnExit = edtNameExit
      end
      object combDataType: TComboBox
        Left = 72
        Top = 164
        Width = 129
        Height = 21
        Style = csDropDownList
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 4
        OnChange = combDataTypeChange
      end
      object combKeyFieldType: TComboBox
        Left = 72
        Top = 192
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        OnChange = edtNameExit
      end
      object memoMemo: TMemo
        Left = 72
        Top = 96
        Width = 317
        Height = 49
        ScrollBars = ssVertical
        TabOrder = 3
        OnExit = edtNameExit
      end
      object combIndexType: TComboBox
        Left = 72
        Top = 292
        Width = 317
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 10
        OnChange = edtNameExit
      end
      object edtDefaultValue: TEdit
        Left = 72
        Top = 220
        Width = 317
        Height = 21
        TabOrder = 8
        OnChange = edtDefaultValueChange
        OnExit = edtNameExit
      end
      object edtRelateTable: TComboBox
        Left = 72
        Top = 316
        Width = 317
        Height = 21
        ItemHeight = 13
        TabOrder = 11
        OnChange = edtRelateTableChange
        OnExit = edtNameExit
      end
      object ckbNullable: TCheckBox
        Left = 72
        Top = 247
        Width = 69
        Height = 17
        Caption = 'Nullable'
        TabOrder = 9
        OnClick = edtNameExit
      end
      object edtHint: TEdit
        Left = 72
        Top = 68
        Width = 317
        Height = 21
        TabOrder = 2
        OnExit = edtNameExit
      end
      object edtDisplayName: TEdit
        Left = 72
        Top = 44
        Width = 317
        Height = 21
        TabOrder = 1
        OnExit = edtNameExit
      end
      object edtName: TEdit
        Left = 72
        Top = 16
        Width = 297
        Height = 21
        TabOrder = 0
        OnExit = edtNameExit
      end
      object edtDataTypeName: TEdit
        Left = 260
        Top = 164
        Width = 129
        Height = 21
        TabOrder = 5
        OnExit = edtNameExit
      end
      object edtRelateField: TEdit
        Left = 72
        Top = 340
        Width = 317
        Height = 21
        TabOrder = 12
        OnExit = edtNameExit
      end
      object ckbAutoIncrement: TCheckBox
        Left = 162
        Top = 247
        Width = 95
        Height = 17
        Caption = 'Auto increment'
        TabOrder = 13
        OnClick = ckbAutoIncrementClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Editor UI'
      ImageIndex = 1
      object Label27: TLabel
        Left = 8
        Top = 172
        Width = 42
        Height = 13
        Caption = 'Value list'
      end
      object Label28: TLabel
        Left = 8
        Top = 224
        Width = 49
        Height = 13
        Caption = 'Dropdown'
      end
      object Label14: TLabel
        Left = 8
        Top = 76
        Width = 19
        Height = 13
        Caption = 'Unit'
      end
      object Label16: TLabel
        Left = 8
        Top = 20
        Width = 28
        Height = 13
        Caption = 'Editor'
      end
      object Label5: TLabel
        Left = 8
        Top = 48
        Width = 25
        Height = 13
        Caption = 'Label'
      end
      object Label20: TLabel
        Left = 8
        Top = 120
        Width = 55
        Height = 13
        Caption = 'Disp format'
      end
      object Label22: TLabel
        Left = 8
        Top = 144
        Width = 53
        Height = 13
        Caption = 'Edit format'
      end
      object Label23: TLabel
        Left = 8
        Top = 292
        Width = 51
        Height = 13
        Caption = 'Font name'
      end
      object Label24: TLabel
        Left = 208
        Top = 292
        Width = 43
        Height = 13
        Caption = 'Font size'
      end
      object Label25: TLabel
        Left = 8
        Top = 348
        Width = 82
        Height = 13
        Caption = 'Foreground color'
      end
      object Label26: TLabel
        Left = 208
        Top = 348
        Width = 82
        Height = 13
        Caption = 'Background color'
      end
      object Bevel3: TBevel
        Left = 4
        Top = 276
        Width = 393
        Height = 5
        Shape = bsTopLine
      end
      object Bevel4: TBevel
        Left = 4
        Top = 104
        Width = 393
        Height = 5
        Shape = bsTopLine
      end
      object memoDropDownItems: TMemo
        Left = 72
        Top = 168
        Width = 317
        Height = 45
        ScrollBars = ssVertical
        TabOrder = 5
        OnExit = edtNameExit
      end
      object combDropDownMode: TComboBox
        Left = 72
        Top = 220
        Width = 317
        Height = 21
        Style = csDropDownList
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 6
        OnChange = edtNameExit
        Items.Strings = (
          'Editable'
          'Fixed')
      end
      object combMeasureUnit: TComboBox
        Left = 72
        Top = 72
        Width = 317
        Height = 21
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 2
        OnExit = edtNameExit
      end
      object combEditorType: TComboBox
        Left = 72
        Top = 16
        Width = 317
        Height = 21
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 0
        OnExit = edtNameExit
        Items.Strings = (
          'TextEdit'
          'RadioBox'
          'SpinEdit'
          'Button'
          'NumberEdit'
          'CheckBox'
          'ComboBox'
          'CurrencyEdit'
          'DateEdit'
          'TimeEdit'
          'HyperLink'
          'Memo'
          'Picture'
          'PopupEdit')
      end
      object edtLabelText: TEdit
        Left = 72
        Top = 44
        Width = 317
        Height = 21
        TabOrder = 1
        OnExit = edtNameExit
      end
      object ckbEditorReadOnly: TCheckBox
        Left = 72
        Top = 247
        Width = 129
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 7
        OnClick = edtNameExit
      end
      object ckbEditorEnabled: TCheckBox
        Left = 208
        Top = 247
        Width = 121
        Height = 17
        Caption = 'Enabled'
        TabOrder = 8
        OnClick = edtNameExit
      end
      object edtDisplayFormat: TEdit
        Left = 72
        Top = 116
        Width = 317
        Height = 21
        TabOrder = 3
        OnExit = edtNameExit
      end
      object edtEditFormat: TEdit
        Left = 72
        Top = 140
        Width = 317
        Height = 21
        TabOrder = 4
        OnExit = edtNameExit
      end
      object edtFontSize: TEdit
        Left = 260
        Top = 288
        Width = 129
        Height = 21
        TabOrder = 9
        OnExit = edtNameExit
      end
      object ckbFontStyleB: TCheckBox
        Left = 72
        Top = 315
        Width = 65
        Height = 17
        Caption = 'Bold'
        TabOrder = 10
        OnClick = edtNameExit
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Operation logic'
      ImageIndex = 2
      object Label10: TLabel
        Left = 8
        Top = 44
        Width = 45
        Height = 13
        Caption = 'Res Type'
      end
      object Label11: TLabel
        Left = 8
        Top = 20
        Width = 37
        Height = 13
        Caption = 'URL link'
      end
      object Label2: TLabel
        Left = 8
        Top = 68
        Width = 38
        Height = 13
        Caption = 'Formula'
      end
      object Label12: TLabel
        Left = 8
        Top = 88
        Width = 53
        Height = 13
        Caption = 'Formula Ex'
      end
      object Label13: TLabel
        Left = 8
        Top = 140
        Width = 44
        Height = 13
        Caption = 'Summary'
      end
      object Label17: TLabel
        Left = 8
        Top = 164
        Width = 27
        Height = 13
        Caption = 'Script'
      end
      object edtResType: TEdit
        Left = 72
        Top = 40
        Width = 317
        Height = 21
        TabOrder = 1
        OnExit = edtNameExit
      end
      object edtURL: TEdit
        Left = 72
        Top = 16
        Width = 317
        Height = 21
        TabOrder = 0
        OnExit = edtNameExit
      end
      object edtFormula: TEdit
        Left = 72
        Top = 64
        Width = 317
        Height = 21
        TabOrder = 2
        OnExit = edtNameExit
      end
      object memoFormulaCondition: TMemo
        Left = 72
        Top = 88
        Width = 317
        Height = 45
        ScrollBars = ssVertical
        TabOrder = 3
        OnExit = edtNameExit
      end
      object combAggregateFun: TComboBox
        Left = 72
        Top = 136
        Width = 317
        Height = 21
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 4
        OnExit = edtNameExit
        Items.Strings = (
          'Sum'
          'Avg'
          'Max'
          'Min'
          'Count')
      end
      object memoValidateRule: TMemo
        Left = 72
        Top = 164
        Width = 317
        Height = 45
        ScrollBars = ssVertical
        TabOrder = 5
        OnExit = edtNameExit
      end
    end
  end
end
