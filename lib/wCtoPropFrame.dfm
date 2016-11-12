object FrameCtoProp: TFrameCtoProp
  Left = 0
  Top = 0
  Width = 497
  Height = 546
  TabOrder = 0
  TabStop = True
  object Panel_Props: TPanel
    Left = 12
    Top = 8
    Width = 357
    Height = 513
    BevelOuter = bvNone
    TabOrder = 0
    object BevelB: TBevel
      Left = 12
      Top = 300
      Width = 317
      Height = 5
      Shape = bsTopLine
    end
    object lbCreateDate: TLabel
      Left = 16
      Top = 164
      Width = 51
      Height = 13
      Caption = #21019#24314#26085#26399':'
      FocusControl = edtCreateDate
    end
    object lbCreator: TLabel
      Left = 16
      Top = 184
      Width = 39
      Height = 13
      Caption = #21019#24314#20154':'
      FocusControl = edtCreator
    end
    object BevelA: TBevel
      Left = 12
      Top = 148
      Width = 317
      Height = 5
      Shape = bsTopLine
    end
    object lbStatus: TLabel
      Left = 16
      Top = 424
      Width = 27
      Height = 13
      Caption = #29366#24577':'
    end
    object lbNodeType: TLabel
      Left = 16
      Top = 243
      Width = 27
      Height = 13
      Caption = #31867#22411':'
    end
    object lbPath: TLabel
      Left = 16
      Top = 126
      Width = 27
      Height = 13
      Caption = #36335#24452':'
      FocusControl = edtPath
    end
    object lbName: TLabel
      Left = 16
      Top = 14
      Width = 27
      Height = 13
      Caption = #21517#31216':'
      FocusControl = edtName
    end
    object lbModifyDate: TLabel
      Left = 16
      Top = 204
      Width = 51
      Height = 13
      Caption = #20462#25913#26085#26399':'
      FocusControl = edtModifyDate
    end
    object lbModifier: TLabel
      Left = 16
      Top = 224
      Width = 39
      Height = 13
      Caption = #20462#25913#20154':'
      FocusControl = edtModifier
    end
    object lbOrderNo: TLabel
      Left = 16
      Top = 446
      Width = 39
      Height = 13
      Caption = #25490#24207#21495':'
      FocusControl = edtOrderNo
    end
    object lbID: TLabel
      Left = 16
      Top = 106
      Width = 27
      Height = 13
      Caption = #32534#21495':'
      FocusControl = edtID
    end
    object lbMmeo: TLabel
      Left = 16
      Top = 317
      Width = 27
      Height = 13
      Caption = #22791#27880':'
    end
    object lbCaption: TLabel
      Left = 16
      Top = 73
      Width = 43
      Height = 13
      Caption = 'TipCode:'
      FocusControl = edtCaption
    end
    object Label1: TLabel
      Left = 84
      Top = 480
      Width = 24
      Height = 13
      Cursor = crHandPoint
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
      OnClick = Label1Click
    end
    object Label2: TLabel
      Left = 16
      Top = 42
      Width = 27
      Height = 13
      Caption = #26631#39064':'
    end
    object Label14: TLabel
      Left = 16
      Top = 271
      Width = 51
      Height = 13
      Caption = #36816#34892#24179#21488':'
    end
    object Label3: TLabel
      Left = 16
      Top = 369
      Width = 27
      Height = 13
      Caption = #33050#26412':'
    end
    object edtName: TEdit
      Left = 84
      Top = 13
      Width = 245
      Height = 21
      TabOrder = 0
      OnChange = edtNameChange
    end
    object edtCreateDate: TEdit
      Left = 84
      Top = 164
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 5
    end
    object edtCreator: TEdit
      Left = 84
      Top = 184
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
    end
    object edtPath: TEdit
      Left = 84
      Top = 126
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
    end
    object combTypeName: TComboBox
      Left = 84
      Top = 241
      Width = 245
      Height = 21
      ItemHeight = 13
      TabOrder = 9
      Text = #30446#24405
      OnChange = edtNameChange
      Items.Strings = (
        #20998#32452
        #30446#24405
        #39033#30446
        #35270#22270)
    end
    object combStatus: TComboBox
      Left = 84
      Top = 420
      Width = 245
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 13
      Text = #26410#30693
      OnChange = edtNameChange
      Items.Strings = (
        #26410#30693
        #27491#24120
        #35774#35745
        #35843#35797
        #21024#38500
        #33609#31295)
    end
    object edtModifyDate: TEdit
      Left = 84
      Top = 204
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 7
    end
    object edtModifier: TEdit
      Left = 84
      Top = 224
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 8
    end
    object edtOrderNo: TEdit
      Left = 84
      Top = 445
      Width = 245
      Height = 21
      TabOrder = 14
      OnChange = edtNameChange
    end
    object edtID: TEdit
      Left = 84
      Top = 106
      Width = 245
      Height = 16
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
    end
    object Memo_Describe: TMemo
      Left = 84
      Top = 311
      Width = 245
      Height = 46
      ScrollBars = ssVertical
      TabOrder = 11
      OnChange = edtNameChange
    end
    object edtCaption: TEdit
      Left = 84
      Top = 40
      Width = 245
      Height = 21
      TabOrder = 1
      OnChange = edtNameChange
    end
    object edtTipCode: TEdit
      Left = 84
      Top = 72
      Width = 245
      Height = 21
      TabOrder = 2
      OnChange = edtNameChange
    end
    object combRunner: TComboBox
      Left = 84
      Top = 268
      Width = 245
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 10
      Text = 'WEB'#21644'CS'#19979#22343#21487#36816#34892
      OnChange = edtNameChange
      Items.Strings = (
        'WEB'#21644'CS'#19979#22343#21487#36816#34892
        #21482#22312'CS'#19979#36816#34892
        #21482#22312'WEB'#19979#36816#34892)
    end
    object MemoXmlScript: TMemo
      Left = 84
      Top = 363
      Width = 245
      Height = 46
      ScrollBars = ssVertical
      TabOrder = 12
      OnChange = edtNameChange
    end
  end
end
