object FrameCtTableDef: TFrameCtTableDef
  Left = 0
  Top = 0
  Width = 629
  Height = 446
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object Splitter1: TSplitter
    Left = 173
    Top = 0
    Height = 446
  end
  object PanelCttbTree: TPanel
    Left = 0
    Top = 0
    Width = 173
    Height = 446
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'List'
    TabOrder = 0
  end
  object PanelTableProp: TPanel
    Left = 200
    Top = 4
    Width = 213
    Height = 137
    BevelOuter = bvNone
    Caption = 'Table properties'
    TabOrder = 1
  end
  object PanelFieldProp: TPanel
    Left = 256
    Top = 184
    Width = 245
    Height = 117
    BevelOuter = bvNone
    Caption = 'Field properties'
    TabOrder = 2
  end
  object PanelDMLGraph: TPanel
    Left = 372
    Top = 264
    Width = 213
    Height = 137
    BevelOuter = bvLowered
    Caption = 'Diagram'
    TabOrder = 3
    Visible = False
  end
end
