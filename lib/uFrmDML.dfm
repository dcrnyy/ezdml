object frmDML: TfrmDML
  Left = 214
  Top = 166
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  BorderWidth = 1
  Caption = 'DML Diagram'
  ClientHeight = 538
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  inline FrameDML1: TFrameDML
    Left = 0
    Top = 0
    Width = 728
    Height = 538
    Align = alClient
    TabOrder = 0
    TabStop = True
    inherited splProperty: TSplitter
      Left = 526
      Height = 493
    end
    inherited StatusBar1: TStatusBar
      Top = 519
      Width = 728
    end
    inherited ToolBar1: TToolBar
      Width = 728
    end
    inherited pnlProperty: TPanel
      Left = 528
      Width = 200
      Height = 493
      inherited vleProperty: TValueListEditor
        Width = 200
        Height = 493
        ColWidths = (
          100
          98)
      end
    end
  end
  object Panel1: TPanel
    Left = 600
    Top = 0
    Width = 133
    Height = 22
    BevelOuter = bvNone
    TabOrder = 1
    object sbtnSave: TSpeedButton
      Left = 4
      Top = 0
      Width = 77
      Height = 22
      Caption = 'Save and close'
      Flat = True
      OnClick = sbtnSaveClick
    end
    object sbtnCancel: TSpeedButton
      Left = 80
      Top = 0
      Width = 45
      Height = 22
      Caption = 'Cancel'
      Flat = True
      OnClick = sbtnCancelClick
    end
  end
end
