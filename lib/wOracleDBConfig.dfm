object frmOraDBConfig: TfrmOraDBConfig
  Left = 447
  Top = 474
  BorderStyle = bsDialog
  Caption = 'ORACLE Connection'
  ClientHeight = 199
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 48
    Top = 96
    Width = 66
    Height = 12
    Caption = 'Host IP:'
  end
  object Label2: TLabel
    Left = 240
    Top = 96
    Width = 30
    Height = 12
    Caption = 'Port:'
  end
  object Label3: TLabel
    Left = 48
    Top = 124
    Width = 54
    Height = 12
    Caption = 'SID:'
  end
  object edtIP: TEdit
    Left = 116
    Top = 92
    Width = 117
    Height = 20
    TabOrder = 3
  end
  object edtPort: TEdit
    Left = 272
    Top = 92
    Width = 49
    Height = 20
    TabOrder = 4
    Text = '1521'
  end
  object edtSvcName: TEdit
    Left = 116
    Top = 120
    Width = 205
    Height = 20
    TabOrder = 5
  end
  object btnOK: TButton
    Left = 92
    Top = 156
    Width = 87
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 188
    Top = 156
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object combNetSvcName: TComboBox
    Left = 48
    Top = 32
    Width = 273
    Height = 20
    ItemHeight = 12
    TabOrder = 1
    OnDropDown = combNetSvcNameDropDown
  end
  object rdbNetSvc: TRadioButton
    Left = 16
    Top = 12
    Width = 145
    Height = 17
    Caption = 'Through local service name:'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rdbNetSvcClick
  end
  object rdbSvcParam: TRadioButton
    Left = 16
    Top = 64
    Width = 149
    Height = 17
    Caption = 'Specify host parameters:'
    TabOrder = 2
    OnClick = rdbSvcParamClick
  end
end
