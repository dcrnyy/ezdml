object CtUIContainerFrame: TCtUIContainerFrame
  Left = 0
  Top = 0
  Width = 545
  Height = 413
  TabOrder = 0
  TabStop = True
  object cxTabControlSubUIs: TcxTabControl
    Left = 0
    Top = 388
    Width = 545
    Height = 25
    Align = alBottom
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    Options = [pcoCloseButton, pcoGoDialog, pcoGradient, pcoGradientClientArea, pcoRedrawOnResize]
    TabIndex = 0
    TabOrder = 0
    TabPosition = tpBottom
    Tabs.Strings = (
      #26631#31614#39029)
    OnCanClose = cxTabControlSubUIsCanClose
    OnChange = cxTabControlSubUIsChange
    OnMouseDown = cxTabControlSubUIsMouseDown
    OnMouseMove = cxTabControlSubUIsMouseMove
    ClientRectBottom = 1
    ClientRectRight = 545
    ClientRectTop = 0
  end
end
