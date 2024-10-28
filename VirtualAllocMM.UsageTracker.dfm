object VAMMUsageTracker: TVAMMUsageTracker
  Left = 0
  Top = 0
  Caption = 'VAMMUsageTracker'
  ClientHeight = 726
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object mVAMMStats: TMemo
    Left = 0
    Top = 25
    Width = 512
    Height = 660
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 25
    Align = alTop
    Caption = 'VAMM (Virtual Alloc Memory Manager) Statistics'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 685
    Width = 512
    Height = 41
    Align = alBottom
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 2
    object btnUpdate: TButton
      Left = 279
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Update'
      TabOrder = 0
      OnClick = btnUpdateClick
    end
    object cbAutoUpdate: TCheckBox
      Left = 192
      Top = 10
      Width = 81
      Height = 17
      Caption = 'AutoUpdate'
      TabOrder = 1
      OnClick = cbAutoUpdateClick
    end
  end
  object timAutoUpdater: TTimer
    Enabled = False
    OnTimer = timAutoUpdaterTimer
    Left = 24
    Top = 37
  end
end
