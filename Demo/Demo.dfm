object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 133
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 224
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Show Usage Tracker'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Allocate Bytes'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 47
    Width = 113
    Height = 25
    Caption = 'Allocate KBytes'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 89
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Overflow'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Underflow'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 144
    Top = 47
    Width = 145
    Height = 25
    Caption = 'Free a Allocation'
    TabOrder = 5
    OnClick = Button6Click
  end
end
