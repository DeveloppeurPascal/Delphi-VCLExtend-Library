object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 618
    Height = 25
    Align = alTop
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
    ExplicitLeft = 280
    ExplicitTop = 232
    ExplicitWidth = 75
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 618
    Height = 404
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitLeft = 232
    ExplicitTop = 200
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
