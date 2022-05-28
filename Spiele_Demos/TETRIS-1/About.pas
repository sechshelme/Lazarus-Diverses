unit About;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons{, GradForm};

type
  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

end.
