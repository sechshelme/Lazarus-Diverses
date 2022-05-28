unit eingabe;

interface

uses
   Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TEingabeBox = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  EingabeBox: TEingabeBox;

implementation

{$R *.lfm}

end.
