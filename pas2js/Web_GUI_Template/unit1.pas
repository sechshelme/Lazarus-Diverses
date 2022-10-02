unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    WButton1: TWButton;
    procedure WButton1Click(Sender: TObject);
  private

  public

  end;

var
  WForm1: TWForm1;

implementation

{$R *.lfm}

{ TWForm1 }

procedure TWForm1.WButton1Click(Sender: TObject);
begin

end;

end.

