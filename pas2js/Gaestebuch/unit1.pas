unit Unit1;

{$mode objfpc}{$H+}

interface

uses
 browserconsole, Web, JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    ButtonSend: TWButton;
    WLabel1: TWLabel;
    WMemo1: TWMemo;
    procedure ButtonSendClick(Sender: TObject);
  private

  public

  end;

var
  WForm1: TWForm1;

implementation

{$R *.lfm}

{ TWForm1 }

procedure TWForm1.ButtonSendClick(Sender: TObject);                    var
  xhr : TJSXMLHttpRequest;
begin
  WLabel1.Caption:=WMemo1.Lines.Text;
  xhr.;
  WMemo1.Lines.;

end;

end.

