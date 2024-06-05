unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LMessages, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure MouseHook(Sender: TObject; Msg: cardinal);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.AddOnUserInputHandler(@MouseHook);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TForm1.MouseHook(Sender: TObject; Msg: cardinal);
begin
  case msg of
    512: begin
      WriteLn('Mouse move');
    end;
    513: begin
      WriteLn('ML down');
    end;
    514: begin
      WriteLn('ML up');
    end;
    519: begin
      WriteLn('MM down');
    end;
    520: begin
      WriteLn('MM up');
    end;
    516: begin
      WriteLn('MR down');
    end;
    517: begin
      WriteLn('MR up');
    end;
    256: begin
      WriteLn('Key down');
    end;
    257: begin
      WriteLn('Key up');
    end;
    48384: begin    // Esc
      Close;
    end;
    else begin
      WriteLn('Custom: ', msg);
    end;
  end;
end;

end.
