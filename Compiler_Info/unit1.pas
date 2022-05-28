unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  WriteLn('Home:                         ' + {$I %HOME%});
  WriteLn('Compilierzeit:                ' + {$I %TIME%});
  WriteLn('Compilierdatum:               ' + {$I %DATE%});
  WriteLn('Erforderliche CPU:            ' + {$I %FPCTARGETCPU%});
  WriteLn('Erforderliche CPU (veraltet): ' + {$I %FPCTARGET%});
  WriteLn('Betriebssystem:               ' + {$I %FPCTARGETOS%});
  WriteLn('FPC Version:                  ' + {$I %FPCVERSION%});
  WriteLn('Name der Datei:               ' + {$I %FILE%});
  WriteLn('Name der Datei:               ' + {$I %FILnENAME%});
  WriteLn('Aktuelle Zeile als String:    ' + {$I %LINE%});
  WriteLn('Aktuelle Zeile als Int:       ' + IntToStr({$I %LINENUM%}));
  WriteLn('Compilierer:                  ' + {$I %USER%});
  WriteLn('Name der aktuellen Routine:   ' + {$I %CURRENTROUTINE%});
end;

begin
end.



