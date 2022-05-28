program CapterCUEConverter;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1},
  SkriptVorschau in 'SkriptVorschau.pas' {Form1},
  Fehlerausgabe in 'Fehlerausgabe.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSkriptForm, SkriptForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
