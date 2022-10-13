program project1; 

{$mode delphi}{$H+}

uses
  Interfaces, Forms, Unit1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWForm1, WForm1);
  Application.Run;
end.