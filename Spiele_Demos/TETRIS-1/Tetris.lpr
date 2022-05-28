program Tetris;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Hauptfor in 'Hauptfor.pas' {HauptFormular},
  About in 'About.pas' {AboutBox},
  Next in 'Next.pas' {NextBox},
  statisti in 'statisti.pas' {StatistikBox},
  hiscore in 'hiscore.pas' {HiscoreBox},
  eingabe in 'eingabe.pas' {EingabeBox},
  hintergr in 'hintergr.pas' {Darstellungsbox},
  opti in 'opti.pas' {SpielOptionenBox}  ;

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'tetris.hlp';
  Application.CreateForm(THauptFormular, HauptFormular);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TNextBox, NextBox);
  Application.CreateForm(TStatistikBox, StatistikBox);
  Application.CreateForm(THiscoreBox, HiscoreBox);
  Application.CreateForm(TEingabeBox, EingabeBox);
  Application.CreateForm(TDarstellungsbox, Darstellungsbox);
  Application.CreateForm(TSpielOptionenBox, SpielOptionenBox);
  Application.Run;
end.
