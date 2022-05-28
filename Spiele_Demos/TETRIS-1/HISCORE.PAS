unit HISCORE;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons{, GradForm};

const
  maxNamen = 50;

type
  THiscoreBox = class(TForm)
    ListBox1: TListBox;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxschreiben;
    procedure Pruefen;
    procedure Erzeugen;
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;

  TName = record
    Name : String[20];
    Punkte : Integer;
    Linien : Integer;
  end;
  TNamen = array[1..maxNamen] of TName;

var
  HiscoreBox : THiscoreBox;
  Namen      : TNamen;
  f          : file of TNamen;

implementation

uses Hauptfor, EINGABE;

{$R *.lfm}

procedure THiscoreBox.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure THiscoreBox.FormCreate(Sender: TObject);
begin
  AssignFile(f, 'tetris.his');
  Reset(f);
  Read(f, Namen);
  CloseFile(f);
  ListBoxschreiben;
end;

procedure THiscoreBox.ListBoxschreiben;
var
  i , k     : Integer;
  s, s1, s2 : String[20];
begin
  for i := 1 to maxNamen do with Namen[i] do begin
    Str(Punkte:10, s1);
    Str(Linien:10, s2);
    s := '';
    for k := 1 to 20 - Length(Name) do s := s + ' ';
    ListBox1.Items.Add(Name + s + s1 + s2);
  end;
end;

procedure THiscoreBox.Erzeugen;
var
  i : Integer;
begin
  for i := 1 to maxNamen do with Namen[i] do begin
    ListBox1.Items.Delete(0);
    Name := ' << N o n a m e >>';
    Punkte := 0;
    Linien := 0;
  end;
  ListBoxschreiben;
end;

procedure THiscoreBox.Pruefen;
var
  i, k   : Integer;
begin
  i := 0;
  repeat
    Inc(i);
  until (i = maxNamen) or (Namen[i].Punkte < SpielStatus.Punkte);
  if i <> maxNamen then begin
    for k := maxNamen downto i + 1 do begin
      Namen[k] := Namen[k - 1];
    end;
    with Eingabebox, Namen[i] do begin
      Edit1.Text := '';
      repeat
        ShowModal;
      until Edit1.Text <> '';
      Name := Edit1.Text;
      Linien := SpielStatus.Linien;
      Punkte := SpielStatus.Punkte;
    end;
    for i := 1 to maxNamen do ListBox1.Items.Delete(0);
    ListBoxschreiben;
    Showmodal;
  end else begin
    Application.MessageBox('Leider verloren !', 'Tetris', mb_iconexclamation);
  end;
end;

procedure THiscoreBox.FormDestroy(Sender: TObject);
begin
  ReWrite(f);
  Write(f, Namen);
  CloseFile(f);
end;

end.
