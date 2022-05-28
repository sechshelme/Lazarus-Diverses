unit OPTI;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TSpielOptionenBox = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SpielOptionenBox: TSpielOptionenBox;

implementation

{$R *.lfm}

uses
  Hauptfor;

procedure TSpielOptionenBox.BitBtn1Click(Sender: TObject);
begin
  with Optionen do begin
    with Hauptformular do begin
      ClientHeight := ClientHeight * TrackBar4.Position div maxY - 1;
      ClientWidth  := ClientWidth  * TrackBar3.Position div  maxX;
    end;
    maxX        := TrackBar3.Position;
    maxY        := TrackBar4.Position;
    StartLevel  := TrackBar1.Position;
    vorbau      := maxY * TrackBar2.Position div 100;
    Virus.Plus  := TrackBar5.Position;
    Virus.minus := TrackBar6.Position;
    Klotzart    := TrackBar7.Position;
  end;
end;

procedure TSpielOptionenBox.BitBtn3Click(Sender: TObject);
begin
  Application.MessageBox('Hier sollte ein Hilfetext stehen','Tetris',0);
end;

procedure TSpielOptionenBox.FormShow(Sender: TObject);
begin
  with Optionen do begin
    TrackBar1.Position := StartLevel;
    TrackBar2.Position := 100 * vorbau div maxY;
    TrackBar3.Position := maxX;
    TrackBar4.Position := maxY;
    TrackBar5.Position := Virus.plus;
    TrackBar6.Position := Virus.minus;
    TrackBar7.Position := Klotzart;
  end;
end;

procedure TSpielOptionenBox.BitBtn4Click(Sender: TObject);
begin
  TrackBar1.Position := 0;
  TrackBar2.Position := 0;
  TrackBar3.Position := 10;
  TrackBar4.Position := 20;
  TrackBar5.Position := 0;
  TrackBar6.Position := 0;
  TrackBar7.Position := 1;
end;

end.
