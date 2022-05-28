unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Save_Combox_Box;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenButton1: TButton;
    ComboBox1: TComboBox;
    OpenButton2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure OpenButton2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadComboBox_from_XML(ComboBox1, ['/etc/fstab', '/avrdude/avrdude.conf']);
  LoadComboBox_from_XML(ComboBox2);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Caption := ComboBox1.Text;
  Label2.Caption := ComboBox2.Text;
end;

procedure TForm1.OpenButton1Click(Sender: TObject);
begin
  OpenDialog1.FileName := ComboBox1.Text;
  if OpenDialog1.Execute then begin
    ComboBox1.Text := OpenDialog1.FileName;
    ComboBox_Insert_Text(ComboBox1);
    SaveComboBox_to_XML(ComboBox1);
  end;
end;

procedure TForm1.OpenButton2Click(Sender: TObject);
begin
  OpenDialog1.FileName := ComboBox2.Text;
  if OpenDialog1.Execute then begin
    ComboBox2.Text := OpenDialog1.FileName;
    ComboBox_Insert_Text(ComboBox2);
    SaveComboBox_to_XML(ComboBox2);
  end;
end;

end.

