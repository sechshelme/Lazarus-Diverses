unit SkriptVorschau;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, RichMemo;

type

  { TSkriptForm }

  TSkriptForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    RichMemo1: TRichMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SkriptForm: TSkriptForm;

implementation

{$R *.lfm}

end.

