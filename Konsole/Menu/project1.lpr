program project1;

uses
  Crt,
  Menu,
  MessageBox,
  Shares,
  Memo;

var
  MyMenu: TMenu;
  MyMenuPos: integer;

  Ende: set of byte = [255];

  procedure Background;
  begin
    TextAttr := $70;
    ClrScr;
    GotoXY(1, 25);
    GotoXY(1, 2);
    TextAttr := $24;
    Write(StringOfChar(#176, 80 * 23));
  end;

  procedure Hilfe;
  var
    Msg: TMessageBox;
  begin
    Msg := TMessageBox.Create;
    Msg.SetColor($27);

    Msg.Add('Spielanleitung');
    Msg.Add('==============');
    Msg.Add;
    Msg.Add('Mit Nummerblock den Cursor bewegen');
    Msg.Add('(7,8,9,4,6,1,2,3)');
    Msg.Add('Ziel des Spieles ist den Cursor von der oberen-');
    Msg.Add('linken Ecke in die rechte-untere Ecke zu bringen');
    Msg.Add('ohne in eine Mine zu fahren.');
    Msg.Add('Die roten Ziffern zeigen an mit wie vielen Minen,');
    Msg.Add('man Beruehrung hat.');
    Msg.Add('Viel Glueck');

    Msg.Run;
    Msg.Free;
  end;

  procedure Editor;
  var
    Memo: TMemo;
  begin
    Memo := TMemo.Create;
    Memo.Add('Test');
    Memo.Run;
    Memo.Free;
  end;

  procedure UnterMenu;
  var
    Menu: TMenu;
    MyUnterMenuPos: integer;
  begin
    Menu := TMenu.Create(20, 5);
    Menu.SetColor($8F, $4F, $80, $40);
    Menu.Add('&111');
    Menu.Add('&222');
    Menu.Add('&333');
    Menu.Add('&444');
    Menu.Add('&Ende');
    Ende := Ende + [Menu.GetItems];
    repeat
      MyUnterMenuPos := Menu.Run;
      case MyUnterMenuPos of
        0: begin
          // 111
        end;
        1: begin
          // 222
        end;
        2: begin
          // 333
        end;
        3: begin
          // 444
        end;
      end;
    until MyUnterMenuPos in Ende;
    Menu.Free;
  end;

begin
  //SetConsoleTitle('Mein Konsolenmenu');
  BackGround;
  MyMenu := TMenu.Create;
  MyMenu.SetColor($8F, $4F, $80, $40);
  MyMenu.Add('&Editor');
  MyMenu.Add('Unter &Menu');
  MyMenu.Add('&Level');
  MyMenu.Add('&Optionen');
  MyMenu.Add('&Hilfe');
  MyMenu.Add('Die ist ein sehr langes &Ende');
  Ende := Ende + [MyMenu.GetItems];
  repeat
    MyMenuPos := MyMenu.Run;
    case MyMenuPos of
      0: begin
        Editor;
        TextAttr := $07;
        Background;
      end;
      1: begin
        UnterMenu;
        TextAttr := $07;
        BackGround;
      end;
      4: begin
        Hilfe;
        TextAttr := $07;
        Background;
      end;
    end;
    GotoXY(60, 12);
    Write(MyMenupos, '----');
  until MyMenuPos in Ende;
  MyMenu.Free;
end.
