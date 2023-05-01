program project1;

uses
  process;

  procedure Run;
  const
    READ_BYTES = 6;
//    READ_BYTES = 65536;
  var
    myProcess: TProcess;
    n: longint;
    total: longint = 0;
    totalStr: string = '';
    buffer: array[0..READ_BYTES - 1] of char;
  begin
    myProcess := TProcess.Create(nil);

    //    myProcess.CommandLine := '/usr/bin/ls /home/tux --color';
//    myProcess.CommandLine := '/usr/bin/ls /usr/include -R --color';
//myProcess.CommandLine := '/usr/bin/ls /usr/local -R';
myProcess.CommandLine := '/usr/bin/ls /usr/local/share/wayland-protocols/unstable';



    myProcess.Options := [poUsePipes];
    myProcess.Execute;

    while myProcess.Running do begin
      FillChar(buffer,READ_BYTES,#0);
      n := myProcess.Output.Read(buffer, Length(buffer));
      Inc(total, n);
      WriteStr(totalStr, totalStr, ' ', n);

      if n < READ_BYTES then begin
  //      buffer[n] := #0;
      end;

//      WriteLn(#10,n);
      Write(buffer);
    end;

    repeat;
      FillChar(buffer,READ_BYTES,#0);
      n := myProcess.Output.Read(buffer, Length(buffer));
      Inc(total, n);
      WriteStr(totalStr, totalStr, ' ', n);

      if n < READ_BYTES then begin
//        buffer[n] := #0;
      end;
//      WriteLn(#10,n);
      Write(buffer);
    until n <= 0;


    myProcess.Free;
    WriteLn(total);
//    WriteLn(totalStr);
  end;

begin
  Run;
end.
