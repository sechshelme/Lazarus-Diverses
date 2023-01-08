program Project1;

uses
    Unix;

procedure ExecAndEcho(const ACmd: string);
var
    t: Text;
    s: string;
begin
    POpen(t, ACmd, 'R');
    while not Eof(t) do
    begin
        Readln(t, s);
        Writeln(s);
    end;
    PClose(t);
end;

procedure GetCoreCount;
begin
    ExecAndEcho('nproc');
end;

procedure GetProcInfo;
begin
    ExecAndEcho('lscpu');
end;

procedure GetProcInfoEx;
begin
    ExecAndEcho('cat /proc/cpuinfo');
end;

function get_nprocs:longint; cdecl; external 'c' name 'get_nprocs';

begin
    Writeln('Core count from libc: ', get_nprocs);
    Writeln('Core count ============');
    GetCoreCount;
    Writeln('Proc info =============');
    GetProcInfo;
    Writeln('Proc info ex ==========');
    GetProcInfoEx;
end.
