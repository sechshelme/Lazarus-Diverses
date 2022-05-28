unit Tools_Info;

interface

uses
  LCLVersion;

var
  FPC_Info:record
    Lazarus, FPC, OS, CPU: String
  end;


implementation

begin
  with FPC_Info do begin
    Lazarus:=  lcl_version;
    FPC:= {$I %FPCVERSION%};
    OS:= {$I %FPCTARGETOS%};
    CPU:= {$I %FPCTARGETCPU%};
  end;
end.

