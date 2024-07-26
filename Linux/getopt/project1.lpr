program project1;

uses
  ctypes,getopt_core, getopt_ext;

var
  opt: cint;

  // https://man7.org/linux/man-pages/man3/getopt.3.html

begin
  repeat
    opt := getopt(argc, argv, 'n:t:help::');
    WriteLn('opt: ', opt, '   optarg: ',optarg);

  until opt = -1;

end.
