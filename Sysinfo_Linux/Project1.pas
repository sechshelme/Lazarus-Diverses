program Project1;
uses
  Linux;

var
	info: TSysInfo;
begin
	if sysInfo(@info) <> 0 then
	begin
		halt(1);
	end;

	writeLn('uptime: ', info.uptime, ' seconds');

	with info do
	begin
		writeLn('total free: ', freeram, ' bytes');
	end;
end.
