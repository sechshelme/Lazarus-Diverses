unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BaseUnix;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    ToggleBox1: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HID_View(device: string);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  BUS_PCI = $01;
  BUS_ISAPNP = $02;
  BUS_USB = $03;
  BUS_HIL = $04;
  BUS_BLUETOOTH = $05;
  BUS_VIRTUAL = $06;

  HID_MAX_DESCRIPTOR_SIZE = 4096;

  _IOC_READ = 2;
  _IOC_WRITE = 1;

  _IOC_NRSHIFT = 0;
  _IOC_NRBITS = 8;
  _IOC_TYPEBITS = 8;
  _IOC_SIZEBITS = 14;

type
  hidraw_report_descriptor = record
    size: UInt32;
    Value: array[0..HID_MAX_DESCRIPTOR_SIZE - 1] of UInt8;
  end;

  hidraw_devinfo = record
    bustype: UInt32;
    vendor: Int16;
    product: Int16;
  end;


function _IOC_TYPESHIFT: integer; inline;
begin
  Result := _IOC_NRSHIFT + _IOC_NRBITS;
end;

function _IOC_SIZESHIFT: integer; inline;
begin
  Result := _IOC_TYPESHIFT + _IOC_TYPEBITS;
end;

function _IOC_DIRSHIFT: integer; inline;
begin
  Result := _IOC_SIZESHIFT + _IOC_SIZEBITS;
end;


function _IOC(dir, typ, nr, size: integer): integer; inline;
begin
  Result := (dir shl _IOC_DIRSHIFT) or (typ shl _IOC_TYPESHIFT) or
    (nr shl _IOC_NRSHIFT) or (size shl _IOC_SIZESHIFT);
end;

function _IOR(typ, Nr, size: integer): integer; inline;
begin
  Result := _IOC(_IOC_READ, typ, Nr, size);
end;

function HIDIOCGRDESCSIZE: integer; inline;
begin
  Result := _IOR(byte('H'), $01, SizeOf(integer));
end;

function HIDIOCGRDESC: integer; inline;
begin
  Result := _IOR(byte('H'), $02, SizeOf(hidraw_report_descriptor));
end;

function HIDIOCGRAWINFO: integer; inline;
begin
  Result := _IOR(byte('H'), $03, SizeOf(hidraw_devinfo));
end;

function HIDIOCGRAWNAME(len: integer): integer; inline;
begin
  Result := _IOC(_IOC_READ, byte('H'), $04, len);
end;

function HIDIOCGRAWPHYS(len: integer): integer; inline;
begin
  Result := _IOC(_IOC_READ, byte('H'), $05, len);
end;


function HIDIOCSFEATURE(len: integer): integer; inline;
begin
  Result := _IOC(_IOC_WRITE or _IOC_READ, byte('H'), $06, len);
end;

function HIDIOCGFEATURE(len: integer): integer; inline;
begin
  Result := _IOC(_IOC_WRITE or _IOC_READ, byte('H'), $07, len);
end;

function bus_str(bus: integer): string;
begin
  case bus of
    BUS_USB:
    begin
      Result := 'USB';
    end;
    BUS_HIL:
    begin
      Result := 'HIL';
    end;
    BUS_BLUETOOTH:
    begin
      Result := 'Bluetooth';
    end;
    BUS_VIRTUAL:
    begin
      Result := 'Virtual';
    end;
    else
    begin
      Result := 'Other';
    end;
  end;
end;

procedure TForm1.HID_View(device: string);
var
  fd, i, res, desc_size: integer;
  buf: array[0..255] of char;
  rpt_dec: hidraw_report_descriptor;
  info: hidraw_devinfo;

  s: string;

begin
  (* Open the Device with non-blocking reads. In real life,
     don't use a hard coded path; use libudev instead. *)
  fd := FpOpen(device, O_RDWR or O_NONBLOCK);       // Device-Name
  if fd < 0 then
  begin
    Memo1.Lines.Add('Unable to open device');
  end;
  FillChar(rpt_dec, SizeOf(rpt_dec), $00);
  FillChar(info, SizeOf(info), $00);
  FillChar(buf, SizeOf(buf), $00);

  (* Get Report Descriptor Size *)
  res := FpIOCtl(fd, HIDIOCGRDESCSIZE, @desc_size);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGRDESCSIZE');
  end
  else
  begin
    Memo1.Lines.Add('Report Descriptor Size: ' + IntToStr(desc_size));
  end;

  (* Get Report Descriptor *)
  rpt_dec.size := desc_size;
  res := FpIOCtl(fd, HIDIOCGRDESC, @rpt_dec);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGRDESC');
  end
  else
  begin
    Memo1.Lines.Add('Report Descriptor: ');
    s := '';
    for i := 0 to rpt_dec.size - 1 do
    begin
      s := s + IntToHex(rpt_dec.Value[i], 2) + ' ';
    end;
    Memo1.Lines.Add(s);
    Memo1.Lines.Add('');
  end;

  (* Get Raw Name *)
  res := FpIOCtl(fd, HIDIOCGRAWNAME(255), @buf);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGRAWNAME');
  end
  else
  begin
    Memo1.Lines.Add('Raw Name: ' + buf);
  end;

  (* Get Physical Location *)
  res := FpIOCtl(fd, HIDIOCGRAWPHYS(255), @buf);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGRAWPHYS');
  end
  else
  begin
    Memo1.Lines.Add('Raw Phys: ' + buf);
  end;

  (* Get Raw Info *)
  res := FpIOCtl(fd, HIDIOCGRAWINFO, @info);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGRAWINFO');
  end
  else
  begin
    Memo1.Lines.Add('Raw Info: ');
    Memo1.Lines.Add('    Tbustype: ' + IntToStr(info.bustype) + ' (' +
      bus_str(info.bustype) + ')');
    Memo1.Lines.Add('    Tvendor: ' + IntToHex(info.vendor, 4));
    Memo1.Lines.Add('    Tproduct: ' + IntToHex(info.product, 4));
    Memo1.Lines.Add('');
  end;

  (* Set Feature *)
  buf[0] := char($09);  // Report Number
  buf[1] := char($ff);
  buf[2] := char($ff);
  buf[3] := char($ff);
  res := FpIOCtl(fd, HIDIOCSFEATURE(4), @buf);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCSFEATURE');
  end
  else
  begin
    Memo1.Lines.Add('ioctl HIDIOCSFEATURE returned: ' + IntToStr(res));
  end;

  (* Get Feature *)

  buf[0] := char($09);  // Report Number
  res := FpIOCtl(fd, HIDIOCGFEATURE(256), @buf);
  if res < 0 then
  begin
    Memo1.Lines.Add('HIDIOCGFEATURE');
  end
  else
  begin
    Memo1.Lines.Add('ioctl HIDIOCGFEATURE returned: ' + IntToStr(res));
    Memo1.Lines.Add('Report data (not containing the report number): ' + IntToStr(res));
    s := '';
    for i := 0 to res - 1 do
    begin
      s := s + IntToHex(byte(buf[i]), 2) + ' ';
    end;
    Memo1.Lines.Add(s);
    Memo1.Lines.Add('');
  end;

  (* Send a Report to the Device *)
  buf[0] := char($01);  // Report Number
  buf[1] := char($77);
  res := FpWrite(fd, buf, 2);
  if res < 0 then
  begin
    Memo1.Lines.Add('Error: ' + IntToStr(errno));
    Memo1.Lines.Add('write');
  end
  else
  begin
    Memo1.Lines.Add('write() wrote ' + IntToStr(res) + ' bytes');
  end;

  (* Get a report from the device *)
  res := FpRead(fd, buf, 16);
  if res < 0 then
  begin
    Memo1.Lines.Add('read');
  end
  else
  begin
    s := '';
    Memo1.Lines.Add('read: ' + IntToStr(res));
    for i := 0 to res - 1 do
    begin
      s := s + IntToHex(byte(buf[i]), 2) + ' ';
    end;
    Memo1.Lines.Add(s);
    Memo1.Lines.Add('');
  end;
  FpClose(fd);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  DeviceList: TStringList;
  i: integer;
begin
  Memo1.Clear;
  DeviceList := FindAllFiles('/dev', 'hidraw*', False);
  //for i := 0 to DeviceList.Count - 1 do
  //begin
  //  Memo1.Lines.Add('Device: ' + DeviceList[i]);
  //  Memo1.Lines.Add('');
  //  HID_View(DeviceList[i]);
  //  Memo1.Lines.Add('');
  //  Memo1.Lines.Add('----------------------------------------------------------------------');
  //  Memo1.Lines.Add('');
  //end;
    HID_View(DeviceList[0]);
  DeviceList.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  USB_CMD_SETGPIO = $50;
  USB_CMD_GETGPIO = $51;
  report_size = 64;     // 64

var
  fd: cint;
  report: array [0..report_size - 1] of byte;

begin
  fd := FpOpen('/dev/hidraw3', O_RDWR or O_NONBLOCK);       // Device-Name
  if fd < 0 then
  begin
    Memo1.Lines.Add('Unable to open device');
  end;

  FillChar(report, report_size, $00);

  report[0] := $60;
  //    report[0] := USB_CMD_SETGPIO;

  report[7] := %11000000;
  if ToggleBox1.Checked then
  begin
    report[8] := %00010000;
  end;
  if ToggleBox2.Checked then
  begin
    report[9] := %00010000;
  end;
  if ToggleBox3.Checked then
  begin
    report[10] := %00010000;
  end;
  if ToggleBox4.Checked then
  begin
    report[11] := %00010000;
  end;

  FpWrite(fd, report, report_size);

  FpClose(fd);
end;

// ================================================================

procedure TForm1.Button3Click(Sender: TObject);
const
  USB_CMD_SETGPIO = $50;
  USB_CMD_GETGPIO = $51;
  report_size = 64;     // 64

var
  fd: cint;
  report: array [0..report_size - 1] of byte;
  s: string;
  i: integer;

begin
  fd := FpOpen('/dev/hidraw3', O_RDWR or O_NONBLOCK);       // Device-Name
  if fd < 0 then
  begin
    Memo1.Lines.Add('Unable to open device');
  end;

  FillChar(report, report_size, $00);

  report[0] := USB_CMD_SETGPIO;
  report[1] := $ff;

  report[2] := $01;
  report[3] := byte(ToggleBox1.Checked);
  report[4] := $00;
  report[5] := $00;

  report[6] := $01;
  report[7] := byte(ToggleBox2.Checked);
  report[8] := $00;
  report[9] := $00;

  report[10] := $01;
  report[11] := byte(ToggleBox3.Checked);
  report[12] := $00;
  report[13] := $00;

  report[14] := $01;
  report[15] := byte(ToggleBox4.Checked);
  report[16] := $00;
  report[17] := $00;

  FpWrite(fd, report, report_size);

  FpClose(fd);
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  report_size = 64;     // 64
  hello = '1234567890';

var
  fd: cint;
  report: array [0..report_size - 1] of byte;
  s: string;
  i: integer;

begin
  fd := FpOpen('/dev/hidraw3', O_RDWR or O_NONBLOCK);       // Device-Name
  if fd < 0 then
  begin
    Memo1.Lines.Add('Unable to open device');
  end;

  FillChar(report, report_size, $00);

  report[0] := $B0;
  report[1] := $00;
  report[2] := Length(hello) + 2;
  report[3] := $03;


  for i := 0 to Length(hello) - 1 do
  begin
    report[i * 2 + 4] := byte(hello[i]);
    report[i * 2 + 5] := 0;
  end;
  FpWrite(fd, report, report_size);

  FpClose(fd);
end;

procedure TForm1.Button5Click(Sender: TObject);
const
  report_size = 64;     // 64
  hello = '1234567890';

var
  fd: cint;
  report: array [0..report_size - 1] of byte;
  s: string;
  i, j: integer;

begin
  fd := FpOpen('/dev/hidraw3', O_RDWR or O_NONBLOCK);       // Device-Name
  if fd < 0 then
  begin
    Memo1.Lines.Add('Unable to open device');
  end;

  FillChar(report, report_size, $00);
  for j := 0 to 1000 do begin
    for i := 0 to report_size - 1 do
    begin
      report[i] := Random($100);
    end;
    report[0] := $60;
    FpWrite(fd, report, report_size);
  end;


  FpClose(fd);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

end.
