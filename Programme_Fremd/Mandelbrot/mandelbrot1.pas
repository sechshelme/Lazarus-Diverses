unit Mandelbrot1;

{$mode objfpc}{$H+}


// idea from https://www.streetinfo.lu/computing/lazarus/doc/mandelbrot.html (Turbo)

interface

uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	 ComCtrls, LResources, LCLType, Arrow, BGRAbitmap, BGRABitmapTypes,
   utf8process;

type

	 { TForm1 }

   TForm1 = class(TForm)
      Arrow1: TArrow;
      Button1: TButton;
      Button2: TButton;
      Button4: TButton;
      Button5: TButton;
      Button6: TButton;
      GroupBox1: TGroupBox;
      GroupBox2: TGroupBox;
      Image1: TImage;
      Label1: TLabel;
      Label10: TLabel;
      Label2: TLabel;
      Label3: TLabel;
      Label4: TLabel;
      Label5: TLabel;
      Label6: TLabel;
      Label7: TLabel;
      Label8: TLabel;
      Label9: TLabel;
      Panel1: TPanel;
      RadioGroup1: TRadioGroup;
      Timer1: TTimer;
      ToggleBox1: TToggleBox;
      TrackBar1: TTrackBar;
      TrackBar2: TTrackBar;
      UpDown1: TUpDown;
      UpDown2: TUpDown;
      procedure Button1Click(Sender: TObject);
      procedure Button2Click(Sender: TObject);
      procedure Button4Click(Sender: TObject);
      procedure Button5Click(Sender: TObject);
      procedure Button6Click(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure InitMandel;
      procedure MandelThread;
      procedure MandelLinear;
      procedure Loop(UseThreads: Boolean);
      procedure FormCreate(Sender: TObject);
      procedure RadioGroup1SelectionChanged(Sender: TObject);
      procedure Timer1Timer(Sender: TObject);
      procedure ToggleBox1Change(Sender: TObject);
      procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
		      NewValue: SmallInt; Direction: TUpDownDirection);
      procedure UpDown2ChangingEx(Sender: TObject; var AllowChange: Boolean;
			NewValue: SmallInt; Direction: TUpDownDirection);
   private
   TimerTick : TDateTime;
   public

   end;


  TMandelThread = class(TThread)
     rys: integer;
     RXoff, RxyZoom: Double;
    protected
      procedure Execute; override;
    public
      Constructor Create(ParaYS :Integer; ParaXoff, ParaxyZoom: Double);
    end;


var
   Form1: TForm1;

implementation
{$R *.lfm}

{$I Turbo256colors.inc}

const
  XMax   = 800;
  YMax   = 600;
  YMid = YMax div 2;
  NIters = 256;
  VgaColors=256;
var
   Xoff, Zoom, XYZoom: Double;
   percent : Double;
   VGAcolor: Integer;
   bmp, TheEnd :TBGRAbitmap;
   cpu : integer;   // number of CPUs
   StopFlag : Boolean = false; // pause
   PercentArray : array of Double; // one entry for every thread
   {$ifdef Windows}
   R : TRect;
   {$endif}

procedure IncF (Var F : double; Increment : double = 1); inline;
begin
f := f + Increment;
end;

// insert space at every 'block' - 2 for hex, 3 for decimals
function goodLooking (s: string; blocklen: integer) : string; inline;
var i,k : integer;
     begin
       result := ''; k := 1;
       for i := length(s) downto 1 do
       begin
	        result := s[i]+result;
	        if (k mod Blocklen = 0) and (i>1) then result := #32+ result;
	        inc (k);
		   end;
		end;


Constructor TMandelThread.Create(ParaYS: Integer; ParaXoff, ParaxyZoom: Double);
  begin
    FreeOnTerminate := false;
    rys := ParaYS;
    RXoff := ParaXoff;
    RxyZoom  := ParaxyZoom;
    inherited Create(false);
  end;

procedure TMandelThread.Execute;
var rX0,rY0, rT,xx,yy : Double;
    it,rXs,idx : integer;
    pix : TBGRAPixel;
    per : Integer = 0;
    q1,q2 : PBGRAPixel;
begin
idx := rYs;
while rYs < YMax div 2 do
begin
q1 := bmp.data+rYs*Xmax+bmp.NbPixels div 2;
q2 := bmp.data-(rYs+1)*Xmax+bmp.NbPixels div 2;
rY0 := rYS / rXYZoom;
 for rXS := 0 to XMax - 1 do
  begin
	 rX0 := rxs / rXYZoom + rXoff;
	 Xx := rX0; Yy := rY0; It := 0;
   repeat
		  rT := XX;
		  XX := Sqr(XX) - Sqr(YY) + rX0;
	    Yy := yY *2 * rT   + rY0;
		  Inc(It);
	 until (Sqr(xX) + Sqr(yY) > 4) or (It = NIters);
   inc(per,It);
	 VGAcolor := It mod VgaColors;
	 pix := TurboVGA[VGAcolor];
    q1^ :=  pix; inc(q1);
    q2^ := pix; inc(q2);
  end; // x
 inc (rYs, CPU);
end; // while   y
incF(PercentArray[idx],per);
end;


procedure TForm1.InitMandel;
begin
Xoff := TrackBar2.Position / 1000;
Zoom := TrackBar1.Position;
XYZoom := (YMax / 3) * Zoom;
end;



procedure TForm1.FormDestroy(Sender: TObject);
begin
   if Assigned(bmp) then bmp.free;
   if Assigned(TheEnd) then TheEnd.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
 const fmandel= 'Mandel';
 var fname : string;
     i: Integer = 0;
begin
   if (ssCtrl in Shift) and (Key=  VK_S) then
         begin
         repeat
         inc(i);
         fname := fmandel+IntToStr(i)+'.png';
				 until not fileExists(fname);
				 image1.Picture.Bitmap.SaveToFile(fname);
         showMessage (fname+' saved');
				 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const LE = LineEnding;

begin
   cpu := GetSystemThreadCount;
   Label7.Caption := IntToStr(CPU)+' CPUs';
   Button4.Caption := 'E'+LE+'X'+LE+'I'+LE+'T';
   InitMandel;
   bmp := TBGRABitmap.create (Image1.Width,Image1.Height);
   TheEnd := TBGRAbitmap.create;
   TheEnd.LoadFromResource('TheEnd');
   {$ifdef Windows}
    R := Rect (0,0,image1.width,image1.height);
   {$endif}
end;


// 0..2 from https://www.streetinfo.lu/computing/lazarus/doc/mandelbrot_pics.html
procedure TForm1.RadioGroup1SelectionChanged(Sender: TObject);
var startTime: QWord;
begin
   case RadioGroup1.ItemIndex of
   0: begin TrackBar1.Position := 60;
            TrackBar2.Position := -1800;
      end;
   1: begin TrackBar1.Position := 150;
            TrackBar2.Position := -1425;
      end;
   2:begin TrackBar1.Position := 750;
           TrackBar2.Position := -1420;

      end;
   3: begin
      Xoff := -1.795; zoom:=3.19;
      end;
   4: Begin
        Xoff := -1.40599998; zoom :=  1732;
      end;
   5: Begin
        Xoff := -1.4059977; zoom :=  225886949;
      end;
   6: Begin
       {$ifdef Windows}
       Image1.Canvas.CopyRect(R,TheEnd.canvas,R);
       {$else}
        TheEnd.draw(Image1.Canvas,0,0,true);
       {$endif}
      end;
    end;// case
 if RadioGroup1.ItemIndex in [0..2] then
   begin
   Button5Click(Nil)
   end else
     if RadioGroup1.ItemIndex in [3..5] then
     begin
     XYZoom := (YMax / 3) * Zoom;
     startTime := GetTickCount64;
     MandelLinear;
     Label4.Caption := IntToStr(GetTickCount64 - StartTime)+' ms';
     end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Label3.Caption := TimeToStr(TimerTick);
   incF(TimerTick,  1/SecsPerDay);
end;

procedure TForm1.ToggleBox1Change(Sender: TObject);
begin
   If ToggleBox1.checked then
      begin
      ToggleBox1.Caption := 'Continue';
      StopFlag := true;
			end else
      begin
      ToggleBox1.Caption := 'Pause';
      StopFlag := false;
      end;
end;

procedure TForm1.UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
	 NewValue: SmallInt; Direction: TUpDownDirection);
begin
   // TUpDownDirection = (updNone, updUp, updDown);
   if Direction = updUp then  TrackBar1.Position:= TrackBar1.Position+1 else
     if Direction = updDown then TrackBar1.Position:= TrackBar1.Position-1

end;

procedure TForm1.UpDown2ChangingEx(Sender: TObject; var AllowChange: Boolean;
	 NewValue: SmallInt; Direction: TUpDownDirection);
begin
   if Direction = updUp then  TrackBar2.Position:= TrackBar2.Position+1 else
     if Direction = updDown then TrackBar2.Position:= TrackBar2.Position-1

end;


procedure MandelKernel(RXoff, RxyZoom: Double); inline;
var rX0,rY0 :Double;
    it,rXs,rys  : integer;
    pix : TBGRAPixel;
    rT,xx,yy : Double;
    p1,p2 : PBGRAPixel;
begin
percent := 0.0;
p1 := bmp.data +bmp.NbPixels div 2;// drawing lower half
p2 := P1 - XMax;  // one line up
 for rYS := 0 to YMid - 1 do begin
     rY0 := rYS / rXYZoom;
		   for rXS := 0 to XMax - 1 do begin
			   rX0 := rxs / rXYZoom + rXoff;
			   Xx := rX0;
         Yy := rY0;
         It := 0;
			      repeat
				       rT := XX;
               XX := Sqr(XX) - Sqr(YY) + rX0;
				       Yy := YY* 2 * rT + rY0;
					     Inc(It);
						until (Sqr(xX) + Sqr(yY) > 4) or (It = NIters );
            incF(percent,It);
			   VGAcolor := It mod VgaColors;
			   pix := TurboVGA[VGAcolor];
         p1^ := pix;
         P2^ := pix;
         inc(p1); inc(P2);
		    end; // rXs
        p2 := p2 - 2*XMax; // one line up and go to first pix of line
  end; // rYs
  bmp.InvalidateBitmap;
  percent := percent/NIters/XMax/YMax*200;
end;

procedure TForm1.MandelLinear;
begin
MandelKernel(Xoff,XYZoom );
 {$ifdef Windows}
 Image1.Canvas.CopyRect(R,bmp.canvas,R);
 {$else}
 bmp.draw(Image1.Canvas,0,0,true);
 {$endif}
end;

procedure TForm1.MandelThread;
var  threadArray: array  of TMandelThread;
     k : integer;

begin
   setLength(threadArray,cpu);
   setLength(PercentArray, cpu);
   fillchar (PercentArray[0],sizeof(double)*Length(PercentArray),0);
   for k := 0 to CPU-1 do threadArray[k] := TMandelThread.create (k,Xoff,XYZoom);
   for k := 0 to CPU-1 do threadArray[k].WaitFor;
   for k := 0 to CPU-1 do threadArray[k].free;
   percent := 0;
   for k := 0 to high(PercentArray) do incF(Percent,PercentArray[k]);
   percent := percent/nIters/XMax/YMax*200;
   bmp.InvalidateBitmap;
   {$ifdef Windows}
   Image1.Canvas.CopyRect(R,bmp.canvas,R);
   {$else}
   bmp.draw(Image1.Canvas,0,0,true);
   {$endif}
end;


procedure TForm1.Loop (UseThreads: Boolean);
var i : Integer;
    startTime: QWord;

begin
GroupBox1.Enabled:= false;
Screen.Cursor := crHourGlass;
If UseThreads then Arrow1.Top := Button2.top else
                   Arrow1.Top := Button6.top;
Arrow1.Visible := true;
Trackbar1.Position:=TrackBar1.Min;
Trackbar2.Position:=TrackBar2.Min;
InitMandel;
TimerTick:= 0;
Label3.Caption := TimeToStr(TimerTick);
Timer1.enabled := true;
for i := 1 to 960 do
	 begin
   startTime := GetTickCount64;
   if UseThreads then MandelThread else MandelLinear;
	 if stopFlag then
	    begin
	    Screen.Cursor := crDefault;
	    Timer1.Enabled := false;
	    repeat
	    sleep (100);
	    application.ProcessMessages;
	    until not  StopFlag;
	    Timer1.Enabled := true;
	    Screen.Cursor := crHourGlass;
	    end;
	 zoom := zoom * 1.025;
   XYZoom := (YMax / 3) * Zoom;
	 case i of
	      0..50: incF (Xoff,0.015);
	      51..160: incF (Xoff,0.003);
	      161..300: IncF(Xoff,0.0001);
	      301..530:IncF(Xoff,0.00000001);
	      531..1000: ;
	 end;

   Label10.caption := IntToStr(GetTickCount64-startTime)+' ms';
   if Zoom < 20 then Label1.Caption := 'Zoom '+formatFloat('0.#', zoom) else
		  Label1.Caption :=  'Zoom '+goodLooking(IntToStr(trunc(zoom)),3);
	 Label2.Caption := 'Loop '+IntTostr(i);
	 Label8.Caption :=  formatFloat('0.#', percent)+'%';
	 Label9.Caption :=  'Xoff '+formatFloat('0.########',Xoff);
	 application.ProcessMessages;
end;
Timer1.enabled := false;
{$ifdef Windows}
 Image1.Canvas.CopyRect(R,TheEnd.canvas,R);
 {$else}
 TheEnd.draw(Image1.Canvas,0,0,true);
 {$endif}
Screen.Cursor := crDefault;
Arrow1.Visible := false;
GroupBox1.Enabled:= True;
end;


procedure TForm1.Button1Click(Sender: TObject);
var startTime: QWord;
begin
   InitMandel;
   startTime := GetTickCount64;
   MandelThread;
   Label4.Caption := IntToStr(GetTickCount64 - StartTime)+' ms';
end;

procedure TForm1.Button5Click(Sender: TObject);
var startTime: QWord;
begin
  InitMandel;
  startTime := GetTickCount64;
  MandelLinear;
  Label4.Caption := IntToStr(GetTickCount64 - StartTime)+' ms';
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   Loop (false);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   Loop(true);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
   close;
end;


initialization
{$I TheEnd.lrs}


end.

