unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
  MemoryBuffer,
  GLUtils,
  GLTypes,
  web, webgl;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    WButton1: TWButton;
    WTimer1: TWTimer;
    procedure FormCreate(Sender: TObject);
    procedure WButton1Click(Sender: TObject);
    procedure WTimer1Timer(Sender: TObject);
  private
    gl: TJSWebGLRenderingContext;
    shader: TShader;
    buffer: TJSWebGLBuffer;
    viewTransform: TMatrix;
    procedure bClick(Sender: TObject);
  public

  end;

var
  WForm1: TWForm1;

implementation

{$R *.lfm}

{ TWForm1 }

procedure TWForm1.WTimer1Timer(Sender: TObject);
begin
  viewTransform.Rotate(0.03);
  shader.SetUniformMat4('viewTransform', viewTransform);

  gl.Clear(gl.COLOR_BUFFER_BIT);
  gl.drawArrays(gl.TRIANGLES, 0, 6);
end;

procedure TWForm1.bClick(Sender: TObject);
begin
  TWForm(Sender).Color:=Random($FFFFFF);
end;


function GetVertexData: TJSUInt8Array;
type
  GLVertex2 = record
    pos: TVec2;
    col: TVec3;
  end;

var
  buf: TMemoryBuffer;
  verts: TJSArray;
  v: GLVertex2;
  i: integer;
begin
  verts := TJSArray.new;

  v.pos := V2(-0.5, -0.5);
  v.col := V3(0.5, 0, 0);
  verts.push(v);

  v.pos := V2(-0.5, 0.5);
  v.col := V3(0, 0.5, 0);
  verts.push(v);

  v.pos := V2(0.5, 0.5);
  v.col := V3(0, 0, 0.5);
  verts.push(v);

  v.pos := V2(0.5, 0.5);
  v.col := V3(1, 0.5, 0.5);
  verts.push(v);

  v.pos := V2(-0.5, -0.5);
  v.col := V3(0.5, 1, 0.5);
  verts.push(v);

  v.pos := V2(0.5, -0.5);
  v.col := V3(0.5, 0.5, 1);
  verts.push(v);

  buf := TMemoryBuffer.Create(20 * verts.length);
  for i := 0 to verts.length - 1 do begin
    v := GLVertex2(verts[i]);
    buf.AddFloats(2, ToFloats(v.pos));
    buf.AddFloats(3, ToFloats3(v.col));
  end;

  Result := buf.GetBytes;
end;


procedure TWForm1.FormCreate(Sender: TObject);
var
  can: TJSHTMLCanvasElement;

  vertexShaderSource: string;
  fragmentShaderSource: string;
begin
  WTimer1.Enabled := False;
  can := TJSHTMLCanvasElement(document.createElement('canvas'));
  can.Width := 400;
  can.Height := 400;
  document.body.appendChild(can);
  gl := TJSWebGLRenderingContext(can.getContext('webgl'));
  if gl = nil then begin
    ShowMessage('OpenGL Fehler');
  end else begin
    ShowMessage('OpenGL io');
  end;

  vertexShaderSource :=
    'attribute vec2 inPos;' + 'attribute vec3 inCol;' + 'uniform mat4 viewTransform;' + 'varying vec3 col;' + 'void main(){' + '  gl_Position = viewTransform * vec4(inPos, 1.0, 1.0);' + '  col = inCol;}';

  fragmentShaderSource :=
    'precision highp float;' + 'varying vec3 col;' + 'void main(void){' + '  gl_FragColor = vec4(col, 1.0); }';

  shader := TShader.Create(gl, vertexShaderSource, fragmentShaderSource);
  shader.Compile;
  shader.BindAttribLocation(0, 'inPos');
  shader.BindAttribLocation(1, 'inCol');
  shader.Link;
  shader.Use;

  // prepare context
  gl.clearColor(1.0, 0.0, 0.0, 1);
  gl.viewport(0, 0, canvas.Width, canvas.Height);
  gl.Clear(gl.COLOR_BUFFER_BIT);

  // setup transform matricies
  viewTransform.Indenty;

  shader.SetUniformMat4('viewTransform', viewTransform);

  // create buffer
  buffer := gl.createBuffer;
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, GetVertexData, gl.STATIC_DRAW);

  // position
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 2, gl.FLOAT, False, 20, 0);

  // color
  gl.enableVertexAttribArray(1);
  gl.vertexAttribPointer(1, 3, gl.FLOAT, False, 20, 8);

  Width:=500;
  Height:=500;
  Left:=500;
  Top:=500;
  Visible:=False;
  WTimer1.Enabled := True;
end;

procedure TWForm1.WButton1Click(Sender: TObject);
var
  f:TWForm;
  b:TWButton;
begin
  f:=TWForm.CreateNew(Self);

  b:=TWButton.Create(f);
  b.Parent:=f;
  b.OnClick:=@bClick;

  f.ShowModal(nil);

end;

end.
