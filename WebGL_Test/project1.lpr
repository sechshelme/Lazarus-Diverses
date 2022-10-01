program project1;

{$mode objfpc}

uses
  browserconsole,
  browserapp,
  JS,
  Classes,
  SysUtils,
  Web,
  Mat4,
  MemoryBuffer,
  GLUtils,
  GLTypes,
  WebGL,
  Math;

type
  GLVertex2 = record
    pos: TVec2;
    color: TRGBAb;
  end;

const
  kSIZEOF_VERTEX = 12;   // vec2 + RGBAb

  function GetVertexData: TJSUInt8Array;
  var
    buffer: TMemoryBuffer;
    verts: TJSArray;
    v: GLVertex2;
    i: integer;
  begin

    // there's really no reason to build the array
    // as vectors first then pack into bytes but
    // we're doing it anyways because this how most
    // will be familar with vertex data from standard
    // OpenGL
    verts := TJSArray.new;

    v.pos := V2(0, 0);
    v.color := RGBAb(255, 0, 0, 255);
    verts.push(v);

    v.pos := V2(0, 100);
    v.color := RGBAb(0, 255, 0, 255);
    verts.push(v);

    v.pos := V2(100, 100);
    v.color := RGBAb(0, 0, 255, 255);
    verts.push(v);

    // pack the array of verticies into a byte buffer
    buffer := TMemoryBuffer.Create(kSIZEOF_VERTEX * verts.length);
    for i := 0 to verts.length - 1 do begin
      v := GLVertex2(verts[i]);
      buffer.AddFloats(2, {v.pos}ToFloats(v.pos));
      buffer.AddBytes(4, v.color);
    end;

    Result := buffer.GetBytes;
  end;


var
  nextTime: TJSFloat32 = 0;
  deltaTime: TJSFloat32 = 0;

var
  gl: TJSWebGLRenderingContext;
  shader: TShader;
  projTransform: TMat4;
  viewTransform: TMat4;
  modelTransform: TMat4;

var
  rotateAngle: double = 0;

  procedure UpdateCanvas(time: TJSDOMHighResTimeStamp);
  var
    now: TJSFloat32;
  begin
    now := time * 0.001;
    deltaTime := now - nextTime;
    nextTime := now;

    modelTransform := TMat4.Identity;
    modelTransform := modelTransform.Multiply(TMat4.Translate(100, 100, 0));
    modelTransform := modelTransform.Multiply(TMat4.RotateZ(DegToRad(rotateAngle)));

    rotateAngle := rotateAngle + (20 * deltaTime);

    shader.SetUniformMat4('modelTransform', modelTransform);

    //writeln(deltaTime);
    gl.Clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 3);

    window.requestAnimationFrame(@UpdateCanvas);
  end;

var
  canvas: TJSHTMLCanvasElement;
  stride: integer;
  offset: integer;
  vertexShaderSource: string;
  fragmentShaderSource: string;
  buffer: TJSWebGLBuffer;
begin

  Writeln('fdgfgfdggfd');

  // make webgl context
  canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  canvas.Width := 700;
  canvas.Height := 700;
  document.body.appendChild(canvas);

  gl := TJSWebGLRenderingContext(canvas.getContext('webgl'));
  if gl = nil then begin
    writeln('failed to load webgl!');
    exit;
  end;

  // create shaders from source in html
  //  vertexShaderSource := document.getElementById('vertex.glsl').textContent;
  //  fragmentShaderSource := document.getElementById('fragment.glsl').textContent;

  vertexShaderSource := '' + LineEnding + 'attribute vec2 inPos; varying vec2 pos;  void main(){ gl_Position = vec4(inPos, 1.0, 1.0);   pos = inPos.xy;}';

  fragmentShaderSource := '' + LineEnding + 'precision highp float; varying vec2 pos;  void main(void){  gl_FragColor = vec4(pos, 0.0, 1.0); }';


  shader := TShader.Create(gl, vertexShaderSource, fragmentShaderSource);
  shader.Compile;
  shader.BindAttribLocation(0, 'inPos');
  //  shader.BindAttribLocation(1, 'in_color');
  shader.Link;
  shader.Use;

  // prepare context
  gl.clearColor(0.3, 0.0, 0.0, 1);
  gl.viewport(0, 0, canvas.Width, canvas.Height);
  gl.Clear(gl.COLOR_BUFFER_BIT);

  // setup transform matricies
  projTransform := TMat4.Ortho(0, gl.canvas.Width, gl.canvas.Height, 0, -1, 1);
  viewTransform := TMat4.Identity;
  modelTransform := TMat4.Identity;

  //shader.SetUniformMat4('projTransform', projTransform);
  //shader.SetUniformMat4('viewTransform', viewTransform);
  //shader.SetUniformMat4('modelTransform', modelTransform);

  // create buffer
  buffer := gl.createBuffer;
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, GetVertexData, gl.STATIC_DRAW);

  offset := 0;
  stride := kSIZEOF_VERTEX;

  // position
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 2, gl.FLOAT, False, stride, offset);
  offset += GLSizeof(gl.FLOAT) * 2;

  // color (normalized = true since we're using unsigned byte)
  gl.enableVertexAttribArray(1);
  gl.vertexAttribPointer(1, 4, gl.UNSIGNED_BYTE, True, stride, offset);
  offset += GLSizeof(gl.UNSIGNED_BYTE) * 4;

  // fire off the timer to draw
  window.requestAnimationFrame(@UpdateCanvas);
end.
