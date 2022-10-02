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

const
  kSIZEOF_VERTEX = 20;   // vec2 + vec3

var
  gl: TJSWebGLRenderingContext;
  shader: TShader;
  viewTransform: TMat4;

  canvas: TJSHTMLCanvasElement;
  buffer: TJSWebGLBuffer;

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
    v.col := V3(1, 0, 0);
    verts.push(v);

    v.pos := V2(-0.5, 0.5);
    v.col := V3(0, 1, 0);
    verts.push(v);

    v.pos := V2(0.5, 0.5);
    v.col := V3(0, 0, 1);
    verts.push(v);

    // pack the array of verticies into a byte buf
    buf := TMemoryBuffer.Create(kSIZEOF_VERTEX * verts.length);
    for i := 0 to verts.length - 1 do begin
      v := GLVertex2(verts[i]);
      buf.AddFloats(2, ToFloats(v.pos));
      buf.AddFloats(3, ToFloats3(v.col));
    end;

    Result := buf.GetBytes;
  end;

  procedure InitCanvas;
  var
    offset: integer;
    vertexShaderSource: string;
    fragmentShaderSource: string;

  begin
    // make webgl context
    canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
    canvas.Width := 400;
    canvas.Height := 400;
    document.body.appendChild(canvas);

    gl := TJSWebGLRenderingContext(canvas.getContext('webgl'));
    if gl = nil then begin
      writeln('failed to load webgl!');
      exit;
    end;

    vertexShaderSource :=
      'attribute vec2 inPos;' +
      'attribute vec3 inCol;' +
      'uniform mat4 viewTransform;' +
      'varying vec3 col;' + 'void main(){' +
      '  gl_Position = viewTransform * vec4(inPos, 1.0, 1.0);' +
      '  col = inCol;}';

    fragmentShaderSource :=
      'precision highp float;' +
      'varying vec3 col;' +
      'void main(void){' +
      '  gl_FragColor = vec4(col, 1.0); }';

    shader := TShader.Create(gl, vertexShaderSource, fragmentShaderSource);
    shader.Compile;
    shader.BindAttribLocation(0, 'inPos');
    shader.BindAttribLocation(1, 'inCol');
    shader.Link;
    shader.Use;

    // prepare context
    gl.clearColor(0.3, 0.0, 0.0, 1);
    gl.viewport(0, 0, canvas.Width, canvas.Height);
    gl.Clear(gl.COLOR_BUFFER_BIT);

    // setup transform matricies
    viewTransform := TMat4.Identity;

    shader.SetUniformMat4('viewTransform', viewTransform);

    // create buffer
    buffer := gl.createBuffer;
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, GetVertexData, gl.STATIC_DRAW);

    offset := 0;

    // position
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 2, gl.FLOAT, False, kSIZEOF_VERTEX, offset);
    offset += GLSizeof(gl.FLOAT) * 2;

    // color
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 3, gl.FLOAT, False, kSIZEOF_VERTEX, offset);
    offset += GLSizeof(gl.FLOAT) * 3;
  end;

  procedure UpdateCanvas(time: TJSDOMHighResTimeStamp);
  const
    rotateAngle: double = 0;
  begin
    rotateAngle := rotateAngle + 0.01;

    viewTransform.RotateZ(rotateAngle);
    shader.SetUniformMat4('viewTransform', viewTransform);

    //writeln(deltaTime);
    gl.Clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 3);

    window.requestAnimationFrame(@UpdateCanvas);
  end;

begin
  Writeln('WebGL Demo');
  InitCanvas;

  // fire off the timer to draw
  window.requestAnimationFrame(@UpdateCanvas);
end.
