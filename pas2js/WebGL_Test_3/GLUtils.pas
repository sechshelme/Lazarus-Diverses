unit GLUtils;

interface

{$modeswitch typehelpers}
uses
  MemoryBuffer,
  BrowserConsole, WebGL, JS,
  Types, SysUtils;

type
  TVector3f = array [0..2] of single;


type
  TMatrix = array[0..3, 0..3] of GLfloat;

  TMatrixfHelper = type Helper for TMatrix
    procedure Indenty;                  // Generiere eine Einheitsmatrix
    procedure Rotate(angele: single);   // Drehe Matrix
  end;

type
  TJSFloat32List = array of single;


  TShader = class
  public
    constructor Create(context: TJSWebGLRenderingContext; vertexShaderSource, fragmentShaderSource: string);
    procedure Compile;
    procedure Link;
    procedure Use;

    function GetAttribLocation(Name: string): GLint;
    procedure BindAttribLocation(index: GLuint; Name: string);

    procedure SetUniformMat4(Name: string; Value: TMatrix);
    procedure SetUniformVec3(Name: string; Value: TVector3f);
    procedure SetUniformFloat(Name: string; Value: GLfloat);

  private
    gl: TJSWebGLRenderingContext;
    vertexShader: TJSWebGLShader;
    fragmentShader: TJSWebGLShader;
    programID: TJSWebGLProgram;

    function GetUniformLocation(Name: string): TJSWebGLUniformLocation;
    function CreateShader(theType: GLenum; Source: string): TJSWebGLShader;
  end;


type
  TModelData = record
    verticies: TJSFloat32Array;     // GLfloat

    // NOTE: it's not clear if WebGL supports GLuint
    // https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/drawElements

    indicies: TJSUint16Array;           // GLushort
    floatsPerVertex: integer;
  end;

const
  kModelVertexFloats = 3 + 2 + 3;

type
  TModel = class
  public
    constructor Create(context: TJSWebGLRenderingContext; modelData: TModelData); overload;
    procedure Draw;
  private
    gl: TJSWebGLRenderingContext;
    Data: TModelData;
    vertexBuffer: TJSWebGLBuffer;
    indexBuffer: TJSWebGLBuffer;
    //elementCount: integer;

    procedure EnableAttributes;
    procedure Load;
  end;

function GLSizeof(glType: nativeint): integer;
procedure GLFatal(gl: TJSWebGLRenderingContext; messageString: string = 'Fatal OpenGL error');

implementation

procedure TMatrixfHelper.Indenty;
const
  MatrixIndenty: TMatrix = ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));
begin
  Self := MatrixIndenty;
end;

procedure TMatrixfHelper.Rotate(angele: single);
var
  i: integer;
  x, y: GLfloat;
begin
  for i := 0 to 1 do begin
    x := Self[i, 0];
    y := Self[i, 1];
    Self[i, 0] := x * cos(angele) - y * sin(angele);
    Self[i, 1] := x * sin(angele) + y * cos(angele);
  end;
end;

{=============================================}
{@! ___Utilities___ }
{=============================================}

procedure Fatal(messageString: string); overload;
begin
  writeln('*** FATAL: ', messageString);
  raise Exception.Create('FATAL');
end;

// TODO: toll free bridge to FPC strings
{procedure Fatal (messageString: TJSString); overload;
begin
    writeln('*** FATAL: ', messageString);
    raise Exception.Create('FATAL');
end;}

procedure GLFatal(gl: TJSWebGLRenderingContext; messageString: string = 'Fatal OpenGL error');
var
  error: integer;
begin
  error := gl.getError();
  if error <> TJSWebGLRenderingContext.NO_ERROR then begin
    // TODO: case doesn't work?
    case error of
      TJSWebGLRenderingContext.INVALID_VALUE: begin
        messageString := messageString + ' (GL_INVALID_VALUE)';
      end;
      TJSWebGLRenderingContext.INVALID_OPERATION: begin
        messageString := messageString + ' (GL_INVALID_OPERATION)';
      end;
      TJSWebGLRenderingContext.INVALID_ENUM: begin
        messageString := messageString + ' (GL_INVALID_ENUM)';
      end;
      otherwise
      begin
        messageString := messageString + ' ' + IntToStr(error);
      end;
    end;
    Fatal(messageString);
  end;
end;

function GLSizeof(glType: nativeint): integer;
begin
  case glType of
    TJSWebGLRenderingContext.UNSIGNED_BYTE, TJSWebGLRenderingContext.byte: begin
      Result := 1;
    end;
    TJSWebGLRenderingContext.SHORT, TJSWebGLRenderingContext.UNSIGNED_SHORT: begin
      Result := 2;
    end;
    TJSWebGLRenderingContext.INT, TJSWebGLRenderingContext.UNSIGNED_INT: begin
      Result := 4;
    end;
    TJSWebGLRenderingContext.FLOAT: begin
      Result := 4;
    end;
    otherwise
    begin
      Fatal('GLSizeof type is invalid.');
    end;
  end;
end;

{=============================================}
{@! ___Model___ }
{=============================================}

constructor TModel.Create(context: TJSWebGLRenderingContext; modelData: TModelData);
begin
  gl := context;
  Data := modelData;
  Load;
end;

procedure TModel.Draw;
begin
  gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

  EnableAttributes;
  gl.drawElements(gl.TRIANGLES, Data.indicies.length, gl.UNSIGNED_SHORT, 0);

  gl.bindBuffer(gl.ARRAY_BUFFER, nil);
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, nil);
end;

procedure TModel.EnableAttributes;
var
  offset: integer;
  stride: integer;
begin

  // NOTE: we don't have VAO's yet so we need to enable vertex attributes for shader
  // before every draw call (unless the array buffer hasn't changed between calls)
  offset := 0;
  stride := Data.floatsPerVertex * GLSizeof(TJSWebGLRenderingContext.FLOAT);

  // position
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 3, gl.FLOAT, False, stride, offset);
  offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 3;

  // texture
  gl.enableVertexAttribArray(1);
  gl.vertexAttribPointer(1, 2, gl.FLOAT, False, stride, offset);
  offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 2;

  // normal
  gl.enableVertexAttribArray(2);
  gl.vertexAttribPointer(2, 3, gl.FLOAT, False, stride, offset);
  offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 3;
end;

procedure TModel.Load;
begin
  indexBuffer := gl.createBuffer;
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, Data.indicies, gl.STATIC_DRAW);
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, nil);

  vertexBuffer := gl.createBuffer;
  gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, Data.verticies, gl.STATIC_DRAW);
  gl.bindBuffer(gl.ARRAY_BUFFER, nil);
end;

{=============================================}
{@! ___Shader___ }
{=============================================}
function TShader.GetUniformLocation(Name: string): TJSWebGLUniformLocation;
begin
  // TODO: cache these. how do we use dictionarys from JS in Pascal?
  Result := gl.getUniformLocation(programID, Name);
  GLFatal(gl, 'gl.getUniformLocation');
end;

procedure TShader.SetUniformFloat(Name: string; Value: GLfloat);
begin
  gl.uniform1f(GetUniformLocation(Name), Value);
  GLFatal(gl, 'gl.uniform1f');
end;

procedure TShader.SetUniformVec3(Name: string; Value: TVector3f);
begin
  //gl.uniform3fv(GetUniformLocation(name), ToFloats(value));
  gl.uniform3f(GetUniformLocation(Name), Value[0], Value[1], Value[2]);
  GLFatal(gl, 'gl.uniform3fv');
end;

procedure TShader.SetUniformMat4(Name: string; Value: TMatrix);
var
  list: TJSFloat32List;
  x, y: integer;
begin
  SetLength(list, 16);
  for x := 0 to 3 do begin
    for y := 0 to 3 do begin
      list[x + y * 4] := Value[x, y];
    end;
  end;
  gl.uniformMatrix4fv(GetUniformLocation(Name), False, list);
  GLFatal(gl, 'gl.uniformMatrix4fv');
end;

function TShader.GetAttribLocation(Name: string): GLint;
begin
  Result := gl.getAttribLocation(programID, Name);
end;

procedure TShader.BindAttribLocation(index: GLuint; Name: string);
begin
  gl.bindAttribLocation(programID, index, Name);
  //GLFatal('glBindAttribLocation '+IntToStr(index)+':'+name);
end;

constructor TShader.Create(context: TJSWebGLRenderingContext; vertexShaderSource, fragmentShaderSource: string);
begin
  gl := context;
  vertexShader := CreateShader(gl.VERTEX_SHADER, vertexShaderSource);
  fragmentShader := CreateShader(gl.FRAGMENT_SHADER, fragmentShaderSource);
end;

function TShader.CreateShader(theType: GLenum; Source: string): TJSWebGLShader;
begin
  Result := gl.createShader(theType);
  if Result = nil then begin
    Fatal('create shader failed');
  end;
  gl.shaderSource(Result, Source);
  gl.compileShader(Result);
  if gl.getShaderParameter(Result, gl.COMPILE_STATUS) then begin
    //writeln('loaded shader ', theType);
    exit;
  end else begin
    Fatal(gl.getShaderInfoLog(Result));
    //gl.deleteShader(shader);
  end;
end;

procedure TShader.Compile;
begin
  programID := gl.createProgram;
  gl.attachShader(programID, vertexShader);
  gl.attachShader(programID, fragmentShader);
end;

procedure TShader.Link;
begin
  gl.linkProgram(programID);
  if not gl.getProgramParameter(programID, gl.LINK_STATUS) then begin
    Fatal(gl.getProgramInfoLog(programID));
    //gl.deleteProgram(programID);
  end;
end;

procedure TShader.Use;
begin
  gl.useProgram(programID);
end;

end.
