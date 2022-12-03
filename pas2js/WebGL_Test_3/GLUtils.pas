unit GLUtils;

interface

{$modeswitch typehelpers}
uses
  MemoryBuffer,
  BrowserConsole, WebGL, JS,
  Types, SysUtils;

type
  TVector3f = array [0..2] of single;

//  PSingleArray=^TSingleArray;
  TSingleArray = array[0..15] of single;


  TMatrix = array[0..3, 0..3] of GLfloat;

  { TMatrixfHelper }

  TMatrixfHelper = type Helper for TMatrix
    //    List: array[0..15] of single absolute Self;
    procedure Indenty;                  // Generiere eine Einheitsmatrix
    procedure Rotate(angele: single);   // Drehe Matrix
    function GetList: TSingleArray;
  end;

type
  //  TJSFloat32List = array of single;


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


const
  kModelVertexFloats = 3 + 2 + 3;

function GLSizeof(glType: nativeint): integer;

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

function TMatrixfHelper.GetList: TSingleArray;
var
  x, y: integer;
begin
  for x := 0 to 3 do begin
    for y := 0 to 3 do begin
      Result[x + y * 4] := Self[x, y];
    end;
  end;
end;

{=============================================}
{@! ___Utilities___ }
{=============================================}

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
    end;
  end;
end;

{=============================================}
{@! ___Shader___ }
{=============================================}
function TShader.GetUniformLocation(Name: string): TJSWebGLUniformLocation;
begin
  // TODO: cache these. how do we use dictionarys from JS in Pascal?
  Result := gl.getUniformLocation(programID, Name);
end;

procedure TShader.SetUniformFloat(Name: string; Value: GLfloat);
begin
  gl.uniform1f(GetUniformLocation(Name), Value);
end;

procedure TShader.SetUniformVec3(Name: string; Value: TVector3f);
begin
  //gl.uniform3fv(GetUniformLocation(name), ToFloats(value));
  gl.uniform3f(GetUniformLocation(Name), Value[0], Value[1], Value[2]);
end;

procedure TShader.SetUniformMat4(Name: string; Value: TMatrix);
type
  t = array[0..15] of single;
var
  m: t absolute Value;
begin
//  gl.uniformMatrix4fv(GetUniformLocation(Name), False, m);
    gl.uniformMatrix4fv(GetUniformLocation(Name), False, Value.GetList);
end;

function TShader.GetAttribLocation(Name: string): GLint;
begin
  Result := gl.getAttribLocation(programID, Name);
end;

procedure TShader.BindAttribLocation(index: GLuint; Name: string);
begin
  gl.bindAttribLocation(programID, index, Name);
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
    Writeln('create shader failed');
  end;
  gl.shaderSource(Result, Source);
  gl.compileShader(Result);
  if gl.getShaderParameter(Result, gl.COMPILE_STATUS) then begin
    //writeln('loaded shader ', theType);
    exit;
  end else begin
    //    Fatal(gl.getShaderInfoLog(Result));
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
    //    Fatal(gl.getProgramInfoLog(programID));
    //gl.deleteProgram(programID);
  end;
end;

procedure TShader.Use;
begin
  gl.useProgram(programID);
end;

end.
