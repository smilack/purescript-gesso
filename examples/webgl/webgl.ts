type GLRecord = {
  gl: WebGLRenderingContext;
  program: WebGLProgram;
  positionAttributeLocation: GLint;
  positionBuffer: WebGLBuffer;
  colorUniformLocation: WebGLUniformLocation;
  angleUniformLocation: WebGLUniformLocation;
};

function main(): void {
  const canvas = document.getElementById("c") as HTMLCanvasElement;
  const gl = init(canvas);
  if (gl) {
    requestAnimationFrame(mkFrame(gl));
  }
}

function init(canvas: HTMLCanvasElement): GLRecord | void {
  const gl: WebGLRenderingContext = canvas.getContext("webgl");

  const vertexShader: WebGLShader | void = createShader(
    gl,
    gl.VERTEX_SHADER,
    vertexShaderSource,
  );
  const fragmentShader: WebGLShader | void = createShader(
    gl,
    gl.FRAGMENT_SHADER,
    fragmentShaderSource,
  );

  if (!vertexShader || !fragmentShader) {
    return;
  }

  const program: WebGLProgram | void = createProgram(
    gl,
    vertexShader,
    fragmentShader,
  );

  if (!program) {
    return;
  }

  const positionAttributeLocation: GLint = gl.getAttribLocation(
    program,
    "a_position",
  );
  const positionBuffer: WebGLBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

  const positions = [
    0,
    0.67,
    0.57,
    -0.33,
    -0.57,
    -0.33,
  ];
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

  const colorUniformLocation: WebGLUniformLocation = gl.getUniformLocation(
    program,
    "u_color",
  );
  const angleUniformLocation: WebGLUniformLocation = gl.getUniformLocation(
    program,
    "u_angle",
  );

  return {
    gl,
    program,
    positionAttributeLocation,
    positionBuffer,
    colorUniformLocation,
    angleUniformLocation,
  };
}

function mkFrame(gl: GLRecord): (timestamp: DOMHighResTimeStamp) => void {
  return frame.bind(null, gl);
}

function frame(glr: GLRecord, timestamp: DOMHighResTimeStamp) {
  const { gl } = glr;
  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
  gl.clearColor(0.0, 0.0, 0.0, 0.0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  let r = (Math.cos(timestamp / 1000) + 1) / 2,
    g = (Math.sin(timestamp / 900) + 1) / 2,
    b = (Math.sin(timestamp / 800) + 1) / 2;

  gl.useProgram(glr.program);
  gl.uniform4f(glr.colorUniformLocation, r, g, b, 1.0);
  gl.uniform1f(glr.angleUniformLocation, (timestamp / 2000) % (2 * Math.PI));
  gl.enableVertexAttribArray(glr.positionAttributeLocation);
  gl.bindBuffer(gl.ARRAY_BUFFER, glr.positionBuffer);
  const size = 2;
  const dataType = gl.FLOAT;
  const normalize = false;
  const stride = 0;
  const offset = 0;
  gl.vertexAttribPointer(
    glr.positionAttributeLocation,
    size,
    dataType,
    normalize,
    stride,
    offset,
  );

  const primitiveType = gl.TRIANGLES;
  const count = 3;
  gl.drawArrays(primitiveType, offset, count);

  requestAnimationFrame(mkFrame(glr));
}

function createProgram(
  gl: WebGLRenderingContext,
  vertexShader: WebGLShader,
  fragmentShader: WebGLShader,
): WebGLProgram | void {
  const program: WebGLProgram = gl.createProgram();
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);
  gl.linkProgram(program);
  const success: boolean = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (success) {
    return program;
  } else {
    console.log(gl.getProgramInfoLog(program));
    gl.deleteProgram(program);
  }
}

function createShader(
  gl: WebGLRenderingContext,
  shaderType: GLenum,
  source: string,
): WebGLShader | void {
  const shader: WebGLShader = gl.createShader(shaderType);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  const success: boolean = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
  if (success) {
    return shader;
  } else {
    console.log(gl.getShaderInfoLog(shader));
    gl.deleteShader(shader);
  }
}

const vertexShaderSource = `
attribute vec2 a_position;

uniform float u_angle;

void main() {
   vec2 u_rotation = vec2(sin(u_angle), cos(u_angle));
   vec2 rotatedPosition = vec2(
     a_position.x * u_rotation.y + a_position.y * u_rotation.x,
     a_position.y * u_rotation.y - a_position.x * u_rotation.x
   );
   gl_Position = vec4(rotatedPosition, 0, 1);
}
`;

const fragmentShaderSource = `
precision mediump float;

uniform vec4 u_color;

void main() {
  gl_FragColor = u_color;
}
`;

main();
