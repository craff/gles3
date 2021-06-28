
in vec3  in_position;

// almost all work is done by the geometry shader.
// gl_Position requires vec4.
void main(){
  gl_Position = vec4(in_position,1);
}
