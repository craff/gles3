uniform mat4 ModelView, Projection;

in vec3  in_position;
in uint face;
out vec4 m_position;
flat out uint v_face;
out vec3 w_position;

void main(){
  m_position = ModelView * vec4(in_position, 1.0);
  gl_Position = Projection * m_position;
  v_face = face;
  w_position = in_position;
}
