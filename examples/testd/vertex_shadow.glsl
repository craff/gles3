in vec3 in_position;
uniform mat4 ModelView,Projection;
out vec3 pos;
out vec4 Position;
void main()
{
  pos = in_position;
  gl_Position = Projection * ModelView * vec4(in_position,1.0);
  Position = gl_Position;
}
