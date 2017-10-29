in vec3 in_position;
out vec3 pos;
uniform mat4 Projection, ModelView;
void main()
{
  pos = in_position;
  gl_Position = Projection * ModelView * vec4(in_position,1.0);
}
