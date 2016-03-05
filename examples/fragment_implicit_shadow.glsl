uniform mat4 Projection,ModelView;
uniform vec3 lightPos;
in vec3 pos;
out vec4 FragColor;

void main()
{
  vec3 p = solve(lightPos,pos);
  vec4 m_position = ModelView * vec4(p,1.0);
  vec4 Position = Projection * m_position;
  float depth = (Position.z / Position.w + 1.0) / 2.0;
  FragColor=vec4(depth,mod(depth,1./256.)*256.0,depth,1.0);
}
