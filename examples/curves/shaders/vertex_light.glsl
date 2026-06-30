uniform mat4 ModelView, Projection;
uniform vec4 lightDiffuse, lightAmbient, color;
uniform vec3 lightPos;

in vec3  in_position;

out vec4 diffuse, ambient, m_position;

void main(){
  // Pass the halfVector to the fragment shader.
  m_position = ModelView * vec4(in_position, 1.0);

  // Compute the diffuse, ambient and globalAmbient terms.
  diffuse = color * lightDiffuse;
  ambient = color * lightAmbient;
  gl_Position = Projection * m_position;
}
