uniform mat4 ModelView, Projection;
uniform vec4 lightDiffuse, lightAmbient, color;
uniform vec3 lightPos;

in vec3  in_position, in_normal;

out vec4 diffuse, ambient, m_position;
out vec3 normal, halfVector;

void main(){
  // Pass the halfVector to the fragment shader.
  m_position = ModelView * vec4(in_position, 1.0);
  halfVector = normalize(lightPos - 2.0 * m_position.xyz);

  // Only works for orthogonal matrices.
  mat3 NormalMatrix =
    mat3(ModelView[0].xyz, ModelView[1].xyz, ModelView[2].xyz);

  // First transform the normal into eye space and normalize the result.
  normal = normalize(NormalMatrix * in_normal);

  // Compute the diffuse, ambient and globalAmbient terms.
  diffuse = color * lightDiffuse;
  ambient = color * lightAmbient;
  gl_Position = Projection * m_position;
}
