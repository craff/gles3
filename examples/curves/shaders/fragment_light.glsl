uniform vec3 lightPos;

in vec4 diffuse, ambient, m_position;

out vec4 FragColor;

void main(){
  vec3 halfV, lightDir;
  float att;

  lightDir = normalize(lightPos - m_position.xyz);

  // The ambient term will always be present.
  vec4 color = ambient;

  // compute the dot product between normal and ldir.
  att = 2.0 * dot(lightDir, lightDir);
  if(att > 0.0){
    color += diffuse * att;
  }

  FragColor = color;
}
