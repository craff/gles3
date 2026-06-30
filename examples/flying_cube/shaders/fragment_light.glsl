uniform vec3 lightPos;
uniform float specular, shininess;

in vec3 normal, halfVector;
in vec4 diffuse, ambient, m_position;

out vec4 FragColor;

void main(){
  vec3 halfV, lightDir;
  float NdotL, NdotHV;

  lightDir = normalize(lightPos - m_position.xyz);

  // The ambient term will always be present.
  vec4 color = ambient;

  // compute the dot product between normal and ldir.
  NdotL = dot(normal, lightDir);
  if(NdotL > 0.0){
    color += diffuse * NdotL;
    halfV = normalize(halfVector);
    NdotHV = max(dot(normal, halfV),0.0);
    color += specular * pow(NdotHV, shininess);
  }

  FragColor = color;
}
