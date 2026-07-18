uniform vec3 lightPos;
uniform float specular,shininess;
uniform sampler2D texture1, texture2;
in vec3 normal,halfVector;
in vec4 diffuse,ambient,m_position;
in vec2 tex_coordinates;
flat in int topFace;
out vec4 FragColor;

void main()
{
  vec3 n,halfV,lightDir;
  float NdotL,NdotHV;

  lightDir = normalize(lightPos - m_position.xyz);

  /* The ambient term will always be present */
  vec4 color = ambient;
  /* a fragment shader can't write a varying variable, hence we need
  a new variable to store the normalized interpolated normal */
  n = normalize(normal);
  /* compute the dot product between normal and ldir */

  NdotL = dot(n,lightDir);
  if (NdotL > 0.0) {
     color += diffuse * NdotL;
     halfV = normalize(halfVector);
     NdotHV = max(dot(n,halfV),0.0);
     color += specular * pow(NdotHV, shininess);
  }
  if (topFace != 1) {
    FragColor=(vec4(1.0) - texture(texture1,tex_coordinates))*color;
  } else {
    FragColor=texture(texture2,tex_coordinates)*color;
  }
}