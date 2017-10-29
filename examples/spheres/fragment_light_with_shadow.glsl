uniform vec3 lightPos;
uniform float specular,shininess;
uniform sampler2D texture1;
uniform highp sampler2DShadow shadowmap;
uniform mat4 shadowproj;
in vec3 normal,halfVector;
in vec4 diffuse,ambient,m_position;
in vec2 tex_coordinates;
out vec4 FragColor;
void main()
{
  vec3 n,halfV,lightDir;
  float NdotL,NdotHV;

  lightDir = normalize(lightPos - m_position.xyz);

  /* The ambient term will always be present */
  vec4 color = ambient;

  vec4 s_pos = shadowproj * m_position;
  float coef = texture(shadowmap,(s_pos.xyz/s_pos.w+vec3(1.,1.,1.))/2.);


  if (coef > 0.0) {
    /* a fragment shader can't write a varying variable, hence we need
       a new variable to store the normalized interpolated normal */
    n = normalize(normal);
    /* compute the dot product between normal and ldir */

    NdotL = dot(n,lightDir);
    if (NdotL > 0.0) {
      color += coef * diffuse * NdotL;
      halfV = normalize(halfVector);
      NdotHV = max(dot(n,halfV),0.0);
      color += coef * specular * pow(NdotHV, shininess);
    }}

  FragColor=texture(texture1,tex_coordinates)*color;
}
