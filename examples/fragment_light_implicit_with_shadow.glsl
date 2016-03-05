uniform vec3 eyePos,lightPos;
uniform vec4 lightDiffuse,lightAmbient,color;
uniform float specular,shininess;
uniform mat4 Projection,ModelView;
uniform mat3 NormalMatrix;
uniform highp sampler2DShadow shadowmap;
uniform mat4 shadowproj;
in vec3 pos;
out vec4 FragColor;

void main()
{
  vec3 p = solve(eyePos,pos);
  vec4 m_position = ModelView * vec4(p,1.0);
  vec4 gPos = Projection * m_position;

  vec3 n = normalize (NormalMatrix * df(p));
  vec3 halfV,lightDir;
  float NdotL,NdotHV;

  lightDir = normalize(lightPos - m_position.xyz);

  /* The ambient term will always be present */
  vec4 col = lightAmbient * color;

  vec4 s_pos = shadowproj * m_position;
  float coef = texture(shadowmap,(s_pos.xyz/s_pos.w+vec3(1.,1.,1.))/2.);


  if (coef > 0.0) {

  NdotL = dot(n,lightDir);
    if (NdotL > 0.0) {
      col += lightDiffuse * color * NdotL;
      halfV = normalize(lightPos - 2.0 * p.xyz);
      NdotHV = max(dot(n,halfV),0.0);
      col += specular * pow(NdotHV, shininess);
    }

  }
  FragColor=col;
  float depth = gPos.z / gPos.w;
  gl_FragDepth=(depth+1.0)/2.0;
}
