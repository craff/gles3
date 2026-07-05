uniform vec3 lightPos;
uniform float specular, shininess;
uniform mat4 ModelView, Projection;

in vec3 normal, halfVector, w_position;
in vec4 diffuse, ambient, m_position;

out vec4 FragColor;

float compare_center(vec3 pos, vec3 center,
                     float bestDist2,
                     out vec3 bestCenter)
{
    float d2 = dot(pos - center, pos - center);

    if (d2 < bestDist2) {
        bestCenter = center;
        return d2;
    }

    bestCenter = vec3(0.0); // ignoré si la distance n'est pas meilleure
    return bestDist2;
}

bool compare_centers(
    vec3 pos,
    vec3 centers[6],
    int count,
    float radius,
    out vec3 bestCenter)
{
    bestCenter = vec3(0.0, 0.0, 0.0);
    float bestDist2 = 1e30;
    for (int i = 0; i < count; ++i)
        bestDist2 = compare_center(pos, centers[i], bestDist2, bestCenter);
    return bool(bestDist2 < radius * radius);
}

const vec3 face1[6] = vec3[](
    vec3(0.0, 0.5,   0.5),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0)
);

const vec3 face2[6] = vec3[](
    vec3(0.25, 0.25, 0.0),
    vec3(0.75, 0.75, 0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0)
);

const vec3 face3[6] = vec3[](
    vec3(0.25, 0.0, 0.25),
    vec3(0.50, 0.0, 0.50),
    vec3(0.75, 0.0, 0.75),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0)
);

const vec3 face4[6] = vec3[](
    vec3(0.25, 1.0, 0.25),
    vec3(0.25, 1.0, 0.75),
    vec3(0.75, 1.0, 0.25),
    vec3(0.75, 1.0, 0.75),
    vec3(0.0, 0.0,   0.0),
    vec3(0.0, 0.0,   0.0)
);

const vec3 face5[6] = vec3[](
    vec3(0.25, 0.25, 1.0),
    vec3(0.25, 0.75, 1.0),
    vec3(0.50, 0.50, 1.0),
    vec3(0.75, 0.25, 1.0),
    vec3(0.75, 0.75, 1.0),
    vec3(0.0, 0.0,   0.0)
);

const vec3 face6[6] = vec3[](
    vec3(1.0, 0.166, 0.25),
    vec3(1.0, 0.166, 0.75),
    vec3(1.0, 0.500, 0.25),
    vec3(1.0, 0.500, 0.75),
    vec3(1.0, 0.833, 0.25),
    vec3(1.0, 0.833, 0.75)
);

void main(){
  vec3 halfV, lightDir;
  float NdotL, NdotHV;

  lightDir = normalize(lightPos - m_position.xyz);

  // The ambient term will always be present.
  vec4 color = ambient;
  vec4 vdiffuse = diffuse;
  float dist2 = 1.0;
  float r2 = 0.01;
  vec3 center = vec3(0.0,0.0,0.0);
  bool hit = false;
  if (w_position.x == 0.0)
    { hit = compare_centers(w_position, face1, 1, 0.15, center); }
  else if (w_position.x == 1.0)
    { hit = compare_centers(w_position, face6, 6, 0.1, center); }
  else if (w_position.z == 0.0)
    { hit = compare_centers(w_position, face2, 2, 0.1, center); }
  else if (w_position.z == 1.0)
    { hit = compare_centers(w_position, face5, 5, 0.1, center); }
  else if (w_position.y == 0.0)
    { hit = compare_centers(w_position, face3, 3, 0.1, center); }
  else if (w_position.y == 1.0)
    { hit = compare_centers(w_position, face4, 4, 0.1, center); }
  if (hit) {
    vdiffuse = vec4(0,0,0,0);
  }
  // compute the dot product between normal and ldir.
  NdotL = dot(normal, lightDir);
  if(NdotL > 0.0){
    color += vdiffuse * NdotL;
    halfV = normalize(halfVector);
    NdotHV = max(dot(normal, halfV),0.0);
    color += specular * pow(NdotHV, shininess);
  }

  FragColor = color;
}
