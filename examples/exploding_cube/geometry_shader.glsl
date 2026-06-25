uniform mat4 ModelView, Projection;
uniform vec4 lightDiffuse, lightAmbient, color;
uniform vec3 lightPos;
uniform float explosion_factor;

layout (triangles) in;
layout (max_vertices=3) out;
layout (triangle_strip) out;

out vec4 diffuse, ambient,m_position;
out vec3 normal, halfVector;

vec4 m_pos[3];

// Compute the diffuse, ambient and globalAmbient terms.



vec3 GetNormal()
{
   vec3 a = vec3(m_pos[0])/m_pos[0][3] - vec3(m_pos[2])/m_pos[2][3];
   vec3 b = vec3(m_pos[1])/m_pos[1][3] - vec3(m_pos[2])/m_pos[2][3];
   return normalize(cross(a, b));
}

vec4 Explode(vec4 pos, vec3 normal)
{
   return(pos + explosion_factor*vec4(normal,0));
}

void main(){
  // compute coordinates in camera frame
  m_pos[0] = ModelView * gl_in[0].gl_Position;
  m_pos[1] = ModelView * gl_in[1].gl_Position;
  m_pos[2] = ModelView * gl_in[2].gl_Position;

  // compute the normal in that frame.
  vec3 face_normal = GetNormal();

  // explode the cube
  m_pos[0] = Explode(m_pos[0],face_normal);
  m_pos[1] = Explode(m_pos[1],face_normal);
  m_pos[2] = Explode(m_pos[2],face_normal);

  // do final computation of halfVector for specular lighe
  // and projected position and emmit the vertex.
  // note: normal, ambient and diffuse are emitted too,
  // but are the same for all vertices.
  normal = face_normal;
  diffuse = color * lightDiffuse;
  ambient = color * lightAmbient;

  for(int i = 0; i < 3; i++) {
    halfVector = normalize(lightPos - 2.0 * m_pos[i].xyz);
    m_position = m_pos[i];
    gl_Position = Projection * m_pos[i];
    EmitVertex();
  }

  EndPrimitive();
}
