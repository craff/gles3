

uniform mat4 InvModelView;

vec3 solve(vec3 eyePos, vec3 pos) {
  vec3 e = (InvModelView * vec4(eyePos,1.)).xyz;
  vec3 v = pos - e;
  float pv = dot(pos,v);
  float v2 = dot(v,v);
  float delta = pv*pv - (dot(pos,pos) - 1.0)*v2;
  if (delta <= 0.0) discard;
  float l = (pv - sqrt(delta))/v2;
  vec3 x = pos - l * v;
  return(x);
}
