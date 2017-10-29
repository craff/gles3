

uniform mat4 InvModelView;

vec3 solve(vec3 eyePos, vec3 pos) {
  vec3 e = (InvModelView * vec4(eyePos,1.)).xyz;
  vec3 v = normalize(pos - e);
  vec3 xa = pos- 0.01 * v;
  float fa = f(xa);
  vec3 xb = xa - f(xa)/dot(df(xa),v) * v;
  float fb = f(xb);
  while(abs(fb) < abs(fa) && dot(xa - xb,xa - xb) > 1e-7) {
    xa = xb; fa = fb;
    xb = xa - fa/dot(df(xa),v) * v;
    fb = f(xb);
  }
  if (dot(xa - xb,xa - xb) < 1e-7) return(xb);
  else discard;
}
