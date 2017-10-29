
float f(vec3 p) {
  return(p.x*p.x + 9.0 * p.y*p.y + p.z*p.z*p.z*p.z - 1.0);
}

vec3 df(vec3 p) {
  return(vec3(2.0 * p.x, 18.0 * p.y, 4.0 * p.z*p.z*p.z));
}
