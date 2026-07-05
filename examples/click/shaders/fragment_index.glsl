flat in uint v_face;
in vec3 w_position;
out vec4 fragColor;

void main(){
     fragColor = vec4(w_position, float(v_face));
}
