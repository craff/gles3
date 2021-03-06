in vec4 Position;
out vec4 FragColor;
void  main()
{
  float depth = (Position.z / Position.w + 1.0) / 2.0;
  FragColor=vec4(depth,mod(depth,1./256.)*256.0,mod(depth,1./65536.)*65536.0,1.0);
}
