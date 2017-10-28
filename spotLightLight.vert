#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 pvm;
out vec3 Pos;

void main()
{
  gl_Position = pvm*vec4(aPos,1.0);
}
