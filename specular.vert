#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
out vec3 Normal;
out vec3 FragPos;
uniform mat4 pvm;


void main()
{
  Normal = aNormal;
  gl_Position = pvm * vec4(aPos, 1.0);
  FragPos = aPos;
} 
