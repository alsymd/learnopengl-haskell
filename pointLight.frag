#version 330 core
struct Material{
  sampler2D specular;
  sampler2D diffuse;
  float shininess;
};

struct Light{
  vec3 position;
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;

  float constant;
  float linear;
  float quadratic;
};


out vec4 FragColor;
in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;
uniform Material material;
uniform Light light;
uniform vec3 viewPos;
void main()
{
  vec3 direction = normalize(light.position-FragPos);
  
  // ambient
  vec3 ambient = light.ambient * vec3(texture(material.diffuse,TexCoords));
  

  // diffuse
  vec3 norm = normalize(Normal);
  float diff = max(dot(norm,direction),0);
  vec3 diffuse = (diff*vec3(texture(material.diffuse,TexCoords)))*light.diffuse;

  // specular
  vec3 viewDir = normalize(viewPos - FragPos);
  vec3 reflectDir = reflect(-direction, norm);
  float spec = pow(max(dot(viewDir,reflectDir),0),material.shininess);
  vec3 specular = vec3(texture(material.specular,TexCoords)) * spec * light.specular;

  // attenuation
  float distance = length(light.position-FragPos);
  float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic*(distance*distance));
  
  FragColor = vec4((light.ambient*ambient+light.diffuse*diffuse+light.specular*specular)*attenuation, 1.0);

}
