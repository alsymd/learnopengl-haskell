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
};


out vec4 FragColor;
in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;
uniform Material material;
uniform vec3 lightColor;
uniform Light light;
uniform vec3 viewPos;
void main()
{
  // ambient
  vec3 ambient = light.diffuse * vec3(texture(material.diffuse,TexCoords));
  

  // diffuse
  vec3 norm = normalize(Normal);
  vec3 lightDir = normalize(light.position-FragPos);
  float diff = max(dot(norm,lightDir),0);
  vec3 diffuse = (diff*vec3(texture(material.diffuse,TexCoords)))*lightColor;

  // specular
  vec3 viewDir = normalize(viewPos - FragPos);
  vec3 reflectDir = reflect(-lightDir, norm);
  float spec = pow(max(dot(viewDir,reflectDir),0),material.shininess);
  vec3 specular = vec3(texture(material.specular,TexCoords)) * spec * lightColor;

  // FragColor = vec4(light.ambient*ambient+light.diffuse*diffuse+light.specular*specular, 1.0);
  // FragColor = texture(material.diffuse,TexCoords);
  FragColor = vec4(light.ambient*ambient+light.diffuse*diffuse+light.specular*specular, 1.0);
}
