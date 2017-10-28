#version 330 core
struct Material {
  sampler2D specular;
  sampler2D diffuse;
  float shininess;
};

struct DirLight{
  vec3 direction;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

uniform DirLight dirLight;
vec3 CalcDirLight(DirLight light, vec3 normal, vec3 viewDir);


struct PointLight{
  vec3 position;
  
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;

  float constant;
  float linear;
  float quadratic;
};

const uint NR_POINT_LIGHTS = 4u;
uniform PointLight pointLights[NR_POINT_LIGHTS];

vec3 CalcPointLight(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir);


struct SpotLight{
  vec3 position;
  vec3 direction;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;

  float constant;
  float linear;
  float quadratic;

  float cutOff;
  float outerCutOff;
};
const uint NR_SPOT_LIGHTS=1u;
uniform SpotLight spotLights[NR_SPOT_LIGHTS];
vec3 CalcSpotLight(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir);



out vec4 FragColor;
in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;
uniform Material material;
uniform vec3 viewPos;
void main() {
  vec3 norm = normalize(Normal);
  vec3 viewDir = normalize(viewPos - FragPos);

  // phase 1: Directional lighting
  // vec3 result = CalcDirLight(dirLight, norm, viewDir);
  vec3 result = vec3(0.0,0.0,0.0);

  // phase 2: Point lihgts
  // for(uint i = 0u; i < NR_POINT_LIGHTS; ++i)
  //   result += CalcPointLight(pointLights[i], norm, FragPos, viewDir);

  // phase 3: Spot light
  for(uint i = 0u; i < NR_SPOT_LIGHTS; ++i)
    result += CalcSpotLight(spotLights[i], norm, FragPos, viewDir);

  FragColor = vec4(result,1.0);
}

vec3 CalcDirLight(DirLight light, vec3 normal, vec3 viewDir)
{
  vec3 lightDir = normalize(-light.direction);
  float diff = max(dot(normal,lightDir),0.0);
  vec3 diffuse = diff * light.diffuse * vec3(texture(material.diffuse, TexCoords));
  
  vec3 reflectDir = reflect(-lightDir, normal);
  float spec = pow(max(dot(reflectDir, viewDir),0.0),material.shininess);
  vec3 specular = spec * light.specular * vec3(texture(material.specular, TexCoords));

  vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoords));

  return (ambient + diffuse + specular);
}

vec3 CalcPointLight(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir)
{
  vec3 lightDir = normalize(light.position - fragPos);

  //attenuation
  float distance = length(light.position-fragPos);
  float attenuation = 1.0/(light.constant+light.linear*distance+light.quadratic*(distance*distance));
  
  //diffuse
  float diff= max(0.0,dot(lightDir,normal));
  vec3 diffuse = attenuation * diff * light.diffuse * vec3(texture(material.diffuse, TexCoords));
  
  //specular
  vec3 reflectDir = reflect(-lightDir, normal);
  float spec = max(0.0,dot(reflectDir,viewDir));
  vec3 specular = attenuation * diff * light.specular * vec3(texture(material.specular, TexCoords));

  //ambient
  vec3 ambient = attenuation*light.ambient * vec3(texture(material.diffuse,TexCoords));

  return (ambient+diffuse+specular);
}

vec3 CalcSpotLight(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir)
{
  vec3 lightDir = normalize(light.position - fragPos);

  //attenuation
  float distance = length(light.position-fragPos);
  float attenuation = 1.0/(light.constant+light.linear*distance+light.quadratic*(distance*distance));
  
  //diffuse
  float diff= max(0.0,dot(lightDir,normal));
  vec3 diffuse = attenuation * diff * light.diffuse * vec3(texture(material.diffuse, TexCoords));
  
  //specular
  vec3 reflectDir = reflect(-lightDir, normal);
  float spec = max(0.0,dot(reflectDir,viewDir));
  vec3 specular = attenuation * diff * light.specular * vec3(texture(material.specular, TexCoords));

  //ambient
  vec3 ambient = attenuation*light.ambient * vec3(texture(material.diffuse,TexCoords));

  //intensity
  float theta = dot(lightDir, normalize(-light.direction));
  float epsilon = light.cutOff - light.outerCutOff;
  float intensity = clamp((theta - light.outerCutOff) / epsilon, 0.0, 1.0);

  return (light.ambient * ambient +
	  (light.diffuse * diffuse + light.specular * specular) * intensity) * attenuation;
}
