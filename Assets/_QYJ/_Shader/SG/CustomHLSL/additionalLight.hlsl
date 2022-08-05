#ifndef ADDITIONALLIGHT_INCLUDE
#define ADDITIONALLIGHT_INCLUDE

//half3 ShadeAdditionalLights(float3 WorldPos, float3 WorldNormal, float3 WorldViewDir, float Roughness, float3 direction, float3 attenuatedLightColor)
//{ 
//    half3 normalWS = WorldNormal;
//    half3 lightDirWS = direction;
//    half3 viewDirWS = viewDirWS;
//    half3 halfView = normalize(lightDirWS + viewDirWS); 
    
//    half NdotL = dot(normalWS, lightDirWS);
    
//    return attenuatedLightColor;

//}

void AdditionalLights_float(float3 WorldPos, out float3 LightColor)
{
    //在节点预览器中的显示
    #ifdef SHADERGRAPH_PREVIEW
        LightColor = 1;
        
    #else  
        #ifdef _ADDITIONAL_LIGHTS
        //half3 additionalLightsResult = 0;
        half3 attenuatedLightColor = 0;
        int additionalLightsCount = GetAdditionalLightsCount();
        for (int i = 0; i < additionalLightsCount; i++)
        {
        
            int perObjectLightIndex = GetPerObjectLightIndex(i);
            Light light = GetAdditionalPerObjectLight(perObjectLightIndex, WorldPos);
            attenuatedLightColor += light.color * light.distanceAttenuation * light.shadowAttenuation;
            //additionalLightsResult += ShadeAdditionalLights(WorldPos, WorldNormal, WorldViewDir, Roughness,light.direction, attenuatedLightColor);
        }
        LightColor = attenuatedLightColor;
    
        #else
            LightColor = 0;
        #endif
        
    #endif
}






#endif