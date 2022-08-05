Shader "MyURP/Outline"
{
    
    Properties
    {
    
        _OutlineColor("OutlineColor", Color) = (0,0,0,1)
        _OutLineWidth("OutLineWidth", Range(0, 1)) = 0.005
        _ShowRenderQueue("ShowRenderQueue",Int)= -1
    }
    SubShader
    {
    
        Pass
        {
    
    	    Name "OUTLINE"
            Tags{
    "LightMode" = "SRPDefaultUnlit"}

    	    Cull Front
    	    Zwrite On
    	    HLSLPROGRAM
    	    #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #pragma target 3.0
        
            struct a2v
            {
    
                float4 vertex       : POSITION;
                float3 normal       : NORMAL;
            };

    	    struct v2f
    	    {
    
   	            float4 pos : SV_POSITION;
    	    };
        
            CBUFFER_START(UnityPerMaterial)
    	        float4 _OutlineColor;
    	        float _OutLineWidth;
                int _ShowRenderQueue;
            CBUFFER_END

    	    v2f vert (a2v v)
    	    {
    
                v2f o;
                v.vertex.xyz += _OutLineWidth * v.normal;
                o.pos = TransformObjectToHClip(v.vertex.xyz);
                return o;
    	    }
    	    half4 frag (v2f i) : SV_Target
    	    {
    
    	        return _OutlineColor;
    	    }
    	    ENDHLSL
        }
    }
}