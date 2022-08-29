///关键词DoublePass
///shader比较简单，基于lambert来编写，加了一个简单的菲尼尔，高光则用各向异性的简单计算方法
///几乎完美解决头发半透问题，分享给大家一起学习。支持大家一起拓展效果和功能，本人美术出身，能力有限，不足之处望海涵。
///作者：范太子    时间：2021.12.04      
///联系方式：403570669@qq.com

Shader "TaiziFan/Hair_DoublePass"
{
    Properties
    { 
        _CutOff("CutOff",Range(0,1)) = 0.5
        [Enum(Off,0,Front,1,Back,2)]_CullMode("剔除",INT) = 2
        _MainColor("MainColor",COLOR) = (1,1,1,1)
        _MainTex ("MainTex", 2D) = "white" {}
        _NormalMap("NormalMap",2D) = "bump" {}
        _NormalScale("NormalScale",FLOAT) = 1
        _SpecularTex("SpecularTex",2D) = "white"{}
        [HDR][Gamma]_SpecularColor("SpecularColor",COLOR) = (1,1,1,1)
        _Gloss("Gloss",Range(0.02,1.0)) = 0.6
    }
    SubShader
    {
        
        Tags { 
        "RenderType"="Transparent"
        "Queue"="Transparent"
        }

        ///第一个PASS，给模型“打底”，让不读深度的半透PASS不会产生渲染序列错误
        Pass
        {
            Tags { "LightMode"="UniversalForward" }
            Cull Back
            //Cull [_CullMode]
            CGPROGRAM
            #pragma multi_compile_fwdbase
            #pragma vertex vert
            #pragma fragment frag
            //雾效支持
            #pragma multi_compile_fog
            #pragma multi_compile_fwdadd_fullshadows

            #include "UnityCG.cginc"
            #include "Lighting.cginc"
            #include "AutoLight.cginc"

            fixed _CutOff;
            int _CullMode;
            half4 _MainColor;
            sampler2D _MainTex;
            float4 _MainTex_ST;
            sampler2D _NormalMap;
            half _NormalScale;
            sampler2D _SpecularTex;
            half4 _SpecularColor;
            half _Gloss;

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
                float3 normal : NORMAL;
                float4 tangent : TANGENT;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                //雾效支持
                UNITY_FOG_COORDS(1)
                float4 pos : SV_POSITION;
                float4 w1 : TEXCOORD2;
                float4 w2 : TEXCOORD3;
                float4 w3 : TEXCOORD4;
                //光照探针颜色
                fixed3  SHLighting : COLOR;
                //接收阴影
                SHADOW_COORDS(6)
            };


            v2f vert (appdata v)
            {
                v2f o;
                o.pos = UnityObjectToClipPos(v.vertex);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                float3 worldNormal = UnityObjectToWorldNormal(v.normal);
                float3 worldPos = mul(unity_ObjectToWorld,v.vertex).xyz;
                float3 worldTangent = UnityObjectToWorldDir(v.tangent.xyz);
                float3 worldBinormal = cross(worldNormal,worldTangent)*v.tangent.w;

                o.w1 = float4(worldTangent.x,worldBinormal.x,worldNormal.x,worldPos.x);
                o.w2 = float4(worldTangent.y,worldBinormal.y,worldNormal.y,worldPos.y);
                o.w3 = float4(worldTangent.z,worldBinormal.z,worldNormal.z,worldPos.z);
                o.SHLighting= ShadeSH9(float4(worldNormal,1)) ;
                //雾效支持
                UNITY_TRANSFER_FOG(o,o.vertex);
                //接收阴影
                TRANSFER_SHADOW(o);
                return o;
            }

            fixed4 frag (v2f i ,float facing : VFACE) : SV_Target
            {
                //如果场景已渲染光照图，全局启用LightProbe光照探针
                #ifndef LIGHTMAP_ON
                    fixed3 ambient = i.SHLighting;
                    #else  
                    fixed3 ambient = UNITY_LIGHTMODEL_AMBIENT.xyz;
                #endif
                
                float3 worldPos = float3(i.w1.w,i.w2.w,i.w3.w);
                fixed3 lightDir = normalize(UnityWorldSpaceLightDir(worldPos));
                fixed3 viewDir = normalize(UnityWorldSpaceViewDir(worldPos));
                fixed4 albedo = tex2D(_MainTex, i.uv)*_MainColor;
                //计算法线
                float3 normal = UnpackScaleNormal(tex2D(_NormalMap,i.uv),_NormalScale);
                normal = normalize(half3(dot(i.w1.xyz,normal),dot(i.w2.xyz,normal),dot(i.w3.xyz,normal))); 

                float isFrontFace = ( facing >= 0 ? 1 : 0 );
                float faceSign = ( facing >= 0 ? 1 : -1 );
                normal *= faceSign;

                //接收阴影
                UNITY_LIGHT_ATTENUATION(atten,i,worldPos);
                
                //漫反射
                fixed3 lamb = (max(dot(normal,lightDir),0)*_LightColor0.rgb*atten+ambient)*albedo.rgb;
                
                //菲尼尔反射
                fixed3 finel = pow(saturate(1-((dot(viewDir,normal))*0.6+0.6)),2)*_LightColor0.rgb*atten*ambient*2;

                //各向异性高光
                //float3 worldBinormal = WorldNormalVector( i, half3( 0, 1, 0 ) );
                float3 worldBinormal = normalize(float3(i.w1.y,i.w2.y,i.w3.y));
                half4 spec = tex2D(_SpecularTex,i.uv);
                half clampSpec = clamp(spec.r,-1.0,1.33);
                half dotSpec = 1.0-pow(dot((clampSpec*normal+worldBinormal),viewDir),2);
                half3 powSpec = max(pow(sqrt(sqrt(dotSpec)),_Gloss*1000)*_LightColor0.rgb*ambient*atten*_SpecularColor.rgb*albedo*2,half3(0,0,0));

                fixed3 col = lamb+finel+powSpec;
                fixed4 finnalCol = fixed4(col.rgb,albedo.a);

                clip(albedo.a-_CutOff);
                //雾效支持
                UNITY_APPLY_FOG(i.fogCoord, col);
                return finnalCol;
            }
            ENDCG
        }

        // ///第二个PASS处理Transparent半透部分
        // Pass
        // {
        //     Tags { "LightMode"="UniversalForward" }
        //     Blend SrcAlpha OneMinusSrcAlpha
        //     Cull [_CullMode]
        //     ZWrite Off
        //     CGPROGRAM
        //     #pragma multi_compile_fwdbase
        //     #pragma vertex vert
        //     #pragma fragment frag
        //     //雾效支持
        //     #pragma multi_compile_fog
        //     #pragma multi_compile_fwdadd_fullshadows

        //     #include "UnityCG.cginc"
        //     #include "Lighting.cginc"
        //     #include "AutoLight.cginc"

        //     fixed _CutOff;
        //     int _CullMode;
        //     half4 _MainColor;
        //     sampler2D _MainTex;
        //     float4 _MainTex_ST;
        //     sampler2D _NormalMap;
        //     half _NormalScale;
        //     sampler2D _SpecularTex;
        //     half4 _SpecularColor;
        //     half _Gloss;

        //     struct appdata
        //     {
        //         float4 vertex : POSITION;
        //         float2 uv : TEXCOORD0;
        //         float3 normal : NORMAL;
        //         float4 tangent : TANGENT;
        //     };

        //     struct v2f
        //     {
        //         float2 uv : TEXCOORD0;
        //         //雾效支持
        //         UNITY_FOG_COORDS(1)
        //         float4 pos : SV_POSITION;
        //         float4 w1 : TEXCOORD2;
        //         float4 w2 : TEXCOORD3;
        //         float4 w3 : TEXCOORD4;
        //         //光照探针颜色
        //         fixed3  SHLighting : COLOR;
        //         //接收阴影
        //         SHADOW_COORDS(6)
        //     };


        //     v2f vert (appdata v)
        //     {
        //         v2f o;
        //         o.pos = UnityObjectToClipPos(v.vertex);
        //         o.uv = TRANSFORM_TEX(v.uv, _MainTex);
        //         float3 worldNormal = UnityObjectToWorldNormal(v.normal);
        //         float3 worldPos = mul(unity_ObjectToWorld,v.vertex).xyz;
        //         float3 worldTangent = UnityObjectToWorldDir(v.tangent.xyz);
        //         float3 worldBinormal = cross(worldNormal,worldTangent)*v.tangent.w;

        //         o.w1 = float4(worldTangent.x,worldBinormal.x,worldNormal.x,worldPos.x);
        //         o.w2 = float4(worldTangent.y,worldBinormal.y,worldNormal.y,worldPos.y);
        //         o.w3 = float4(worldTangent.z,worldBinormal.z,worldNormal.z,worldPos.z);
        //         o.SHLighting= ShadeSH9(float4(worldNormal,1)) ;
        //         //雾效支持
        //         UNITY_TRANSFER_FOG(o,o.vertex);
        //         //接收阴影
        //         TRANSFER_SHADOW(o);
        //         return o;
        //     }

        //     fixed4 frag (v2f i ,float facing : VFACE) : SV_Target
        //     {
        //         //如果场景已渲染光照图，全局启用LightProbe光照探针
        //         #ifndef LIGHTMAP_ON
        //             fixed3 ambient = i.SHLighting;
        //             #else  
        //             fixed3 ambient = UNITY_LIGHTMODEL_AMBIENT.xyz;
        //         #endif
                
        //         float3 worldPos = float3(i.w1.w,i.w2.w,i.w3.w);
        //         fixed3 lightDir = normalize(UnityWorldSpaceLightDir(worldPos));
        //         fixed3 viewDir = normalize(UnityWorldSpaceViewDir(worldPos));
        //         fixed4 albedo = tex2D(_MainTex, i.uv)*_MainColor;
        //         //计算法线
        //         float3 normal = UnpackScaleNormal(tex2D(_NormalMap,i.uv),_NormalScale);
        //         normal = normalize(half3(dot(i.w1.xyz,normal),dot(i.w2.xyz,normal),dot(i.w3.xyz,normal))); 

        //         float isFrontFace = ( facing >= 0 ? 1 : 0 );
        //         float faceSign = ( facing >= 0 ? 1 : -1 );
        //         normal *= faceSign;

        //         //接收阴影
        //         UNITY_LIGHT_ATTENUATION(atten,i,worldPos);
                
        //         //漫反射
        //         fixed3 lamb = (max(dot(normal,lightDir),0)*_LightColor0.rgb*atten+ambient)*albedo.rgb;
                
        //         //菲尼尔反射
        //         fixed3 finel = pow(saturate(1-((dot(viewDir,normal))*0.6+0.6)),2)*_LightColor0.rgb*atten*ambient*2;

        //         //各向异性高光
        //         //float3 worldBinormal = WorldNormalVector( i, half3( 0, 1, 0 ) );
        //         float3 worldBinormal = normalize(float3(i.w1.y,i.w2.y,i.w3.y));
        //         half4 spec = tex2D(_SpecularTex,i.uv);
        //         half clampSpec = clamp(spec.r,-1.0,1.33);
        //         half dotSpec = 1.0-pow(dot((clampSpec*normal+worldBinormal),viewDir),2);
        //         half3 powSpec = max(pow(sqrt(sqrt(dotSpec)),_Gloss*1000)*_LightColor0.rgb*ambient*atten*_SpecularColor.rgb*albedo*2,half3(0,0,0));

        //         fixed3 col = lamb+finel+powSpec;
        //         fixed4 finnalCol = fixed4(col.rgb,albedo.a);

        //         //clip(albedo.a-_CutOff);
        //         //雾效支持
        //         UNITY_APPLY_FOG(i.fogCoord, col);
        //         return finnalCol;
        //     }
        //     ENDCG
        // }

        // ///处理多光源支持PASS，移动端可以干掉
        // Pass
        // {
        //     Name "FORWARD_DELTA"
        //     Tags { "LightMode"="ForwardAdd" }
        //     Blend One One
        //     Cull [_CullMode]
        //     CGPROGRAM
        //     #pragma multi_compile_fwdadd
        //     #pragma vertex vert
        //     #pragma fragment frag
        //     //雾效支持
        //     #pragma multi_compile_fog

        //     #include "UnityCG.cginc"
        //     #include "Lighting.cginc"
        //     #include "AutoLight.cginc"

        //     fixed _CutOff;
        //     int _CullMode;
        //     half4 _MainColor;
        //     sampler2D _MainTex;
        //     float4 _MainTex_ST;
        //     sampler2D _NormalMap;
        //     half _NormalScale;
        //     sampler2D _SpecularTex;
        //     half4 _SpecularColor;
        //     half _Gloss;

        //     struct appdata
        //     {
        //         float4 vertex : POSITION;
        //         float2 uv : TEXCOORD0;
        //         float3 normal : NORMAL;
        //         float4 tangent : TANGENT;
        //     };

        //     struct v2f
        //     {
        //         float2 uv : TEXCOORD0;
        //         //雾效支持
        //         UNITY_FOG_COORDS(1)
        //         float4 pos : SV_POSITION;
        //         float4 w1 : TEXCOORD2;
        //         float4 w2 : TEXCOORD3;
        //         float4 w3 : TEXCOORD4;
        //         //光照探针颜色
        //         fixed3  SHLighting : COLOR;
        //     };


        //     v2f vert (appdata v)
        //     {
        //         v2f o;
        //         o.pos = UnityObjectToClipPos(v.vertex);
        //         o.uv = TRANSFORM_TEX(v.uv, _MainTex);
        //         float3 worldNormal = UnityObjectToWorldNormal(v.normal);
        //         float3 worldPos = mul(unity_ObjectToWorld,v.vertex).xyz;
        //         float3 worldTangent = UnityObjectToWorldDir(v.tangent.xyz);
        //         float3 worldBinormal = cross(worldNormal,worldTangent)*v.tangent.w;

        //         o.w1 = float4(worldTangent.x,worldBinormal.x,worldNormal.x,worldPos.x);
        //         o.w2 = float4(worldTangent.y,worldBinormal.y,worldNormal.y,worldPos.y);
        //         o.w3 = float4(worldTangent.z,worldBinormal.z,worldNormal.z,worldPos.z);
        //         o.SHLighting= ShadeSH9(float4(worldNormal,1)) ;
        //         //雾效支持
        //         UNITY_TRANSFER_FOG(o,o.vertex);
        //         return o;
        //     }

        //     fixed4 frag (v2f i ,float facing : VFACE) : SV_Target
        //     {
                
        //         float3 worldPos = float3(i.w1.w,i.w2.w,i.w3.w);

        //         #ifdef USING_DIRECTIONAL_LIGHT
        //             fixed3 lightDir = normalize(_WorldSpaceLightPos0.xyz);
        //         #else
        //             fixed3 lightDir = normalize(_WorldSpaceLightPos0.xyz - worldPos.xyz);
        //         #endif

        //         fixed3 viewDir = normalize(UnityWorldSpaceViewDir(worldPos));
        //         fixed4 albedo = tex2D(_MainTex, i.uv)*_MainColor;
        //         //计算法线
        //         float3 normal = UnpackScaleNormal(tex2D(_NormalMap,i.uv),_NormalScale);
        //         normal = normalize(half3(dot(i.w1.xyz,normal),dot(i.w2.xyz,normal),dot(i.w3.xyz,normal))); 

        //         float isFrontFace = ( facing >= 0 ? 1 : 0 );
        //         float faceSign = ( facing >= 0 ? 1 : -1 );
        //         normal *= faceSign;

        //         #ifdef USING_DIRECTIONAL_LIGHT
		// 			fixed atten = 1.0;
		// 		#else
		// 			#if defined (POINT)
		// 		        float3 lightCoord = mul(unity_WorldToLight, float4(worldPos, 1)).xyz;
		// 		        fixed atten = tex2D(_LightTexture0, dot(lightCoord, lightCoord).rr).UNITY_ATTEN_CHANNEL;
		// 		    #elif defined (SPOT)
		// 		        float4 lightCoord = mul(unity_WorldToLight, float4(worldPos, 1));
		// 		        fixed atten = (lightCoord.z > 0) * tex2D(_LightTexture0, lightCoord.xy / lightCoord.w + 0.5).w * tex2D(_LightTextureB0, dot(lightCoord, lightCoord).rr).UNITY_ATTEN_CHANNEL;
		// 		    #else
		// 		        fixed atten = 1.0;
		// 		    #endif
		// 		#endif

        //         //漫反射
        //         fixed3 lamb = (max(dot(normal,lightDir),0)*_LightColor0.rgb*atten)*albedo.rgb;
                

        //         //各向异性高光
        //         //float3 worldBinormal = WorldNormalVector( i, half3( 0, 1, 0 ) );
        //         float3 worldBinormal = normalize(float3(i.w1.y,i.w2.y,i.w3.y));
        //         half4 spec = tex2D(_SpecularTex,i.uv);
        //         half clampSpec = clamp(spec.r,-1.0,1.33);
        //         half dotSpec = 1.0-pow(dot((clampSpec*normal+worldBinormal),viewDir),2);
        //         half3 powSpec = max(pow(sqrt(sqrt(dotSpec)),_Gloss*1000)*_LightColor0.rgb*atten*_SpecularColor.rgb*albedo,half3(0,0,0));

        //         fixed3 col = lamb+powSpec;
        //         fixed4 finnalCol = fixed4(col.rgb,albedo.a);

        //         clip(albedo.a-_CutOff);
        //         //雾效支持
        //         UNITY_APPLY_FOG(i.fogCoord, col);
        //         return finnalCol;
        //     }
        //     ENDCG
        // }

        // //投射阴影PASS，移动端想干掉也可以干掉，投影有其它方案代替
        // Pass
        // {
        //     Name "ShadowCaster"
        //     Tags {"LightMode" = "ShadowCaster"}
        //     Cull off
        //     CGPROGRAM
        //     #pragma vertex vert
        //     #pragma fragment frag
        //     #pragma multi_compile_shadowcaster
        //     #include "UnityCG.cginc"

        //     half _CutOff;
        //     sampler2D _MainTex;
        //     float4 _MainTex_ST;

        //     struct appdata
        //     {
        //         float4 vertex : POSITION;
        //         float2 uv : TEXCOORD0;

        //     };
        //     struct v2f
        //     {
        //         V2F_SHADOW_CASTER;
        //         float2 uv : TEXCOORD1;
        //     };
        //     v2f vert(appdata v)
        //     {
        //         v2f o;
        //         o.uv = TRANSFORM_TEX(v.uv,_MainTex);
        //         TRANSFER_SHADOW_CASTER(o)
        //         return o;
        //     }
        //     float4 frag(v2f i) : SV_Target{
        //         fixed4 mainTex = tex2D(_MainTex,i.uv);
        //         clip(mainTex.a-_CutOff);
        //         SHADOW_CASTER_FRAGMENT(i)
        //     }
        //     ENDCG
        // }
    }
}
