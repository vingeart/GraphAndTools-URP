///关键词AlphaToMask On
///优点：使用一个PASS就可以实现比较不错的头发半透效果，且解决Transparent的渲染排序错误问题，结构简单，拓展方便。
///缺点：①必须打开摄像机抗锯齿MSAA，否则就是CutOut效果。移动端开启MSAA比较费，亲测在一部分手机上会出问题（只有CutOut效果）。
///      ②边缘部分有瑕疵，半透与不透之间的过渡很生硬，无法用于渐变半透的布料。
///由于兼容性不是特别好，不推荐使用。所以我就懒得做高光，多光源等计算。
///作者：范太子    时间：2021.12.03

Shader "TaiziFan/Hair_AlphaToMask"
{
    Properties
    { 
        _CutOff("CutOff",Range(0,1)) = 0.5
        [Enum(Off,0,Front,1,Back,2)]_CullMode("剔除",INT) = 2
        _MainColor("MainColor",COLOR) = (1,1,1,1)
        _MainTex ("MainTex", 2D) = "white" {}
        _NormalMap("NormalMap",2D) = "bump" {}
        _NormalScale("NormalScale",FLOAT) = 1
    }
    SubShader
    {
        
        Tags { 
        "RenderType"="TransparentCutout"
        "Queue"="AlphaTest"
        }

        Pass
        {
            Tags { "LightMode"="UniversalForward" }

            Cull [_CullMode]
            AlphaToMask On
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

                //计算法线
                float3 normal = UnpackScaleNormal(tex2D(_NormalMap,i.uv),_NormalScale);
                normal = normalize(half3(dot(i.w1.xyz,normal),dot(i.w2.xyz,normal),dot(i.w3.xyz,normal))); 

                float isFrontFace = ( facing >= 0 ? 1 : 0 );
                float faceSign = ( facing >= 0 ? 1 : -1 );
                normal *= faceSign;
                //接收阴影
                UNITY_LIGHT_ATTENUATION(atten,i,worldPos);

                fixed4 albedo = tex2D(_MainTex, i.uv)*_MainColor;
                
                //漫反射
                fixed3 lamb = (max(dot(normal,lightDir),0)*_LightColor0.rgb*atten+ambient)*albedo.rgb;
                
                fixed3 col = lamb;
                fixed4 finnalCol = fixed4(col.rgb,albedo.a);

                clip(albedo.a-_CutOff);
                //雾效支持
                UNITY_APPLY_FOG(i.fogCoord, col);
                return finnalCol;
            }
            ENDCG
        }


        //投射阴影PASS
        Pass
        {
            Name "ShadowCaster"
            Tags {"LightMode" = "ShadowCaster"}
            Cull off
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_shadowcaster
            #include "UnityCG.cginc"

            half _CutOff;
            sampler2D _MainTex;
            float4 _MainTex_ST;

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;

            };
            struct v2f
            {
                V2F_SHADOW_CASTER;
                float2 uv : TEXCOORD1;
            };
            v2f vert(appdata v)
            {
                v2f o;
                o.uv = TRANSFORM_TEX(v.uv,_MainTex);
                TRANSFER_SHADOW_CASTER(o)
                return o;
            }
            float4 frag(v2f i) : SV_Target{
                fixed4 mainTex = tex2D(_MainTex,i.uv);
                clip(mainTex.a-_CutOff);
                SHADOW_CASTER_FRAGMENT(i)
            }
            ENDCG
        }

    }
}
