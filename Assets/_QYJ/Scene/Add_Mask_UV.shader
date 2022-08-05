Shader "X/SFX/Add_Mask_UV" {
    Properties {
        _TintColor ("Color", Color) = (0.5,0.5,0.5,0.5)
        _MainTex ("MainTex", 2D) = "white" {}
        _MainUSpeed ("MainUSpeed", Range(-1, 1)) = 0
        _MainVSpeed ("MainVSpeed", Range(-1, 1)) = 0
        _Mask ("Mask", 2D) = "white" {}
    }
    SubShader {
        Tags {"IgnoreProjector"="True" "Queue"="Transparent" "RenderType"="Transparent"}
        Pass {
            Name "FORWARD"
            Tags {"LightMode"="ForwardBase"}
            Blend SrcAlpha One
            ZWrite Off
            
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "UnityCG.cginc"

            sampler2D _MainTex; float4 _MainTex_ST;
            sampler2D _Mask; float4 _Mask_ST;
            half4 _TintColor;
            float _MainUSpeed,_MainVSpeed;

            struct a2v {
                float4 vertex : POSITION;
                float2 texcoord0 : TEXCOORD0;
                half4 vertexColor : COLOR;
            };
            struct v2f {
                float4 pos : SV_POSITION;
                float2 uv0 : TEXCOORD0;
                half4 vertexColor : COLOR;
            };
            v2f vert (a2v v) {
                v2f o;
                o.uv0 = v.texcoord0;
                o.vertexColor = v.vertexColor;
                o.pos = UnityObjectToClipPos( v.vertex );
                return o;
            }
            float4 frag(v2f i) : SV_Target {
                float2 MainUV = (i.uv0+(float2(_MainUSpeed,_MainVSpeed)*_Time.g));
                half4 _MainTex_var = tex2D(_MainTex,TRANSFORM_TEX(MainUV, _MainTex));
                half4 _Mask_var = tex2D(_Mask,TRANSFORM_TEX(i.uv0, _Mask));
                float3 finalColor = (((_MainTex_var.rgb*_MainTex_var.a)*(_TintColor.rgb*_TintColor.a)*4.0)*(i.vertexColor.rgb*i.vertexColor.a)*(_Mask_var.rgb*_Mask_var.a));
                half4 finalRGBA = fixed4(finalColor,1);
                return finalRGBA;
            }
            ENDCG
        }
    }
}
