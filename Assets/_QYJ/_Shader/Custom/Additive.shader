Shader "sgws/Particles/Additive" {
Properties {
    [HDR]_TintColor ("Tint Color", Color) = (0.5,0.5,0.5,0.5)
    _GlowScale("Glow Scale", float) = 1
    _MainTex ("Particle Texture", 2D) = "white" {}
    [Toggle] _UseUvTrail("======UV条带======", Float) = 0
    [Toggle] _UseUvAni("======UV滚动======", Float) = 0
    _SpeedU("SpeedU", Float) = 0
    _SpeedV("SpeedV", Float) = 0
    [Toggle] _UseUvDistortion("======UV扰乱======", Float) = 0
    _UVNoiseTex("NoiseTex", 2D) = "black" {}
    _Distortion ("Distortion", Float) = 0
    _NoiseScroll ("NoiseScroll", Vector) = (0,0,1,1)
    [Toggle] _UseMask("======Mask======", Float) = 0
    [Toggle] _UseMaskUvAni("======MaskUV滚动======", Float) = 0
    _Mask("Mask ( R Channel )", 2D) = "white" {}
    _MaskSpeedU("MaskSpeedU", Float) = 0
    _MaskSpeedV("MaskSpeedV", Float) = 0
    [Toggle] _UseDissolve("======溶解======", Float) = 0
    _DissolveTex ("DissolveTex( R Channel )", 2D) = "white" {}
    _Dissolve ("Dissolve", Range(0, 1.01)) = 0
    _DissolveWidth ("DissolveWidth", Range(0, 1)) = 0
    [Toggle] _UseHardEdge("-------------->硬边溶解", Float) = 0
    [NoScaleOffset]_RampTex ("RampTex", 2D) = "white" {}
    [HDR]_EdgeColor ("Edge Color", Color) = (1,1,1,1)
    _EdgeGlow("Edge Glow", Float) = 1
    [Toggle] _UseUIMaskClip("======UI裁切======", Float) = 0
    _ClipRect("Clip rect",  Vector) = (-1, -1, 1, 1)
    [Toggle] _UseReceiveFog("======接收雾效======", Float) = 0
	[Toggle] _UseVertexMask("======顶点Mask======", Float) = 0
	_VertexOffset ("VertexOffset", Vector) = (0,0,1,1)
	_VertexFlipX ("程序专用", Float) = 1
}

Category {
    Tags { "Queue"="Transparent" "IgnoreProjector"="True" "RenderType"="Transparent" "PreviewType"="Plane" }
    Blend SrcAlpha One
    ColorMask RGB
    Cull Off Lighting Off ZWrite Off

    SubShader {
        Pass {

            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma target 2.0

            #pragma shader_feature _USEMASK_ON
			#pragma shader_feature _USEMASKUVANI_ON
            #pragma shader_feature _USEUVTRAIL_ON
            #pragma shader_feature _USEUVANI_ON
            #pragma shader_feature _USEUVDISTORTION_ON
            #pragma shader_feature _USEDISSOLVE_ON
            #pragma shader_feature _USEHARDEDGE_ON
            #pragma shader_feature _USERECEIVEFOG_ON
            #pragma shader_feature _USEUIMASKCLIP_ON
			#pragma shader_feature _USEVERTEXMASK_ON

            #pragma multi_compile_particles
            #pragma multi_compile_fog

            #include "UnityCG.cginc"
            #include "UnityUI.cginc"

            sampler2D _MainTex;
            fixed4 _TintColor;

            #if _USEMASK_ON
                sampler2D _Mask;
                fixed4 _Mask_ST;
				#if _USEMASKUVANI_ON
				float _MaskSpeedU;
				float _MaskSpeedV;
				#endif
            #endif

            #if _USEUVANI_ON
                //fixed _SpeedU;
                //fixed _SpeedV;
				float _SpeedU;
				float _SpeedV;
            #endif

            #if _USEUVDISTORTION_ON
                sampler2D _UVNoiseTex;
                fixed4 _UVNoiseTex_ST;
                fixed _Distortion;
                fixed4 _NoiseScroll;
            #endif

            #if _USEDISSOLVE_ON
                sampler2D _DissolveTex;
                fixed4 _DissolveTex_ST;
                fixed _Dissolve;
                fixed _DissolveWidth;
            #endif

            #if _USEHARDEDGE_ON
                sampler2D _RampTex;
                fixed4 _EdgeColor;
                fixed _EdgeGlow;
            #endif

            #if _USEUIMASKCLIP_ON
                fixed4 _ClipRect;
            #endif

			#if _USEVERTEXMASK_ON
				fixed4 _VertexOffset;
				fixed _VertexFlipX;
			#endif

            struct appdata_t {
                float4 vertex : POSITION;
                fixed4 color : COLOR;
                fixed4 texcoord : TEXCOORD0;
                fixed2 texcoord1 : TEXCOORD1;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct v2f {
                float4 vertex : SV_POSITION;
                fixed4 color : COLOR;
                fixed4 texcoord : TEXCOORD0;

                fixed2 texcoord1 : TEXCOORD1;

                #if _USEMASK_ON
                    fixed2 texcoordMask : TEXCOORD2;
                #endif

                #if _USEUVDISTORTION_ON
                    fixed2 texcoordNoise : TEXCOORD3;
                #endif

                #if _USEDISSOLVE_ON
                    fixed2 texcoordDissolve: TEXCOORD4;
                #endif


                #if _USEUIMASKCLIP_ON
                    fixed4 worldPosition : TEXCOORD5;
                #endif

                #if _USERECEIVEFOG_ON
                    UNITY_FOG_COORDS(6)
                #endif

				#if _USEVERTEXMASK_ON
					fixed4 vertex2 : TEXCOORD7;
				#endif

                UNITY_VERTEX_OUTPUT_STEREO
            };

            fixed4 _MainTex_ST;
            fixed _GlowScale;

            v2f vert (appdata_t v)
            {
                v2f o;
                UNITY_SETUP_INSTANCE_ID(v);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.color = v.color * _TintColor;
                o.texcoord.xy = TRANSFORM_TEX(v.texcoord,_MainTex);
                o.texcoord.zw = v.texcoord.zw;
                o.texcoord1 = v.texcoord1;

                #if _USEMASK_ON
                    o.texcoordMask = TRANSFORM_TEX(v.texcoord,_Mask);
                #endif

                #if _USEUVDISTORTION_ON
                    o.texcoordNoise = TRANSFORM_TEX(v.texcoord,_UVNoiseTex);
                #endif

                #if _USEDISSOLVE_ON
                    o.texcoordDissolve = TRANSFORM_TEX(v.texcoord,_DissolveTex);
                #endif

                #if _USEUIMASKCLIP_ON
                    o.worldPosition = mul(unity_ObjectToWorld, v.vertex);
                #endif

                #if _USERECEIVEFOG_ON
                    UNITY_TRANSFER_FOG(o,o.vertex);
                #endif

				#if _USEVERTEXMASK_ON
					o.vertex2 = v.vertex;
				#endif

                return o;
            }


            fixed4 frag (v2f i) : SV_Target
            {
                #if _USEUVTRAIL_ON
                    i.texcoord.xy = saturate(i.texcoord.xy+i.texcoord1);
                #else
                    #if _USEUVANI_ON
                        //i.texcoord.xy += _Time.g * fixed2(_SpeedU, _SpeedV);
						// 直接用fixed2 * _Time.g,运行时间长后，会有精度问题，取出来会出像素块，float*fixed数值过大后会导致小数部分精度丢失？（流光移动时卡顿，但不掉帧，移动间隔有明显卡顿，像没有差值的直接两点移动）
						//i.texcoord.xy += fixed2(fmod(_Time.y * _SpeedU, 1.0), fmod(_Time.y * _SpeedV, 1.0));
						/*i.texcoord.xy += fixed2(_Time.y * _SpeedU, _Time.y * _SpeedV);*/
						i.texcoord.xy += _Time.y * float2(_SpeedU, _SpeedV);
                    #endif
                #endif

                #if _USEUVDISTORTION_ON
					float2 noiseScrollXY = _NoiseScroll.xy;
					i.texcoordNoise += fixed2(_Time.g * noiseScrollXY);
                    //i.texcoordNoise += _Time.g * _NoiseScroll.xy;
                    i.texcoord.xy += tex2D(_UVNoiseTex, i.texcoordNoise).xy*_Distortion*_NoiseScroll.zw;
                #endif


                fixed4 col = 2.0f * i.color * tex2D(_MainTex, i.texcoord.xy)*_GlowScale;
                //col.rgb *= i.texcoord.w;
                //col.a = saturate(col.a);


                #if _USEMASK_ON
					#if _USEMASKUVANI_ON
						i.texcoordMask += _Time.y * float2(_MaskSpeedU, _MaskSpeedV);
					#endif
                    col.a = saturate(col.a * tex2D(_Mask, i.texcoordMask).r);
                #endif

                #if _USEDISSOLVE_ON
                    fixed DissolveWithParticle =(_Dissolve*(1-i.texcoord.z)*(1+_DissolveWidth)-_DissolveWidth) ;
                    fixed dissolveAlpha = saturate(smoothstep( DissolveWithParticle, (DissolveWithParticle+_DissolveWidth), tex2D(_DissolveTex, i.texcoordDissolve).r));
                    #if _USEHARDEDGE_ON
                        fixed dissolveEdge = frac(1-dissolveAlpha);
                        col.rgb +=(tex2D(_RampTex,fixed2(dissolveEdge,0.5)).rgb*sign(dissolveEdge))*_EdgeGlow*_EdgeColor.rgb;
                        col.a *= sign(dissolveAlpha)*_EdgeColor.a;
                    #else
                        col.a *= dissolveAlpha;
                    #endif
                #endif

                #if _USERECEIVEFOG_ON
                    UNITY_APPLY_FOG_COLOR(i.fogCoord, col, fixed4(0,0,0,0));
                #endif

                #if _USEUIMASKCLIP_ON
                    col.a *= UnityGet2DClipping(i.worldPosition.xy, _ClipRect);
                #endif

				#if _USEVERTEXMASK_ON
					col.a *= smoothstep(1, 0, ((i.vertex2.x - _VertexOffset.x) * _VertexOffset.z + (i.vertex2.y - _VertexOffset.y) * _VertexOffset.w) * _VertexFlipX);
				#endif

                return col;
            }
            ENDCG
        }
    }
}
}