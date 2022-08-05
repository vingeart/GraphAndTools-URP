// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SFX/ASE/Dissolve/DissolveDefault"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][Enum(Add,1,Blend,10)]_BlendMode("Blend Mode", Float) = 1
		_DepthBlend("Depth Blend", Range( 0 , 2)) = 0
		_MainTex("Main Tex", 2D) = "white" {}
		[HDR]_MainTexColor("Main Tex Color", Color) = (1,1,1,1)
		_MainTexRotation("Main Tex Rotation", Float) = 0
		[Toggle(_MAINTEXUVAUTOSPEED_ON)] _MainTexUVAutoSpeed("Main TexUVAutoSpeed", Float) = 0
		_MainTexUSpeed("Main Tex USpeed", Float) = 0
		_MainTexVSpeed("Main Tex VSpeed", Float) = 0
		[Toggle(_MAINTEXDISTORT_ON)] _MainTexDistort("Main Tex Distort", Float) = 0
		_MainTexDistPower("Main Tex DistPower", Range( 0 , 1)) = 0
		_MaskTex("Mask Tex", 2D) = "white" {}
		_MaskTexRGBA("Mask Tex RGBA", Vector) = (1,0,0,0)
		_MaskTexRotation("Mask Tex Rotation", Float) = 0
		[Toggle(_MASKTEXUVAUTOSPEED_ON)] _MaskTexUVAutoSpeed("Mask TexUVAutoSpeed", Float) = 0
		_MaskTexUSpeed("Mask TexUSpeed", Float) = 0
		_MaskTexVSpeed("Mask TexVSpeed", Float) = 0
		_MaskDistort("Mask Distort", Range( 0 , 1)) = 0
		_DissTex("Diss Tex", 2D) = "white" {}
		_DissTexRGBA("Diss Tex RGBA", Vector) = (1,0,0,0)
		[HDR]_DissEdgeColor("Diss EdgeColor", Color) = (1,1,1,1)
		_DissTexRotation("Diss Tex Rotation", Float) = 0
		_DissTexUSpeed("Diss TexUSpeed", Float) = 0
		_DissTexVSpeed("Diss TexVSpeed", Float) = 0
		[Toggle(_DISSSLIDERSWITCH_ON)] _DissSliderSwitch("Diss Slider Switch", Float) = 0
		_Disspower("Diss power", Range( 0 , 1)) = 0
		_DissEdgeWidth("Diss Edge Width", Range( 0 , 1)) = 0
		_DissHardness("Diss Hardness", Range( 0 , 1)) = 0
		_DissTexDistort("Diss Tex Distort", Range( 0 , 1)) = 0
		_DistortTex("Distort Tex", 2D) = "white" {}
		_DistortTexRGBA("Distort Tex RGBA", Vector) = (0,0,0,1)
		_DistortTexRotation("Distort Tex Rotation", Float) = 0
		_DistortTexUSpeed("Distort TexUSpeed", Float) = 0
		[ASEEnd]_DistortTexVSpeed("Distort TexVSpeed", Float) = 0

		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }
		
		Cull Off
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 2.0

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend SrcAlpha [_BlendMode], One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#pragma multi_compile_instancing

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"


			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _MAINTEXDISTORT_ON
			#pragma shader_feature_local _MAINTEXUVAUTOSPEED_ON
			#pragma shader_feature_local _DISSSLIDERSWITCH_ON
			#pragma shader_feature_local _MASKTEXUVAUTOSPEED_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#ifdef ASE_FOG
				float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_color : COLOR;
				float4 ase_texcoord6 : TEXCOORD6;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _DissTex_ST;
			float4 _DissEdgeColor;
			float4 _MaskTex_ST;
			half4 _MaskTexRGBA;
			float4 _DistortTex_ST;
			float4 _DistortTexRGBA;
			half4 _MainTexColor;
			half4 _DissTexRGBA;
			float4 _MainTex_ST;
			float _Disspower;
			float _MaskDistort;
			float _DissHardness;
			half _MaskTexRotation;
			float _MaskTexUSpeed;
			float _MaskTexVSpeed;
			float _DissEdgeWidth;
			float _BlendMode;
			half _DissTexRotation;
			float _DissTexUSpeed;
			float _DissTexDistort;
			float _MainTexVSpeed;
			float _MainTexUSpeed;
			half _MainTexRotation;
			float _MainTexDistPower;
			float _DistortTexVSpeed;
			float _DistortTexUSpeed;
			half _DistortTexRotation;
			float _DissTexVSpeed;
			float _DepthBlend;
			CBUFFER_END
			sampler2D _MainTex;
			sampler2D _DistortTex;
			sampler2D _DissTex;
			sampler2D _MaskTex;


						
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);

				float4 texCoord8 = v.ase_texcoord1;
				texCoord8.xy = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult141 = (float3(texCoord8.x , texCoord8.y , texCoord8.z));
				float3 appendResult144 = (float3(_Disspower , _DissEdgeWidth , _DissHardness));
				#ifdef _DISSSLIDERSWITCH_ON
				float3 staticSwitch143 = appendResult144;
				#else
				float3 staticSwitch143 = appendResult141;
				#endif
				float3 break147 = staticSwitch143;
				float temp_output_22_0_g1790 = break147.y;
				float temp_output_13_0_g1790 = ( break147.x * ( 1.0 + temp_output_22_0_g1790 ) );
				float Hardness13 = break147.z;
				float temp_output_14_0_g1790 = ( 1.0 - Hardness13 );
				float vertexToFrag24_g1790 = ( temp_output_13_0_g1790 * ( 1.0 + temp_output_14_0_g1790 ) );
				o.ase_texcoord3.z = vertexToFrag24_g1790;
				
				float vertexToFrag25_g1790 = ( ( temp_output_13_0_g1790 - temp_output_22_0_g1790 ) * ( 1.0 + temp_output_14_0_g1790 ) );
				o.ase_texcoord3.w = vertexToFrag25_g1790;
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord6 = screenPos;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				o.ase_texcoord4 = v.ase_texcoord1;
				o.ase_texcoord5 = v.ase_texcoord2;
				o.ase_color = v.ase_color;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float2 texCoord9 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_19_0_g1786 = texCoord9;
				float2 temp_cast_0 = (_DistortTexRotation).xx;
				float cos2_g1787 = cos( radians( temp_cast_0.x ) );
				float sin2_g1787 = sin( radians( temp_cast_0.x ) );
				float2 rotator2_g1787 = mul( temp_output_19_0_g1786 - half2( 0.5,0.5 ) , float2x2( cos2_g1787 , -sin2_g1787 , sin2_g1787 , cos2_g1787 )) + half2( 0.5,0.5 );
				float2 appendResult57 = (float2(_DistortTexUSpeed , _DistortTexVSpeed));
				float4 tex2DNode24_g1786 = tex2D( _DistortTex, ( ( ( rotator2_g1787 + ( appendResult57 * ( _TimeParameters.x ) ) ) * _DistortTex_ST.xy ) + _DistortTex_ST.zw ) );
				float dotResult31_g1786 = dot( tex2DNode24_g1786 , _DistortTexRGBA );
				float Distort56 = dotResult31_g1786;
				float2 temp_cast_3 = (Distort56).xx;
				float4 texCoord8 = IN.ase_texcoord4;
				texCoord8.xy = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				#ifdef _MAINTEXDISTORT_ON
				float staticSwitch80 = _MainTexDistPower;
				#else
				float staticSwitch80 = texCoord8.w;
				#endif
				float2 lerpResult77 = lerp( texCoord9 , temp_cast_3 , staticSwitch80);
				float2 temp_output_19_0_g1791 = lerpResult77;
				float2 temp_cast_4 = (_MainTexRotation).xx;
				float cos2_g1792 = cos( radians( temp_cast_4.x ) );
				float sin2_g1792 = sin( radians( temp_cast_4.x ) );
				float2 rotator2_g1792 = mul( temp_output_19_0_g1791 - half2( 0.5,0.5 ) , float2x2( cos2_g1792 , -sin2_g1792 , sin2_g1792 , cos2_g1792 )) + half2( 0.5,0.5 );
				float4 texCoord74 = IN.ase_texcoord5;
				texCoord74.xy = IN.ase_texcoord5.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult76 = (float2(texCoord74.x , texCoord74.y));
				float2 appendResult66 = (float2(_MainTexUSpeed , _MainTexVSpeed));
				#ifdef _MAINTEXUVAUTOSPEED_ON
				float2 staticSwitch75 = ( appendResult66 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch75 = appendResult76;
				#endif
				float4 tex2DNode24_g1791 = tex2D( _MainTex, ( ( ( rotator2_g1792 + staticSwitch75 ) * _MainTex_ST.xy ) + _MainTex_ST.zw ) );
				float4 appendResult34_g1791 = (float4(half3(1,1,1) , 1.0));
				float4 temp_output_138_0 = ( tex2DNode24_g1791 * _MainTexColor * appendResult34_g1791 );
				float2 temp_cast_7 = (Distort56).xx;
				float2 lerpResult54 = lerp( texCoord9 , temp_cast_7 , _DissTexDistort);
				float2 temp_output_19_0_g1788 = lerpResult54;
				float2 temp_cast_8 = (_DissTexRotation).xx;
				float cos2_g1789 = cos( radians( temp_cast_8.x ) );
				float sin2_g1789 = sin( radians( temp_cast_8.x ) );
				float2 rotator2_g1789 = mul( temp_output_19_0_g1788 - half2( 0.5,0.5 ) , float2x2( cos2_g1789 , -sin2_g1789 , sin2_g1789 , cos2_g1789 )) + half2( 0.5,0.5 );
				float2 appendResult42 = (float2(_DissTexUSpeed , _DissTexVSpeed));
				float4 tex2DNode24_g1788 = tex2D( _DissTex, ( ( ( rotator2_g1789 + ( appendResult42 * ( _TimeParameters.x ) ) ) * _DissTex_ST.xy ) + _DissTex_ST.zw ) );
				float dotResult31_g1788 = dot( tex2DNode24_g1788 , _DissTexRGBA );
				float temp_output_21_0 = ( dotResult31_g1788 + 1.0 );
				float vertexToFrag24_g1790 = IN.ase_texcoord3.z;
				float3 appendResult141 = (float3(texCoord8.x , texCoord8.y , texCoord8.z));
				float3 appendResult144 = (float3(_Disspower , _DissEdgeWidth , _DissHardness));
				#ifdef _DISSSLIDERSWITCH_ON
				float3 staticSwitch143 = appendResult144;
				#else
				float3 staticSwitch143 = appendResult141;
				#endif
				float3 break147 = staticSwitch143;
				float Hardness13 = break147.z;
				float temp_output_203_0_g1804 = Hardness13;
				float4 lerpResult34 = lerp( _DissEdgeColor , temp_output_138_0 , saturate( ( ( ( temp_output_21_0 - vertexToFrag24_g1790 ) - temp_output_203_0_g1804 ) / ( 1.0 - temp_output_203_0_g1804 ) ) ));
				
				float vertexToFrag25_g1790 = IN.ase_texcoord3.w;
				float temp_output_203_0_g1801 = Hardness13;
				float2 temp_cast_12 = (Distort56).xx;
				float2 lerpResult85 = lerp( texCoord9 , temp_cast_12 , _MaskDistort);
				float2 temp_output_19_0_g1802 = lerpResult85;
				float2 temp_cast_13 = (_MaskTexRotation).xx;
				float cos2_g1803 = cos( radians( temp_cast_13.x ) );
				float sin2_g1803 = sin( radians( temp_cast_13.x ) );
				float2 rotator2_g1803 = mul( temp_output_19_0_g1802 - half2( 0.5,0.5 ) , float2x2( cos2_g1803 , -sin2_g1803 , sin2_g1803 , cos2_g1803 )) + half2( 0.5,0.5 );
				float2 appendResult96 = (float2(texCoord74.z , texCoord74.w));
				float2 appendResult93 = (float2(_MaskTexUSpeed , _MaskTexVSpeed));
				#ifdef _MASKTEXUVAUTOSPEED_ON
				float2 staticSwitch92 = ( appendResult93 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch92 = appendResult96;
				#endif
				float4 tex2DNode24_g1802 = tex2D( _MaskTex, ( ( ( rotator2_g1803 + staticSwitch92 ) * _MaskTex_ST.xy ) + _MaskTex_ST.zw ) );
				float dotResult31_g1802 = dot( tex2DNode24_g1802 , _MaskTexRGBA );
				float4 screenPos = IN.ase_texcoord6;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float screenDepth122 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				float distanceDepth122 = abs( ( screenDepth122 - LinearEyeDepth( ase_screenPosNorm.z,_ZBufferParams ) ) / ( _DepthBlend ) );
				float clampResult123 = clamp( distanceDepth122 , 0.0 , 1.0 );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = ( lerpResult34 * IN.ase_color ).rgb;
				float Alpha = ( temp_output_138_0.a * saturate( ( ( ( temp_output_21_0 - vertexToFrag25_g1790 ) - temp_output_203_0_g1801 ) / ( 1.0 - temp_output_203_0_g1801 ) ) ) * dotResult31_g1802 * IN.ase_color.a * clampResult123 );
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif
				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				return half4( Color, Alpha );
			}

			ENDHLSL
		}
	
	}
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18900
1939;12;1842;1019;166.9871;1551.229;2.794859;True;False
Node;AmplifyShaderEditor.CommentaryNode;113;-458.1792,-442.5151;Inherit;False;3294.615;1217.115;Dissolve;13;54;63;12;14;10;36;37;42;44;43;64;114;137;溶解;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;65;-1026.795,853.0027;Inherit;False;1145.086;673.9999;Distort;9;56;101;62;61;102;57;59;58;119;扰动;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;114;638.0236,-67.64746;Inherit;False;2159.05;818.2277;溶解边缘;17;35;31;28;27;25;21;20;16;13;7;140;141;143;144;145;146;147;溶解边缘软硬度及边缘宽度;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;58;-973.7947,1167.207;Inherit;False;Property;_DistortTexVSpeed;Distort TexVSpeed;32;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;59;-976.7947,1098.207;Inherit;False;Property;_DistortTexUSpeed;Distort TexUSpeed;31;0;Create;True;0;0;0;False;0;False;0;-3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;7;686.8757,42.01862;Inherit;False;406;347;Costom UV2;1;8;U-溶解强度；V溶解边缘宽度；W溶解边缘硬度 ；T主贴图的扰动强度;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;146;821.8792,639.11;Inherit;False;Property;_DissHardness;Diss Hardness;26;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;8;736.8757,92.01871;Inherit;False;1;-1;4;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;145;766.8792,542.11;Inherit;False;Property;_DissEdgeWidth;Diss Edge Width;25;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;140;731.4451,467.9766;Inherit;False;Property;_Disspower;Diss power;24;0;Create;True;0;0;0;False;0;False;0;0.05;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TimeNode;102;-997.5344,1213.186;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;9;-1936.238,-653.1953;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;57;-747.8683,1115.488;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;141;1086.897,127.9889;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;103;-1200.814,1291.852;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;144;1080.707,375.0751;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;101;-610.4414,1236.871;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;119;-794.9465,1367.741;Half;False;Property;_DistortTexRotation;Distort Tex Rotation;30;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;62;-989.9932,1336.301;Inherit;False;Property;_DistortTexRGBA;Distort Tex RGBA;29;0;Create;True;0;0;0;False;0;False;0,0,0,1;1,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;61;-971.0932,904.0028;Inherit;True;Property;_DistortTex;Distort Tex;28;0;Create;True;0;0;0;False;0;False;None;c922c1326f413ef4680c2c7dbdc922de;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.StaticSwitch;143;1250.946,186.3684;Inherit;False;Property;_DissSliderSwitch;Diss Slider Switch;23;0;Create;True;0;0;0;False;0;False;0;0;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;44;-405.9715,-317.5154;Inherit;False;Property;_DissTexVSpeed;Diss TexVSpeed;22;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;43;-410.9715,-392.5151;Inherit;False;Property;_DissTexUSpeed;Diss TexUSpeed;21;0;Create;True;0;0;0;False;0;False;0;-2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;136;-365.0919,1251.141;Inherit;False;FX_Tex_Speed_Rotator;-1;;1786;fd59490482a254543a193c2f92752b96;4,26,1,35,0,37,1,40,1;7;69;FLOAT2;0,0;False;16;SAMPLER2D;0;False;30;COLOR;0,0,0,0;False;25;COLOR;0,0,0,0;False;20;FLOAT2;0,0;False;19;FLOAT2;0,0;False;33;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;118;955.6785,-1926.733;Inherit;False;1398.369;1295.8;Main Tex;15;68;67;66;70;76;69;15;18;17;79;77;80;81;75;138;主贴图部分;1,1,1,1;0;0
Node;AmplifyShaderEditor.WireNode;64;-408.1792,-87.26138;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TimeNode;37;-147.0329,-271.4394;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;42;-112.3403,-390.1883;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;115;-2564.48,-752.582;Inherit;False;398;381;CustomUV3;2;74;116;UV控制主贴图纹理流动;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;63;6.758983,233.8286;Inherit;False;Property;_DissTexDistort;Diss Tex Distort;27;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;117;799.0667,1139.347;Inherit;False;1919.993;973.936;Mask;14;95;94;90;93;86;91;96;104;92;85;89;87;88;139;遮罩;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;67;1011.644,-1657.335;Inherit;False;Property;_MainTexUSpeed;Main Tex USpeed;6;0;Create;True;0;0;0;False;0;False;0;-3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;147;1547.023,171.2492;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RegisterLocalVarNode;56;-126.5959,1143.257;Inherit;False;Distort;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;68;1005.679,-1573.335;Inherit;False;Property;_MainTexVSpeed;Main Tex VSpeed;7;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;54;459.8838,40.35506;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;12;122.5479,124.4953;Half;False;Property;_DissTexRotation;Diss Tex Rotation;20;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;36;58.33068,-312.9402;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;81;1383.57,-766.1928;Inherit;False;Property;_MainTexDistPower;Main Tex DistPower;9;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;94;849.0667,1318.568;Inherit;False;Property;_MaskTexUSpeed;Mask TexUSpeed;14;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;66;1353.31,-1691.008;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;95;854.0667,1393.568;Inherit;False;Property;_MaskTexVSpeed;Mask TexVSpeed;15;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TimeNode;70;1307.617,-1584.259;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;14;136.3749,-229.6775;Half;True;Property;_DissTex;Diss Tex;17;0;Create;True;0;0;0;False;0;False;None;5292ac4dd8e2dfa4dbfde09e087fff18;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.Vector4Node;10;99.55071,-45.69223;Half;False;Property;_DissTexRGBA;Diss Tex RGBA;18;0;Create;True;0;0;0;False;0;False;1,0,0,0;1,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;74;-2514.48,-702.582;Inherit;False;2;-1;4;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;13;1809.861,251.4173;Float;False;Hardness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;80;1722.658,-770.9328;Inherit;False;Property;_MainTexDistort;Main Tex Distort;8;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;16;1766.627,330.0842;Inherit;False;355;209;Comment;1;23;调整溶解参数值域;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;93;1163.698,1347.895;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;137;664.2034,-295.5394;Inherit;False;FX_Tex_Speed_Rotator;-1;;1788;fd59490482a254543a193c2f92752b96;4,26,1,35,0,37,1,40,1;7;69;FLOAT2;0,0;False;16;SAMPLER2D;0;False;30;COLOR;0,0,0,0;False;25;COLOR;0,0,0,0;False;20;FLOAT2;0,0;False;19;FLOAT2;0,0;False;33;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;76;1340.403,-1876.733;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;79;1454.541,-904.6708;Inherit;False;56;Distort;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TimeNode;90;1126.005,1450.644;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;120;3172.628,446.3697;Inherit;False;798.923;209;软粒子;3;123;122;121;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;20;1395.698,634.5802;Inherit;False;13;Hardness;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;69;1523.981,-1613.76;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;104;1621.817,1908.544;Inherit;False;56;Distort;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;77;1856.415,-983.3121;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;23;1816.627,380.0843;Inherit;False;FX_Diss_EdgeWidth_Hardness;-1;;1790;7f5c0828cae086846b5187c68c8f94ee;0;3;21;FLOAT;0;False;22;FLOAT;0;False;23;FLOAT;0;False;2;FLOAT;0;FLOAT;20
Node;AmplifyShaderEditor.TexturePropertyNode;15;1343.004,-1426.297;Half;True;Property;_MainTex;Main Tex;2;0;Create;True;0;0;0;False;0;False;None;2627b4204c427784da0e29ff0d4c9ed8;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;121;3222.628,535.6486;Inherit;False;Property;_DepthBlend;Depth Blend;1;0;Create;True;0;0;0;False;0;False;0;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;75;1770.597,-1764.25;Inherit;False;Property;_MainTexUVAutoSpeed;Main TexUVAutoSpeed;5;0;Create;True;0;0;0;False;0;False;0;0;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;21;1635.269,-6.122127;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;91;1397.803,1423.303;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;17;1363.54,-1041.662;Half;False;Property;_MainTexRotation;Main Tex Rotation;4;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;98;-1349.237,1541.067;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ColorNode;18;1343.106,-1237.556;Half;False;Property;_MainTexColor;Main Tex Color;3;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;4.237095,4.237095,4.237095,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;96;1400.54,1189.347;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;86;1736.613,1997.283;Inherit;False;Property;_MaskDistort;Mask Distort;16;0;Create;True;0;0;0;False;0;False;0;0.2;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;27;2197.259,209.9636;Inherit;False;13;Hardness;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;138;2097.047,-1129.227;Inherit;False;FX_Tex_Speed_Rotator;-1;;1791;fd59490482a254543a193c2f92752b96;4,26,0,35,0,37,1,40,1;7;69;FLOAT2;0,0;False;16;SAMPLER2D;0;False;30;COLOR;0,0,0,0;False;25;COLOR;0,0,0,0;False;20;FLOAT2;0,0;False;19;FLOAT2;0,0;False;33;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;25;2198.57,338.9797;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;89;1753.21,1382.132;Half;True;Property;_MaskTex;Mask Tex;10;0;Create;True;0;0;0;False;0;False;None;cba920d5d698d8e428c41671ab943345;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.Vector4Node;88;1791.619,1576.292;Half;False;Property;_MaskTexRGBA;Mask Tex RGBA;11;0;Create;True;0;0;0;False;0;False;1,0,0,0;1,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DepthFade;122;3546.628,506.6477;Inherit;False;True;False;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;87;1773.383,1750.305;Half;False;Property;_MaskTexRotation;Mask Tex Rotation;12;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;85;2116.551,1814.751;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;92;1715.702,1239.573;Inherit;False;Property;_MaskTexUVAutoSpeed;Mask TexUVAutoSpeed;13;0;Create;True;0;0;0;False;0;False;0;0;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;139;2455.06,1533.861;Inherit;False;FX_Tex_Speed_Rotator;-1;;1802;fd59490482a254543a193c2f92752b96;4,26,1,35,0,37,1,40,1;7;69;FLOAT2;0,0;False;16;SAMPLER2D;0;False;30;COLOR;0,0,0,0;False;25;COLOR;0,0,0,0;False;20;FLOAT2;0,0;False;19;FLOAT2;0,0;False;33;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;30;3163.629,-398.8877;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.VertexColorNode;22;3403.027,-244.888;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;29;3445.691,-1092.151;Inherit;False;376.9424;381.7066;溶解边缘颜色UV3;1;72;粒子CustomData控制溶解边缘颜色;1,1,1,1;0;0
Node;AmplifyShaderEditor.FunctionNode;28;2536.072,326.5567;Inherit;True;FX_SmoothStepSimlpe;-1;;1801;69ad0cce1b3a7ac469db50b367d82cde;0;3;202;FLOAT;0;False;203;FLOAT;0;False;205;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;123;3833.549,496.3697;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;72;3554.5,-949.515;Inherit;False;Property;_DissEdgeColor;Diss EdgeColor;19;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;0.7490196,0.1801362,0,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;31;2500.354,-17.64749;Inherit;True;FX_SmoothStepSimlpe;-1;;1804;69ad0cce1b3a7ac469db50b367d82cde;0;3;202;FLOAT;0;False;203;FLOAT;0;False;205;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;112;4559.229,96.22072;Inherit;False;Property;_BlendMode;Blend Mode;0;1;[Enum];Create;True;0;2;Add;1;Blend;10;0;True;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;35;2169.674,-15.80239;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;34;3822.963,-508.322;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;33;3863.84,-47.88897;Inherit;True;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;26;4283.156,-300.1109;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;110;3951.18,-441.0367;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;106;4418.924,-361.3657;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;108;3951.18,-441.0367;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;107;4900.924,-290.3657;Half;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;SFX/ASE/Dissolve/DissolveDefault;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;0;False;True;2;5;False;-1;10;True;112;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;Hidden/InternalErrorShader;0;0;Standard;22;Surface;1;  Blend;0;Two Sided;0;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;0;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;0;DOTS Instancing;0;Meta Pass;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;False;True;False;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;109;3951.18,-441.0367;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.CommentaryNode;116;-2530.48,-500.582;Inherit;False;302;105;Comment;0;WT控制Mask贴图纹理流动;1,1,1,1;0;0
WireConnection;57;0;59;0
WireConnection;57;1;58;0
WireConnection;141;0;8;1
WireConnection;141;1;8;2
WireConnection;141;2;8;3
WireConnection;103;0;9;0
WireConnection;144;0;140;0
WireConnection;144;1;145;0
WireConnection;144;2;146;0
WireConnection;101;0;57;0
WireConnection;101;1;102;2
WireConnection;143;1;141;0
WireConnection;143;0;144;0
WireConnection;136;69;101;0
WireConnection;136;16;61;0
WireConnection;136;30;62;0
WireConnection;136;20;119;0
WireConnection;136;19;103;0
WireConnection;64;0;9;0
WireConnection;42;0;43;0
WireConnection;42;1;44;0
WireConnection;147;0;143;0
WireConnection;56;0;136;0
WireConnection;54;0;64;0
WireConnection;54;1;56;0
WireConnection;54;2;63;0
WireConnection;36;0;42;0
WireConnection;36;1;37;2
WireConnection;66;0;67;0
WireConnection;66;1;68;0
WireConnection;13;0;147;2
WireConnection;80;1;8;4
WireConnection;80;0;81;0
WireConnection;93;0;94;0
WireConnection;93;1;95;0
WireConnection;137;69;36;0
WireConnection;137;16;14;0
WireConnection;137;30;10;0
WireConnection;137;20;12;0
WireConnection;137;19;54;0
WireConnection;76;0;74;1
WireConnection;76;1;74;2
WireConnection;69;0;66;0
WireConnection;69;1;70;2
WireConnection;77;0;9;0
WireConnection;77;1;79;0
WireConnection;77;2;80;0
WireConnection;23;21;147;0
WireConnection;23;22;147;1
WireConnection;23;23;20;0
WireConnection;75;1;76;0
WireConnection;75;0;69;0
WireConnection;21;0;137;0
WireConnection;91;0;93;0
WireConnection;91;1;90;2
WireConnection;98;0;9;0
WireConnection;96;0;74;3
WireConnection;96;1;74;4
WireConnection;138;69;75;0
WireConnection;138;16;15;0
WireConnection;138;25;18;0
WireConnection;138;20;17;0
WireConnection;138;19;77;0
WireConnection;25;0;21;0
WireConnection;25;1;23;20
WireConnection;122;0;121;0
WireConnection;85;0;98;0
WireConnection;85;1;104;0
WireConnection;85;2;86;0
WireConnection;92;1;96;0
WireConnection;92;0;91;0
WireConnection;139;69;92;0
WireConnection;139;16;89;0
WireConnection;139;30;88;0
WireConnection;139;20;87;0
WireConnection;139;19;85;0
WireConnection;30;0;138;0
WireConnection;28;202;25;0
WireConnection;28;203;27;0
WireConnection;123;0;122;0
WireConnection;31;202;35;0
WireConnection;31;203;27;0
WireConnection;35;0;21;0
WireConnection;35;1;23;0
WireConnection;34;0;72;0
WireConnection;34;1;138;0
WireConnection;34;2;31;0
WireConnection;33;0;30;3
WireConnection;33;1;28;0
WireConnection;33;2;139;0
WireConnection;33;3;22;4
WireConnection;33;4;123;0
WireConnection;26;0;34;0
WireConnection;26;1;22;0
WireConnection;107;2;26;0
WireConnection;107;3;33;0
ASEEND*/
//CHKSM=0CB60B5C389BACF803FC3876B6B9729F70D45BAD