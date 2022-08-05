// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "BOXOPHOBIC/Atmospherics/Height Fog Per Object"
{
	Properties
	{
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector]_HeightFogPerObject("_HeightFogPerObject", Float) = 1
		[HideInInspector]_IsHeightFogShader("_IsHeightFogShader", Float) = 1
		[HideInInspector]_IsUniversalPipeline("_IsUniversalPipeline", Float) = 1
		[HideInInspector]_TransparentQueue("_TransparentQueue", Int) = 3100
		[BBanner(Height Fog, Per Object)]_TITLEE("< TITLEE >", Float) = 1
		[BCategory(Custom Alpha Inputs)]_CUSTOM("[ CUSTOM ]", Float) = 1
		_BaseColor("BaseColor", Color) = (1,1,1,1)
		[ASEEnd]_BaseMap("BaseMap", 2D) = "white" {}

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
		
		Cull Back
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 3.0

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
			
			Blend SrcAlpha OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#define ASE_SRP_VERSION 999999

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma multi_compile AHF_DIRECTIONALMODE_OFF AHF_DIRECTIONALMODE_ON
			#pragma multi_compile AHF_NOISEMODE_OFF AHF_NOISEMODE_PROCEDURAL3D


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
				float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _BaseColor;
			float4 _BaseMap_ST;
			half _IsUniversalPipeline;
			int _TransparentQueue;
			half _TITLEE;
			half _CUSTOM;
			half _IsHeightFogShader;
			half _HeightFogPerObject;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			half4 AHF_FogColor;
			half AHF_DirectionalIntensity;
			half AHF_DirectionalModeBlend;
			half AHF_FogDistanceStart;
			half AHF_FogDistanceEnd;
			half AHF_FogHeightEnd;
			half AHF_FogHeightStart;
			half AHF_NoiseScale;
			half3 AHF_NoiseSpeed;
			half AHF_NoiseDistanceEnd;
			half AHF_NoiseIntensity;
			half AHF_NoiseModeBlend;
			half AHF_FogIntensity;
			sampler2D _BaseMap;


			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			
			
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				int CustomVertexOffset918 = 0;
				float3 temp_cast_0 = CustomVertexOffset918;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = temp_cast_0;
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
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif
				float3 temp_output_2_0_g587 = (AHF_FogColor).rgb;
				float3 gammaToLinear3_g587 = FastSRGBToLinear( temp_output_2_0_g587 );
				#ifdef UNITY_COLORSPACE_GAMMA
				float3 staticSwitch1_g587 = temp_output_2_0_g587;
				#else
				float3 staticSwitch1_g587 = gammaToLinear3_g587;
				#endif
				float3 temp_output_924_0 = staticSwitch1_g587;
				float3 temp_output_2_0_g588 = (_MainLightColor).rgb;
				float3 gammaToLinear3_g588 = FastSRGBToLinear( temp_output_2_0_g588 );
				#ifdef UNITY_COLORSPACE_GAMMA
				float3 staticSwitch1_g588 = temp_output_2_0_g588;
				#else
				float3 staticSwitch1_g588 = gammaToLinear3_g588;
				#endif
				float3 WorldPosition144 = WorldPosition;
				float3 normalizeResult5_g582 = normalize( ( WorldPosition144 - _WorldSpaceCameraPos ) );
				float dotResult6_g582 = dot( normalizeResult5_g582 , SafeNormalize(_MainLightPosition.xyz) );
				float temp_output_7_0_g583 = -1.0;
				half DirectionalMask134 = ( ( ( dotResult6_g582 - temp_output_7_0_g583 ) / ( 1.0 - temp_output_7_0_g583 ) ) * AHF_DirectionalIntensity * AHF_DirectionalModeBlend );
				float3 lerpResult135 = lerp( temp_output_924_0 , staticSwitch1_g588 , DirectionalMask134);
				#if defined(AHF_DIRECTIONALMODE_OFF)
				float3 staticSwitch241 = temp_output_924_0;
				#elif defined(AHF_DIRECTIONALMODE_ON)
				float3 staticSwitch241 = lerpResult135;
				#else
				float3 staticSwitch241 = temp_output_924_0;
				#endif
				
				float temp_output_7_0_g563 = AHF_FogDistanceStart;
				half FogDistanceMask186 = saturate( ( ( distance( WorldPosition144 , _WorldSpaceCameraPos ) - temp_output_7_0_g563 ) / ( AHF_FogDistanceEnd - temp_output_7_0_g563 ) ) );
				float temp_output_7_0_g565 = AHF_FogHeightEnd;
				half FogHeightMask193 = saturate( ( ( (WorldPosition144).y - temp_output_7_0_g565 ) / ( AHF_FogHeightStart - temp_output_7_0_g565 ) ) );
				float temp_output_115_0 = ( FogDistanceMask186 * FogHeightMask193 );
				float simplePerlin3D15_g566 = snoise( ( ( WorldPosition144 * ( 1.0 / AHF_NoiseScale ) ) + ( -AHF_NoiseSpeed * _TimeParameters.x ) ) );
				float temp_output_7_0_g569 = -1.0;
				float temp_output_7_0_g560 = AHF_NoiseDistanceEnd;
				half NoiseDistanceMask353 = saturate( ( ( distance( WorldPosition144 , _WorldSpaceCameraPos ) - temp_output_7_0_g560 ) / ( 0.0 - temp_output_7_0_g560 ) ) );
				float lerpResult20_g566 = lerp( 1.0 , ( ( simplePerlin3D15_g566 - temp_output_7_0_g569 ) / ( 1.0 - temp_output_7_0_g569 ) ) , ( NoiseDistanceMask353 * AHF_NoiseIntensity * AHF_NoiseModeBlend ));
				half NoiseSimplex3D234 = lerpResult20_g566;
				#if defined(AHF_NOISEMODE_OFF)
				float staticSwitch242 = temp_output_115_0;
				#elif defined(AHF_NOISEMODE_PROCEDURAL3D)
				float staticSwitch242 = ( temp_output_115_0 * NoiseSimplex3D234 );
				#else
				float staticSwitch242 = temp_output_115_0;
				#endif
				float2 uv_BaseMap = IN.ase_texcoord3.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float temp_output_1_0_g586 = tex2Dbias( _BaseMap, float4( uv_BaseMap, 0, 0.0) ).a;
				float lerpResult3_g586 = lerp( temp_output_1_0_g586 , ceil( temp_output_1_0_g586 ) , FogDistanceMask186);
				half CustomAlphaInputs897 = ( _BaseColor.a * lerpResult3_g586 );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = staticSwitch241;
				float Alpha = ( staticSwitch242 * AHF_FogIntensity * CustomAlphaInputs897 );
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				return half4( Color, Alpha );
			}

			ENDHLSL
		}

	
	}
	
	
	
}
/*ASEBEGIN
Version=18900
0;32;1906;987;4140.322;4946.89;1;True;False
Node;AmplifyShaderEditor.WorldPosInputsNode;885;-3328,-1664;Float;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;144;-2496,-1664;Float;False;WorldPosition;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;626;-3328,-128;Inherit;False;144;WorldPosition;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;573;-3328,-896;Inherit;False;144;WorldPosition;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;929;-3072,-128;Inherit;False;Fog Noise Distance;-1;;559;b49331aa561bee84c8546167a63b6b58;0;1;18;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;927;-3072,-896;Inherit;False;Fog Distance;-1;;562;a5f090963b8f9394a984ee752ce42488;0;1;13;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;353;-2528,-128;Half;False;NoiseDistanceMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;189;-3328,-512;Inherit;False;144;WorldPosition;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;928;-3072,-512;Inherit;False;Fog Height;-1;;564;61a3ca69e0854664d87b5233acc9cbae;0;1;8;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;186;-2528,-896;Half;False;FogDistanceMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;895;-3328,-3328;Inherit;False;0;892;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;931;-3328,336;Inherit;False;353;NoiseDistanceMask;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;217;-3328,256;Inherit;False;144;WorldPosition;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;892;-3072,-3328;Inherit;True;Property;_BaseMap;BaseMap;7;0;Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;MipBias;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;179;-3328,-1280;Inherit;False;144;WorldPosition;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;193;-2512,-512;Half;False;FogHeightMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;902;-3008,-3136;Inherit;False;186;FogDistanceMask;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;930;-3008,256;Inherit;False;Fog Noise;-1;;566;fe51dc291bbca2147aa47de2202288d1;0;2;22;FLOAT3;0,0,0;False;23;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;894;-3328,-3584;Half;False;Property;_BaseColor;BaseColor;6;0;Create;False;0;0;0;False;0;False;1,1,1,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;913;-2688,-3328;Inherit;False;Handle Tex Alpha;-1;;586;92f31391e7f50294c9c2d8747c81d6b6;0;2;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;188;-3328,-4224;Inherit;False;186;FogDistanceMask;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LightColorNode;939;-3285.322,-4539.89;Inherit;False;0;3;COLOR;0;FLOAT3;1;FLOAT;2
Node;AmplifyShaderEditor.GetLocalVarNode;194;-3328,-4144;Inherit;False;193;FogHeightMask;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;137;-3328,-4736;Half;False;Global;AHF_FogColor;AHF_FogColor;4;0;Create;False;0;0;0;False;0;False;0.4411765,0.722515,1,0;0,0.5827588,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;234;-2512,256;Half;False;NoiseSimplex3D;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;926;-3072,-1280;Inherit;False;Fog Directional;-1;;582;b76195d9ca3254b49ac2428d15088cd6;0;1;15;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SwizzleNode;863;-3072,-4544;Inherit;False;FLOAT3;0;1;2;3;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;134;-2512,-1280;Half;False;DirectionalMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SwizzleNode;862;-3072,-4736;Inherit;False;FLOAT3;0;1;2;3;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;250;-3328,-4064;Inherit;False;234;NoiseSimplex3D;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;896;-2304,-3584;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;115;-3072,-4224;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;897;-2144,-3584;Half;False;CustomAlphaInputs;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;925;-2880,-4544;Inherit;False;Handle Color Space;-1;;588;f6f44b689bae74d47a0885dbe3018c48;0;1;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.IntNode;919;-3328,-2688;Float;False;Constant;_Int0;Int 0;5;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;136;-3328,-4352;Inherit;False;134;DirectionalMask;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;924;-2880,-4736;Inherit;False;Handle Color Space;-1;;587;f6f44b689bae74d47a0885dbe3018c48;0;1;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;276;-2880,-4096;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;918;-3136,-2688;Half;False;CustomVertexOffset;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.LerpOp;135;-2368,-4608;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;278;-2432,-4128;Half;False;Global;AHF_FogIntensity;AHF_FogIntensity;3;1;[HideInInspector];Create;False;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;898;-2432,-4032;Inherit;False;897;CustomAlphaInputs;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;242;-2688,-4224;Float;False;Property;AHF_NOISEMODE;AHF_NoiseMode;14;0;Create;False;0;0;0;False;0;False;1;0;0;False;;KeywordEnum;2;_OFF;_PROCEDURAL3D;Create;False;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;879;-3328,-5120;Half;False;Property;_HeightFogPerObject;_HeightFogPerObject;0;1;[HideInInspector];Create;False;0;0;0;True;0;False;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;920;-1856,-4608;Inherit;False;918;CustomVertexOffset;1;0;OBJECT;0;False;1;INT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;277;-2048,-4224;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;890;-3072,-5120;Half;False;Property;_IsHeightFogShader;_IsHeightFogShader;1;1;[HideInInspector];Create;False;0;0;0;True;0;False;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;102;-3561,-4567;Half;False;Global;AHF_DirectionalColor;AHF_DirectionalColor;12;0;Create;False;0;0;0;False;0;False;1,0.6300203,0.1617647,0;1,0.5318965,0.2499999,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;923;-3328,-5248;Half;False;Property;_TITLEE;< TITLEE >;4;0;Create;True;0;0;0;True;1;BBanner(Height Fog, Per Object);False;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;922;-2576,-5120;Float;False;Property;_TransparentQueue;_TransparentQueue;3;1;[HideInInspector];Create;False;0;0;0;True;0;False;3100;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;933;-2816,-5120;Half;False;Property;_IsUniversalPipeline;_IsUniversalPipeline;2;1;[HideInInspector];Create;False;0;0;0;True;0;False;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;241;-2176,-4736;Float;False;Property;AHF_DIRECTIONALMODE;AHF_DirectionalMode;11;0;Create;False;0;0;0;False;0;False;1;0;0;False;;KeywordEnum;2;_OFF;_ON;Create;False;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;899;-3169,-5248;Half;False;Property;_CUSTOM;[ CUSTOM ];5;0;Create;True;0;0;0;True;1;BCategory(Custom Alpha Inputs);False;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;938;-1488,-4736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;937;-1488,-4736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;934;-1488,-4736;Float;False;True;-1;2;;0;3;BOXOPHOBIC/Atmospherics/Height Fog Per Object;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;2;0;False;True;2;5;False;-1;10;False;-1;0;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;0;False;-1;True;False;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;22;Surface;1;  Blend;0;Two Sided;1;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;0;GPU Instancing;0;LOD CrossFade;0;Built-in Fog;0;DOTS Instancing;0;Meta Pass;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;False;False;False;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;935;-1488,-4736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;936;-1488,-4736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.CommentaryNode;354;-3328,-256;Inherit;False;1022.526;100;Noise Distance Mask;0;;0.7529412,1,0.7529412,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;235;-3328,128;Inherit;False;1022.805;100;Noise;0;;0.7529412,1,0.7529412,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;196;-3328,-640;Inherit;False;1028.158;100;Fog Height;0;;0,0.5882353,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;190;-3328,-1024;Inherit;False;1023.127;100;Fog Distance;0;;0,0.5882353,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;174;-3328,-1792;Inherit;False;1024.716;100;World Position;0;;0,1,0,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;880;-3328,-5376;Inherit;False;895.4729;100;Drawers;0;;1,0.475862,0,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;612;-3328,-4864;Inherit;False;2045.196;100;Final Pass;0;;0.684,1,0,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;180;-3328,-1408;Inherit;False;1023.152;100;Directional Light Support;0;;1,0.634,0.1617647,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;916;-3328,-2816;Inherit;False;2045.196;100;Custom Vertex Offset / Add here your custom Vertex Offset;0;;0.684,1,0,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;891;-3328,-3712;Inherit;False;2043.196;100;Custom Alpha Inputs / Add here your custom Alpha Inputs;0;;0.684,1,0,1;0;0
WireConnection;144;0;885;0
WireConnection;929;18;626;0
WireConnection;927;13;573;0
WireConnection;353;0;929;0
WireConnection;928;8;189;0
WireConnection;186;0;927;0
WireConnection;892;1;895;0
WireConnection;193;0;928;0
WireConnection;930;22;217;0
WireConnection;930;23;931;0
WireConnection;913;1;892;4
WireConnection;913;2;902;0
WireConnection;234;0;930;0
WireConnection;926;15;179;0
WireConnection;863;0;939;0
WireConnection;134;0;926;0
WireConnection;862;0;137;0
WireConnection;896;0;894;4
WireConnection;896;1;913;0
WireConnection;115;0;188;0
WireConnection;115;1;194;0
WireConnection;897;0;896;0
WireConnection;925;2;863;0
WireConnection;924;2;862;0
WireConnection;276;0;115;0
WireConnection;276;1;250;0
WireConnection;918;0;919;0
WireConnection;135;0;924;0
WireConnection;135;1;925;0
WireConnection;135;2;136;0
WireConnection;242;1;115;0
WireConnection;242;0;276;0
WireConnection;277;0;242;0
WireConnection;277;1;278;0
WireConnection;277;2;898;0
WireConnection;241;1;924;0
WireConnection;241;0;135;0
WireConnection;934;2;241;0
WireConnection;934;3;277;0
WireConnection;934;5;920;0
ASEEND*/
//CHKSM=747FCD9A111DCECBA33D5FD433488CAF826E1275