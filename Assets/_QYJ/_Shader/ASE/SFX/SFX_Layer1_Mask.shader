// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SFX/ASE/Layer/Layer1_mask"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][Enum(Add,1,Blend,10)]_BlendMode("Blend Mode", Float) = 1
		_MainTex("Main Tex", 2D) = "white" {}
		_MainTexRGBA("Main Tex RGBA", Vector) = (0,0,0,1)
		[HDR]_MainTexColor("Main Tex Color", Color) = (1,1,1,1)
		_MainTexRoation("Main Tex Roation", Range( 0 , 360)) = 0
		_MainTexDisstort("Main Tex Disstort", Range( 0 , 1)) = 0
		[Toggle(_MAINTEXUVAUTOSPEED_ON)] _MainTexUVAutoSpeed("Main Tex UVAutoSpeed", Float) = 0
		_MainTexUSpeed("Main Tex USpeed", Float) = 0
		_MainTexVSpeed("Main Tex VSpeed", Float) = 0
		_MaskTex("Mask Tex", 2D) = "white" {}
		_MaskTexRGBA("Mask Tex RGBA", Vector) = (0,0,0,1)
		_MaskTexRotation("Mask Tex Rotation", Range( 0 , 360)) = 0
		_MaskTexDisstort("Mask Tex Disstort", Range( 0 , 1)) = 0
		[Toggle(_MASKTEXUVAUTOSPEED_ON)] _MaskTexUVAutoSpeed("Mask Tex UVAutoSpeed", Float) = 0
		_MaskTexUSpeed("Mask Tex USpeed", Float) = 0
		_MaskTexVSpeed("Mask Tex VSpeed", Float) = 0
		_DisstortTex("Disstort Tex", 2D) = "white" {}
		_DisstortTexRGBA("Disstort Tex RGBA", Vector) = (0,0,0,1)
		_DisstortTexRotation("Disstort Tex Rotation", Range( 0 , 360)) = 0
		[Toggle(_DISSTORTTEXUVAUTOSPEED_ON)] _DisstortTexUVAutoSpeed("Disstort Tex UVAutoSpeed", Float) = 0
		_DisstortTexUSpeed("Disstort Tex USpeed", Float) = 0
		_DisstortTexVSpeed("Disstort Tex VSpeed", Float) = 0
		[ASEEnd]_DepthBlend("Depth Blend", Range( 0 , 2)) = 2

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
			
			Blend SrcAlpha [_BlendMode], One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#pragma multi_compile_instancing
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1

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

			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _MAINTEXUVAUTOSPEED_ON
			#pragma shader_feature_local _DISSTORTTEXUVAUTOSPEED_ON
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
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
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
			float4 _MainTexColor;
			float4 _MainTex_ST;
			float4 _DisstortTex_ST;
			float4 _MaskTex_ST;
			float4 _MaskTexRGBA;
			float4 _DisstortTexRGBA;
			half4 _MainTexRGBA;
			float _BlendMode;
			float _MaskTexDisstort;
			float _MaskTexRotation;
			float _MaskTexVSpeed;
			float _MaskTexUSpeed;
			float _DisstortTexRotation;
			float _DisstortTexVSpeed;
			float _DisstortTexUSpeed;
			float _MainTexRoation;
			float _MainTexVSpeed;
			float _MainTexUSpeed;
			float _MainTexDisstort;
			float _DepthBlend;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;
			sampler2D _DisstortTex;
			sampler2D _MaskTex;
			uniform float4 _CameraDepthTexture_TexelSize;


						
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord6 = screenPos;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				o.ase_texcoord4 = v.ase_texcoord1;
				o.ase_texcoord5 = v.ase_texcoord2;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
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
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;

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
				o.ase_texcoord1 = v.ase_texcoord1;
				o.ase_texcoord2 = v.ase_texcoord2;
				o.ase_color = v.ase_color;
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
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				o.ase_texcoord2 = patch[0].ase_texcoord2 * bary.x + patch[1].ase_texcoord2 * bary.y + patch[2].ase_texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
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
				float2 texCoord15 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult44 = (float2(IN.ase_texcoord4.x , IN.ase_texcoord4.y));
				float2 appendResult55 = (float2(_MainTexUSpeed , _MainTexVSpeed));
				#ifdef _MAINTEXUVAUTOSPEED_ON
				float2 staticSwitch49 = ( appendResult55 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch49 = appendResult44;
				#endif
				float cos20 = cos( radians( _MainTexRoation ) );
				float sin20 = sin( radians( _MainTexRoation ) );
				float2 rotator20 = mul( ( ( _MainTex_ST.xy * texCoord15 ) + _MainTex_ST.zw + staticSwitch49 ) - half2( 0.5,0.5 ) , float2x2( cos20 , -sin20 , sin20 , cos20 )) + half2( 0.5,0.5 );
				float2 texCoord91 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult104 = (float2(IN.ase_texcoord5.x , IN.ase_texcoord5.y));
				float2 appendResult88 = (float2(_DisstortTexUSpeed , _DisstortTexVSpeed));
				#ifdef _DISSTORTTEXUVAUTOSPEED_ON
				float2 staticSwitch103 = ( appendResult88 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch103 = appendResult104;
				#endif
				float cos98 = cos( radians( _DisstortTexRotation ) );
				float sin98 = sin( radians( _DisstortTexRotation ) );
				float2 rotator98 = mul( ( ( _DisstortTex_ST.xy * texCoord91 ) + _DisstortTex_ST.zw + staticSwitch103 ) - half2( 0.5,0.5 ) , float2x2( cos98 , -sin98 , sin98 , cos98 )) + half2( 0.5,0.5 );
				float dotResult101 = dot( tex2D( _DisstortTex, rotator98 ) , _DisstortTexRGBA );
				float Disstort106 = dotResult101;
				float2 temp_cast_1 = (Disstort106).xx;
				float2 lerpResult110 = lerp( rotator20 , temp_cast_1 , _MainTexDisstort);
				float4 tex2DNode9 = tex2D( _MainTex, lerpResult110 );
				
				float dotResult12 = dot( tex2DNode9 , _MainTexRGBA );
				float2 texCoord40 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult71 = (float2(IN.ase_texcoord4.z , IN.ase_texcoord4.w));
				float2 appendResult69 = (float2(_MaskTexUSpeed , _MaskTexVSpeed));
				#ifdef _MASKTEXUVAUTOSPEED_ON
				float2 staticSwitch73 = ( appendResult69 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch73 = appendResult71;
				#endif
				float cos37 = cos( radians( _MaskTexRotation ) );
				float sin37 = sin( radians( _MaskTexRotation ) );
				float2 rotator37 = mul( ( ( _MaskTex_ST.xy * texCoord40 ) + _MaskTex_ST.zw + staticSwitch73 ) - half2( 0.5,0.5 ) , float2x2( cos37 , -sin37 , sin37 , cos37 )) + half2( 0.5,0.5 );
				float2 temp_cast_4 = (Disstort106).xx;
				float2 lerpResult108 = lerp( rotator37 , temp_cast_4 , _MaskTexDisstort);
				float dotResult75 = dot( tex2D( _MaskTex, lerpResult108 ) , _MaskTexRGBA );
				float4 screenPos = IN.ase_texcoord6;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float screenDepth25 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				float distanceDepth25 = abs( ( screenDepth25 - LinearEyeDepth( ase_screenPosNorm.z,_ZBufferParams ) ) / ( _DepthBlend ) );
				float clampResult28 = clamp( distanceDepth25 , 0.0 , 1.0 );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = ( _MainTexColor * tex2DNode9 * IN.ase_color ).rgb;
				float Alpha = ( _MainTexColor.a * dotResult12 * IN.ase_color.a * dotResult75 * clampResult28 );
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

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#pragma multi_compile_instancing
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#pragma shader_feature_local _MAINTEXUVAUTOSPEED_ON
			#pragma shader_feature_local _DISSTORTTEXUVAUTOSPEED_ON
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
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_color : COLOR;
				float4 ase_texcoord5 : TEXCOORD5;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainTexColor;
			float4 _MainTex_ST;
			float4 _DisstortTex_ST;
			float4 _MaskTex_ST;
			float4 _MaskTexRGBA;
			float4 _DisstortTexRGBA;
			half4 _MainTexRGBA;
			float _BlendMode;
			float _MaskTexDisstort;
			float _MaskTexRotation;
			float _MaskTexVSpeed;
			float _MaskTexUSpeed;
			float _DisstortTexRotation;
			float _DisstortTexVSpeed;
			float _DisstortTexUSpeed;
			float _MainTexRoation;
			float _MainTexVSpeed;
			float _MainTexUSpeed;
			float _MainTexDisstort;
			float _DepthBlend;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;
			sampler2D _DisstortTex;
			sampler2D _MaskTex;
			uniform float4 _CameraDepthTexture_TexelSize;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord5 = screenPos;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_texcoord3 = v.ase_texcoord1;
				o.ase_texcoord4 = v.ase_texcoord2;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
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

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;

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
				o.ase_texcoord1 = v.ase_texcoord1;
				o.ase_texcoord2 = v.ase_texcoord2;
				o.ase_color = v.ase_color;
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
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				o.ase_texcoord2 = patch[0].ase_texcoord2 * bary.x + patch[1].ase_texcoord2 * bary.y + patch[2].ase_texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
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

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
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

				float2 texCoord15 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult44 = (float2(IN.ase_texcoord3.x , IN.ase_texcoord3.y));
				float2 appendResult55 = (float2(_MainTexUSpeed , _MainTexVSpeed));
				#ifdef _MAINTEXUVAUTOSPEED_ON
				float2 staticSwitch49 = ( appendResult55 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch49 = appendResult44;
				#endif
				float cos20 = cos( radians( _MainTexRoation ) );
				float sin20 = sin( radians( _MainTexRoation ) );
				float2 rotator20 = mul( ( ( _MainTex_ST.xy * texCoord15 ) + _MainTex_ST.zw + staticSwitch49 ) - half2( 0.5,0.5 ) , float2x2( cos20 , -sin20 , sin20 , cos20 )) + half2( 0.5,0.5 );
				float2 texCoord91 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult104 = (float2(IN.ase_texcoord4.x , IN.ase_texcoord4.y));
				float2 appendResult88 = (float2(_DisstortTexUSpeed , _DisstortTexVSpeed));
				#ifdef _DISSTORTTEXUVAUTOSPEED_ON
				float2 staticSwitch103 = ( appendResult88 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch103 = appendResult104;
				#endif
				float cos98 = cos( radians( _DisstortTexRotation ) );
				float sin98 = sin( radians( _DisstortTexRotation ) );
				float2 rotator98 = mul( ( ( _DisstortTex_ST.xy * texCoord91 ) + _DisstortTex_ST.zw + staticSwitch103 ) - half2( 0.5,0.5 ) , float2x2( cos98 , -sin98 , sin98 , cos98 )) + half2( 0.5,0.5 );
				float dotResult101 = dot( tex2D( _DisstortTex, rotator98 ) , _DisstortTexRGBA );
				float Disstort106 = dotResult101;
				float2 temp_cast_1 = (Disstort106).xx;
				float2 lerpResult110 = lerp( rotator20 , temp_cast_1 , _MainTexDisstort);
				float4 tex2DNode9 = tex2D( _MainTex, lerpResult110 );
				float dotResult12 = dot( tex2DNode9 , _MainTexRGBA );
				float2 texCoord40 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult71 = (float2(IN.ase_texcoord3.z , IN.ase_texcoord3.w));
				float2 appendResult69 = (float2(_MaskTexUSpeed , _MaskTexVSpeed));
				#ifdef _MASKTEXUVAUTOSPEED_ON
				float2 staticSwitch73 = ( appendResult69 * ( _TimeParameters.x ) );
				#else
				float2 staticSwitch73 = appendResult71;
				#endif
				float cos37 = cos( radians( _MaskTexRotation ) );
				float sin37 = sin( radians( _MaskTexRotation ) );
				float2 rotator37 = mul( ( ( _MaskTex_ST.xy * texCoord40 ) + _MaskTex_ST.zw + staticSwitch73 ) - half2( 0.5,0.5 ) , float2x2( cos37 , -sin37 , sin37 , cos37 )) + half2( 0.5,0.5 );
				float2 temp_cast_3 = (Disstort106).xx;
				float2 lerpResult108 = lerp( rotator37 , temp_cast_3 , _MaskTexDisstort);
				float dotResult75 = dot( tex2D( _MaskTex, lerpResult108 ) , _MaskTexRGBA );
				float4 screenPos = IN.ase_texcoord5;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float screenDepth25 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				float distanceDepth25 = abs( ( screenDepth25 - LinearEyeDepth( ase_screenPosNorm.z,_ZBufferParams ) ) / ( _DepthBlend ) );
				float clampResult28 = clamp( distanceDepth25 , 0.0 , 1.0 );
				
				float Alpha = ( _MainTexColor.a * dotResult12 * IN.ase_color.a * dotResult75 * clampResult28 );
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

	
	}
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18900
136;466;1906;669;2314.475;-36.9563;2.685146;True;False
Node;AmplifyShaderEditor.CommentaryNode;78;-3165.752,2397.191;Inherit;False;4024.214;1335.837;Distort;8;106;101;99;100;83;82;86;79;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;79;-3106.33,2796.081;Inherit;False;1222.106;822.7736;扰动贴图UV流动;3;103;81;80;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;80;-2978.599,2847.328;Inherit;False;672.6682;412.3875;材质球控制UV流动;5;90;87;88;85;84;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;84;-2906.492,2917.102;Inherit;False;Property;_DisstortTexUSpeed;Disstort Tex USpeed;20;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;81;-2962.675,3284.573;Inherit;False;426.8318;257;粒子系统控制主贴图UV流动;2;104;102;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;85;-2906.492,2997.102;Inherit;False;Property;_DisstortTexVSpeed;Disstort Tex VSpeed;21;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;86;-2403.3,2490.359;Inherit;True;Property;_DisstortTex;Disstort Tex;16;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.DynamicAppendNode;88;-2634.491,2949.102;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TimeNode;87;-2858.492,3093.101;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;74;-3660.273,964.9894;Inherit;False;3074.315;1308.425;Mask;10;75;76;32;61;63;33;38;107;109;108;;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;102;-2918.675,3373.573;Inherit;False;2;4;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;82;-1974.843,2457.536;Inherit;False;850.4453;321.7212;Disstort UVTiling;5;97;93;92;91;89;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;77;-4051.851,-337.1509;Inherit;False;3615.087;1242.334;Main Tex;13;12;11;10;8;9;7;30;31;58;110;111;112;24;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;83;-1783.853,2802.571;Inherit;False;788;305;Distort旋转;4;98;96;95;94;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;63;-3624.184,1413.119;Inherit;False;1222.106;822.7736;Mask贴图UV流动;3;73;65;64;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;90;-2442.491,3013.102;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureTransformNode;89;-1898.529,2507.981;Inherit;False;-1;False;1;0;SAMPLER2D;;False;2;FLOAT2;0;FLOAT2;1
Node;AmplifyShaderEditor.TextureCoordinatesNode;91;-1899.621,2629.802;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;104;-2663.674,3379.573;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;58;-4001.85,49.13716;Inherit;False;1222.106;822.7736;主贴图UV流动;3;56;49;57;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;92;-1544.253,2526.031;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;57;-3951.85,99.13718;Inherit;False;672.6682;412.3875;材质球控制UV流动;5;52;50;53;55;51;;1,1,1,1;0;0
Node;AmplifyShaderEditor.WireNode;93;-1573.039,2683.046;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;94;-1741.853,3006.571;Inherit;False;Property;_DisstortTexRotation;Disstort Tex Rotation;18;0;Create;True;0;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;64;-3574.184,1463.119;Inherit;False;672.6682;412.3875;材质球控制UV流动;5;72;69;68;67;66;;1,1,1,1;0;0
Node;AmplifyShaderEditor.StaticSwitch;103;-2251.852,3253.126;Inherit;False;Property;_DisstortTexUVAutoSpeed;Disstort Tex UVAutoSpeed;19;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;97;-1276.398,2554.167;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RadiansOpNode;95;-1413.854,2996.571;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;67;-3502.078,1612.893;Inherit;False;Property;_MaskTexVSpeed;Mask Tex VSpeed;15;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;96;-1445.854,2868.571;Half;False;Constant;_Vector1;Vector 1;7;0;Create;True;0;0;0;False;0;False;0.5,0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;52;-3879.744,168.911;Inherit;False;Property;_MainTexUSpeed;Main Tex USpeed;7;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;66;-3502.078,1532.893;Inherit;False;Property;_MaskTexUSpeed;Mask Tex USpeed;14;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;65;-3439.9,1907.339;Inherit;False;426.8318;257;粒子系统控制主贴图UV流动;2;71;70;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;56;-3766.567,547.3572;Inherit;False;426.8318;257;粒子系统控制主贴图UV流动;2;47;44;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;53;-3879.744,248.9109;Inherit;False;Property;_MainTexVSpeed;Main Tex VSpeed;8;0;Create;True;0;0;0;False;0;False;0;-2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TimeNode;68;-3454.078,1708.892;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TimeNode;51;-3831.744,344.9105;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;7;-3526.391,-188.7223;Inherit;True;Property;_MainTex;Main Tex;1;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TexCoordVertexDataNode;47;-3718.567,595.3571;Inherit;False;1;4;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexCoordVertexDataNode;70;-3391.9,1955.339;Inherit;False;1;4;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;38;-2954.555,1044.044;Inherit;False;850.4453;321.7212;MaskUVTiling;5;42;41;40;39;62;;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;69;-3230.078,1564.893;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;31;-3238.244,-287.1509;Inherit;False;850.4453;321.7212;主贴图UVTiling;3;13;23;15;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RotatorNode;98;-1185.467,2856.648;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;61;-3362.984,1126.134;Inherit;True;Property;_MaskTex;Mask Tex;9;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.DynamicAppendNode;55;-3607.744,200.9109;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;33;-2330.428,1846.193;Inherit;False;788;305;MASK旋转;4;37;36;35;34;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;30;-2688.596,334.6967;Inherit;False;788;305;主贴图UV旋转;4;19;16;22;20;;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;44;-3494.567,627.3572;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;72;-3038.078,1628.893;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;15;-3201.135,-101.1296;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;50;-3415.744,264.9109;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;71;-3140.9,2002.339;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector4Node;100;-694.7418,3030.22;Inherit;False;Property;_DisstortTexRGBA;Disstort Tex RGBA;17;0;Create;True;0;0;0;False;0;False;0,0,0,1;1,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;99;-799.3425,2764.677;Inherit;True;Global;DissTex1111;Diss Tex1111;6;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;40;-2879.334,1216.309;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureTransformNode;39;-2878.242,1094.488;Inherit;False;-1;False;1;0;SAMPLER2D;;False;2;FLOAT2;0;FLOAT2;1
Node;AmplifyShaderEditor.TextureTransformNode;23;-3199.244,-233.1506;Inherit;False;-1;False;1;0;SAMPLER2D;;False;2;FLOAT2;0;FLOAT2;1
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;41;-2519.889,1164.191;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.WireNode;62;-2552.753,1269.554;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;13;-2668.268,-233.2005;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;19;-2638.596,512.6966;Inherit;False;Property;_MainTexRoation;Main Tex Roation;4;0;Create;True;0;0;0;False;0;False;0;90;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;34;-2288.428,2050.193;Inherit;False;Property;_MaskTexRotation;Mask Tex Rotation;11;0;Create;True;0;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;73;-2693.078,1876.892;Inherit;False;Property;_MaskTexUVAutoSpeed;Mask Tex UVAutoSpeed;13;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DotProductOpNode;101;-336.5869,2981.11;Inherit;False;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;49;-3159.744,270.9105;Inherit;False;Property;_MainTexUVAutoSpeed;Main Tex UVAutoSpeed;6;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;106;-117.7658,2785.087;Inherit;False;Disstort;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RadiansOpNode;22;-2353.596,528.6965;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;16;-2615.597,394.6967;Half;False;Constant;_RotateCenter;Rotate Center;7;0;Create;True;0;0;0;False;0;False;0.5,0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleAddOpNode;42;-2250.674,1199.124;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;36;-1992.429,1912.193;Half;False;Constant;_Vector0;Vector 0;7;0;Create;True;0;0;0;False;0;False;0.5,0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RadiansOpNode;35;-1960.429,2040.193;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;24;-2280.799,-135.5201;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;107;-1632.142,1456.593;Inherit;False;106;Disstort;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotatorNode;37;-1768.043,1949.271;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;29;-430.1789,1563.76;Inherit;False;798.923;209;软粒子;3;26;25;28;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;111;-2006,94;Inherit;False;106;Disstort;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotatorNode;20;-2209.595,423.6967;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;112;-2075,199;Inherit;False;Property;_MainTexDisstort;Main Tex Disstort;5;0;Create;True;0;0;0;False;0;False;0;0.05;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;109;-1627.001,1575.362;Inherit;False;Property;_MaskTexDisstort;Mask Tex Disstort;12;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;26;-380.1788,1653.039;Inherit;False;Property;_DepthBlend;Depth Blend;22;0;Create;True;0;0;0;False;0;False;2;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;110;-1732.139,102.7687;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.LerpOp;108;-1359.425,1412.031;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;9;-1490.583,66.20953;Inherit;True;Property;_TextureSample0;Texture Sample 0;5;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;32;-1039.239,1198.456;Inherit;True;Global;MaskTex1111;Mask Tex1111;11;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector4Node;8;-1446.983,280.2121;Half;False;Property;_MainTexRGBA;Main Tex RGBA;2;0;Create;True;0;0;0;False;0;False;0,0,0,1;0,0,0,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DepthFade;25;-56.17885,1624.038;Inherit;False;True;False;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;76;-992.265,1609.018;Inherit;False;Property;_MaskTexRGBA;Mask Tex RGBA;10;0;Create;True;0;0;0;False;0;False;0,0,0,1;1,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ClampOpNode;28;230.7428,1613.76;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;12;-1159.722,278.0148;Inherit;False;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;75;-693.2282,1452.365;Inherit;False;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;10;-1341.428,575.4772;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;11;-1479.699,-153.7303;Inherit;False;Property;_MainTexColor;Main Tex Color;3;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;0.7490196,0.7490196,0.7490196,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;17;536.5399,526.0021;Inherit;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;499.9799,949.7657;Inherit;False;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;134;1323.26,1105.625;Inherit;False;Property;_BlendMode;Blend Mode;0;1;[Enum];Create;True;0;2;Add;1;Blend;10;0;True;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;131;1264.275,693.3167;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;132;1264.275,693.3167;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;133;1264.275,693.3167;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;129;1264.275,693.3167;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;130;1264.275,693.3167;Float;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;SFX/ASE/Layer/Layer1_mask;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;2;0;True;True;2;5;False;-1;10;True;134;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;Hidden/InternalErrorShader;0;0;Standard;22;Surface;1;  Blend;0;Two Sided;0;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;0;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;0;DOTS Instancing;0;Meta Pass;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;False;True;False;False;;False;0
Node;AmplifyShaderEditor.CommentaryNode;127;-2125,44;Inherit;False;574.8611;271;扰动;0;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;126;-1682.142,1362.031;Inherit;False;504.717;329.3311;扰动;0;;1,1,1,1;0;0
WireConnection;88;0;84;0
WireConnection;88;1;85;0
WireConnection;90;0;88;0
WireConnection;90;1;87;2
WireConnection;89;0;86;0
WireConnection;104;0;102;1
WireConnection;104;1;102;2
WireConnection;92;0;89;0
WireConnection;92;1;91;0
WireConnection;93;0;89;1
WireConnection;103;1;104;0
WireConnection;103;0;90;0
WireConnection;97;0;92;0
WireConnection;97;1;93;0
WireConnection;97;2;103;0
WireConnection;95;0;94;0
WireConnection;69;0;66;0
WireConnection;69;1;67;0
WireConnection;98;0;97;0
WireConnection;98;1;96;0
WireConnection;98;2;95;0
WireConnection;55;0;52;0
WireConnection;55;1;53;0
WireConnection;44;0;47;1
WireConnection;44;1;47;2
WireConnection;72;0;69;0
WireConnection;72;1;68;2
WireConnection;50;0;55;0
WireConnection;50;1;51;2
WireConnection;71;0;70;3
WireConnection;71;1;70;4
WireConnection;99;0;86;0
WireConnection;99;1;98;0
WireConnection;39;0;61;0
WireConnection;23;0;7;0
WireConnection;41;0;39;0
WireConnection;41;1;40;0
WireConnection;62;0;39;1
WireConnection;13;0;23;0
WireConnection;13;1;15;0
WireConnection;73;1;71;0
WireConnection;73;0;72;0
WireConnection;101;0;99;0
WireConnection;101;1;100;0
WireConnection;49;1;44;0
WireConnection;49;0;50;0
WireConnection;106;0;101;0
WireConnection;22;0;19;0
WireConnection;42;0;41;0
WireConnection;42;1;62;0
WireConnection;42;2;73;0
WireConnection;35;0;34;0
WireConnection;24;0;13;0
WireConnection;24;1;23;1
WireConnection;24;2;49;0
WireConnection;37;0;42;0
WireConnection;37;1;36;0
WireConnection;37;2;35;0
WireConnection;20;0;24;0
WireConnection;20;1;16;0
WireConnection;20;2;22;0
WireConnection;110;0;20;0
WireConnection;110;1;111;0
WireConnection;110;2;112;0
WireConnection;108;0;37;0
WireConnection;108;1;107;0
WireConnection;108;2;109;0
WireConnection;9;0;7;0
WireConnection;9;1;110;0
WireConnection;32;0;61;0
WireConnection;32;1;108;0
WireConnection;25;0;26;0
WireConnection;28;0;25;0
WireConnection;12;0;9;0
WireConnection;12;1;8;0
WireConnection;75;0;32;0
WireConnection;75;1;76;0
WireConnection;17;0;11;0
WireConnection;17;1;9;0
WireConnection;17;2;10;0
WireConnection;14;0;11;4
WireConnection;14;1;12;0
WireConnection;14;2;10;4
WireConnection;14;3;75;0
WireConnection;14;4;28;0
WireConnection;130;2;17;0
WireConnection;130;3;14;0
ASEEND*/
//CHKSM=EFC19D0090C2967F189E068113E0ED63E71A86B8