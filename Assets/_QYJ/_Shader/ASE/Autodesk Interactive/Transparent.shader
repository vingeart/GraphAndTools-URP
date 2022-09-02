// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Art/Effect/Transparent"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][Header(Base)][Enum(UnityEngine.Rendering.CullMode)][Space]_CullMode("Cull Mode", Int) = 2
		[Enum(Add,1,Alpha,10)]_BlendDst("Blend Dst", Int) = 1
		_AlphaControl("Alpha Control", Range( 0 , 1)) = 1
		[HDR]_Color("Color", Color) = (1,1,1,1)
		[HDR]_ShadeColor("Shade Color", Color) = (1,1,1,1)
		_ShadeRange("Shade Range", Float) = 0
		[Enum(Base R,0,Base A,1)]_ColorLerpChannel("Color Lerp Channel", Int) = 0
		[Header(Dissolve)][Space][Toggle(_USEDISSOLVE_ON)] _UseDissolve("Use Dissolve", Float) = 0
		[Header(Mask)][Space][Toggle(_USEMASK_ON)] _UseMask("Use Mask", Float) = 0
		[Header(Rim)][Space][Toggle(_USERIM_ON)] _UseRim("Use Rim", Float) = 0
		[Header(Render)][Space][Toggle(_DEPTHFADE_ON)] _DepthFade("Depth Fade", Float) = 0
		_DepthFadeDistance("Depth Fade Distance", Float) = 1
		[Enum(Off,0,On,1)]_ZWriteMode("ZWrite Mode", Int) = 0
		[Enum(UnityEngine.Rendering.CompareFunction)]_ZTestMode("ZTest Mode", Int) = 4
		[ASEEnd][Enum(Off,0,On,3)]_EffectStencilBuffer("Effect Stencil Buffer", Float) = 0

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
		
		Cull [_CullMode]
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
			
			Blend SrcAlpha [_BlendDst], One OneMinusSrcAlpha
			ZWrite [_ZWriteMode]
			ZTest [_ZTestMode]
			Offset 0,0
			ColorMask RGBA
			Stencil
			{
				Ref [_EffectStencilBuffer]
				Comp [_EffectStencilBuffer]
				Pass Keep
				Fail Keep
				ZFail Keep
			}

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
			#pragma shader_feature_local _USEDISSOLVE_ON
			#pragma shader_feature_local _USERIM_ON
			#pragma shader_feature_local _USEMASK_ON
			#pragma shader_feature_local _DEPTHFADE_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
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
				float4 ase_color : COLOR;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _ShadeColor;
			float4 _Color;
			int _BlendDst;
			int _ZWriteMode;
			int _CullMode;
			int _ZTestMode;
			float _EffectStencilBuffer;
			int _ColorLerpChannel;
			float _ShadeRange;
			float _AlphaControl;
			float _DepthFadeDistance;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			uniform float4 _CameraDepthTexture_TexelSize;


						
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
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
				float4 baseMap34 = float4( 0,0,0,0 );
				float4 break230 = baseMap34;
				float lerpResult228 = lerp( break230.x , break230.w , (float)_ColorLerpChannel);
				float4 lerpResult26 = lerp( _ShadeColor , _Color , saturate( ( lerpResult228 - _ShadeRange ) ));
				float4 colorMap85 = ( lerpResult26 * baseMap34 );
				#ifdef _USEMASK_ON
				float4 staticSwitch59 = float4( 0,0,0,0 );
				#else
				float4 staticSwitch59 = float4(1,1,1,1);
				#endif
				float4 maskMap77 = staticSwitch59;
				float4 appendResult160 = (float4(IN.ase_color.r , IN.ase_color.g , IN.ase_color.b , 1.0));
				float4 mixMap84 = ( ( colorMap85 * maskMap77 ) * appendResult160 );
				#ifdef _USERIM_ON
				float4 staticSwitch141 = float4( 0,0,0,0 );
				#else
				float4 staticSwitch141 = mixMap84;
				#endif
				float4 finalMap150 = staticSwitch141;
				float4 break200 = finalMap150;
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float screenDepth186 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				float distanceDepth186 = saturate( abs( ( screenDepth186 - LinearEyeDepth( ase_screenPosNorm.z,_ZBufferParams ) ) / ( _DepthFadeDistance ) ) );
				#ifdef _DEPTHFADE_ON
				float staticSwitch188 = distanceDepth186;
				#else
				float staticSwitch188 = 1.0;
				#endif
				float alphaControl124 = ( IN.ase_color.a * _AlphaControl * staticSwitch188 );
				float4 appendResult201 = (float4(break200.r , break200.g , break200.b , ( break200.a * alphaControl124 )));
				#ifdef _USEDISSOLVE_ON
				float4 staticSwitch103 = float4( 0,0,0,0 );
				#else
				float4 staticSwitch103 = appendResult201;
				#endif
				float4 colorOutput134 = staticSwitch103;
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = (colorOutput134).xyz;
				float Alpha = (colorOutput134).w;
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
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "//Unlit/Transparent"
	
}
/*ASEBEGIN
Version=18900
525;294;1394;705;5004.4;-237.0999;1.343904;True;False
Node;AmplifyShaderEditor.CommentaryNode;246;-15138.84,-44.10397;Inherit;False;3955.555;682.1085;Comment;30;39;42;41;239;237;54;243;235;242;244;233;51;47;50;49;7;55;9;11;8;12;16;15;21;23;13;28;34;10;252;Base;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;204;-10568.94,10.88903;Inherit;False;1698.401;666.2404;Comment;13;231;203;230;228;30;85;33;36;26;32;25;24;31;Color;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;34;-11407.29,261.3402;Inherit;False;baseMap;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;203;-10517.36,380.4084;Inherit;False;34;baseMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.BreakToComponentsNode;230;-10307.8,384.8458;Inherit;False;FLOAT4;1;0;FLOAT4;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.IntNode;231;-10384.51,551.0797;Inherit;False;Property;_ColorLerpChannel;Color Lerp Channel;6;1;[Enum];Create;True;0;2;Base R;0;Base A;1;0;False;0;False;0;1;False;0;1;INT;0
Node;AmplifyShaderEditor.LerpOp;228;-10104.8,401.8458;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;30;-10114.27,545.5239;Inherit;False;Property;_ShadeRange;Shade Range;5;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;31;-9899.272,467.5238;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;80;-11303.45,1007.793;Inherit;False;2406.198;843.3212;Comment;19;64;78;59;70;68;66;71;75;73;74;72;67;65;63;61;58;60;62;77;Mask;1,1,1,1;0;0
Node;AmplifyShaderEditor.ColorNode;24;-9809.035,250.3378;Inherit;False;Property;_Color;Color;3;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;32;-9741.272,467.5238;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;25;-9804.564,60.88929;Inherit;False;Property;_ShadeColor;Shade Color;4;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector4Node;78;-9622.339,1057.793;Inherit;False;Constant;_Vector1;Vector 1;20;0;Create;True;0;0;0;False;0;False;1,1,1,1;0,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;36;-9499.392,394.1619;Inherit;False;34;baseMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;26;-9485.509,231.8486;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;33;-9264.888,288.1756;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;59;-9364.797,1126.488;Inherit;False;Property;_UseMask;Use Mask;29;0;Create;True;0;0;0;False;2;Header(Mask);Space;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;191;-8638.045,-25.09332;Inherit;False;1485.253;804.9821;Comment;14;84;121;81;160;82;83;122;123;124;189;188;190;186;232;Mix;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;85;-9094.535,272.8269;Inherit;False;colorMap;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;77;-9121.254,1116.15;Inherit;False;maskMap;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;81;-7979.119,41.01543;Inherit;False;85;colorMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.VertexColorNode;121;-8011.582,250.8465;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;82;-7978.32,141.0147;Inherit;False;77;maskMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;160;-7761.823,247.0133;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;232;-7763.94,73.78431;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;83;-7551.591,117.5639;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;198;-8299.006,1029.66;Inherit;False;1117.156;760.6466;Comment;10;144;150;141;143;147;149;145;148;151;146;Rim;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;84;-7378.792,113.537;Inherit;False;mixMap;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;190;-8588.045,663.3072;Inherit;False;Property;_DepthFadeDistance;Depth Fade Distance;44;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;189;-8225.262,542.4962;Inherit;False;Constant;_Float1;Float 1;41;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DepthFade;186;-8333.332,644.8872;Inherit;False;True;True;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;143;-7862.594,1156.661;Inherit;False;84;mixMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;122;-8118.404,443.1584;Inherit;False;Property;_AlphaControl;Alpha Control;2;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;188;-8061.262,572.4962;Inherit;False;Property;_DepthFade;Depth Fade;43;0;Create;True;0;0;0;False;2;Header(Render);Space;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;141;-7622.342,1213.743;Inherit;False;Property;_UseRim;Use Rim;37;0;Create;True;0;0;0;False;2;Header(Rim);Space;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;150;-7405.844,1212.792;Inherit;False;finalMap;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;123;-7761.027,423.8005;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;185;-6919.131,75.62024;Inherit;False;2302.415;1697.498;Comment;34;134;103;196;201;202;136;200;138;170;132;130;157;126;131;115;127;116;177;117;112;109;173;175;107;156;179;120;111;118;172;110;108;119;171;Dissolve;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;136;-5877.409,428.1309;Inherit;False;150;finalMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;124;-7577.027,417.8005;Inherit;False;alphaControl;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;196;-5748.505,627.6738;Inherit;False;124;alphaControl;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;200;-5691.553,433.1397;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;202;-5551.553,562.1398;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;201;-5402.553,434.1397;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StaticSwitch;103;-5108.107,719.5187;Inherit;False;Property;_UseDissolve;Use Dissolve;16;0;Create;True;0;0;0;False;2;Header(Dissolve);Space;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;155;-4428.24,473.2606;Inherit;False;863.4709;514.7211;Comment;9;1;37;154;86;6;259;5;262;264;Output;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;134;-4858.562,719.2089;Inherit;False;colorOutput;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;86;-4381.24,783.7305;Inherit;False;134;colorOutput;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;102;-13810.33,1032.135;Inherit;False;2182.241;1112.35;Comment;22;53;251;250;52;87;91;101;92;194;90;195;89;96;193;192;94;97;93;98;99;254;256;UV Distort;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;23;-11912.15,408.7764;Inherit;False;Property;_BasePower;Base Power;10;0;Create;True;0;0;0;False;0;False;1;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;111;-6602.386,1114.259;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;175;-6508.375,337.7007;Inherit;False;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.SaturateNode;173;-6497.555,563.947;Inherit;False;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.IntNode;107;-6561.156,1318.173;Inherit;False;Property;_AlphaMask;Alpha Mask;18;1;[Enum];Create;True;0;3;Final;0;Base;1;One;2;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.IntNode;116;-6518.455,928.1046;Inherit;False;Property;_DissolveMask;Dissolve Mask;19;1;[Enum];Create;True;0;2;Final Alpha;0;Mask Alpha;1;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;179;-6552.669,439.3428;Inherit;False;124;alphaControl;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;97;-13487.33,1632.094;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ComponentMaskNode;117;-6577.371,719.6718;Inherit;False;False;False;False;True;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;94;-13567.33,1483.094;Inherit;False;0;89;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;110;-6806.386,1114.259;Inherit;False;34;baseMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;119;-6782.622,819.1776;Inherit;False;77;maskMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;177;-6546.981,260.5627;Inherit;False;124;alphaControl;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;172;-6667.555,563.947;Inherit;False;2;0;INT;0;False;1;INT;1;False;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;108;-6805.753,1021.885;Inherit;False;150;finalMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;194;-13255.17,1698.38;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PannerNode;96;-13274.33,1488.094;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;89;-13322.89,1230.863;Inherit;True;Property;_DistortionMap;Distortion Map;24;0;Create;True;0;0;0;False;0;False;None;None;True;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.IntNode;171;-6869.131,333.4788;Inherit;False;Property;_DissolveType;Dissolve Type;17;1;[Enum];Create;True;0;3;Hard;2;Soft;1;Texcoord1x;0;0;False;0;False;2;2;False;0;1;INT;0
Node;AmplifyShaderEditor.Vector3Node;53;-12804.88,1163.335;Inherit;False;Constant;_Vector0;Vector 0;12;0;Create;True;0;0;0;False;0;False;0,0,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SamplerNode;91;-12942.68,1333.344;Inherit;True;Property;_TextureSample5;Texture Sample 5;22;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;250;-12575.19,1228.662;Inherit;False;Property;_Keyword1;Keyword 0;46;0;Create;True;0;0;0;False;0;False;0;0;0;False;_DISTORTIONTYPE_NORMAL;Toggle;2;Key0;Key1;Fetch;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;39;-15084.28,85.22729;Inherit;False;0;7;4;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;42;-14810.31,144.5675;Inherit;False;False;False;True;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;90;-12946.42,1560.292;Inherit;True;Property;_TextureSample4;Texture Sample 4;22;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;118;-6781.371,719.6718;Inherit;False;150;finalMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;13;-11901.59,181.6414;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;156;-6551.816,125.6205;Inherit;False;1;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;109;-6601.753,1021.885;Inherit;False;False;False;False;True;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;154;-4126.854,741.2025;Inherit;False;True;True;True;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;262;-3988.457,617.4279;Inherit;False;Property;_EffectStencilBuffer;Effect Stencil Buffer;47;1;[Enum];Create;True;0;2;Off;0;On;3;0;True;0;False;0;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;138;-6306.82,1661.554;Inherit;False;Property;_TransparentBlend;Transparent Blend;42;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;264;-4135.835,522.2186;Inherit;False;Property;_ZTestMode;ZTest Mode;46;1;[Enum];Create;True;0;0;1;UnityEngine.Rendering.CompareFunction;True;0;False;4;4;False;0;1;INT;0
Node;AmplifyShaderEditor.ComponentMaskNode;37;-4126.986,846.0304;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;5;-3751.954,615.9634;Inherit;False;Property;_CullMode;Cull Mode;0;2;[Header];[Enum];Create;True;1;Base;0;1;UnityEngine.Rendering.CullMode;True;1;Space;False;2;2;False;0;1;INT;0
Node;AmplifyShaderEditor.IntNode;259;-3945.172,522.8046;Inherit;False;Property;_ZWriteMode;ZWrite Mode;45;1;[Enum];Create;True;1;Render;2;Off;0;On;1;0;True;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.IntNode;6;-3748.113,523.2604;Inherit;False;Property;_BlendDst;Blend Dst;1;1;[Enum];Create;True;0;2;Add;1;Alpha;10;0;True;0;False;1;10;False;0;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;99;-13760.33,1691.094;Inherit;False;Property;_DistortionVSpeed;Distortion V Speed;26;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;98;-13760.33,1599.094;Inherit;False;Property;_DistortionUSpeed;Distortion U Speed;25;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;93;-13762.7,1804.323;Inherit;False;Property;_DistortionIntensity;Distortion Intensity;28;0;Create;True;0;0;0;False;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;120;-6573.423,818.5778;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;192;-13753.92,1909.362;Inherit;False;1;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;227;-5818.145,750.3439;Inherit;False;DissolveMap;-1;;21;;0;0;0
Node;AmplifyShaderEditor.RangedFloatNode;195;-13546.14,2007.396;Inherit;False;Property;_UseTexcoord1y;Use Texcoord1.y;27;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;127;-6312.91,1379.103;Inherit;False;Property;_DissolveColor;Dissolve Color;20;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;115;-6269.522,796.8197;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;126;-6271.91,1291.103;Inherit;False;150;finalMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;130;-6310.156,1027.385;Inherit;False;LerpTwiceFloat;-1;;13;;0;0;0
Node;AmplifyShaderEditor.LerpOp;170;-6275.271,475.284;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;131;-6264.954,1200.194;Inherit;False;Property;_EdgePower;Edge Power;21;0;Create;True;0;0;0;False;0;False;4;4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;132;-6268.256,1562.028;Inherit;False;Property;_EdgeOffset;Edge Offset;22;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;157;-6279.225,241.6806;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;112;-6543.434,1204.604;Inherit;False;Constant;_Float0;Float 0;27;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;193;-13487.46,1864.072;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;41;-14811.69,46.87637;Inherit;False;True;True;False;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;147;-7895.505,1290.791;Inherit;False;RimBlend;-1;;12;;0;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;144;-8195.231,1401.029;Inherit;False;251;normalMap;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;65;-9916.991,1379.826;Inherit;False;Property;_MaskScale;Mask Scale;32;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;61;-10123.43,1375.482;Inherit;False;Property;_MaskMode;Mask Mode;31;1;[Enum];Create;True;0;2;R;0;RGBA;1;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.DynamicAppendNode;63;-10107.31,1145.359;Inherit;False;FLOAT4;4;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;49;-13517.86,362.4036;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;60;-10434.83,1261.481;Inherit;True;Property;_TextureSample3;Texture Sample 3;14;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;55;-13241.54,239.3075;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;7;-13296.95,5.896028;Inherit;True;Property;_BaseMap;Base Map;7;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;8;-12930.86,6.413026;Inherit;True;Property;_TextureSample0;Texture Sample 0;3;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PannerNode;72;-10718.29,1385.115;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;58;-10764.24,1176.749;Inherit;True;Property;_MaskMap;Mask Map;30;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;11;-12937.52,333.2033;Inherit;True;Property;_TextureSample2;Texture Sample 2;3;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;251;-12270.82,1227.839;Inherit;False;normalMap;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;9;-12826.86,224.6732;Inherit;False;FlipbookSample;-1;;3;;0;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;75;-10922.29,1581.115;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;70;-11178.2,1550.281;Inherit;False;Property;_UseUV2;Use UV2;34;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;68;-11252.05,1403.969;Inherit;False;1;58;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;12;-12499.57,198.7713;Inherit;False;Property;_BaseUV;Base UV;11;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;Default;FlipbookBlending;Matcap;Create;True;True;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;73;-11214.29,1644.114;Inherit;False;Property;_MaskUSpeed;Mask U Speed;35;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;67;-11253.45,1247.569;Inherit;False;0;58;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.IntNode;15;-12183.79,289.8404;Inherit;False;Property;_BaseAlphaChannel;Base Alpha Channel;8;1;[Enum];Create;True;0;2;R;0;A;1;0;False;0;False;0;1;False;0;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;74;-11215.29,1735.114;Inherit;False;Property;_MaskVSpeed;Mask V Speed;36;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SwizzleNode;16;-12140.79,151.7411;Inherit;False;FLOAT4;0;1;2;0;1;0;COLOR;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.FunctionNode;28;-11652.68,266.5433;Inherit;False;ScaleAndPower;-1;;4;;0;0;0
Node;AmplifyShaderEditor.RangedFloatNode;21;-11902.15,319.7764;Inherit;False;Property;_BaseScale;Base Scale;9;0;Create;True;0;0;0;False;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;71;-10938.43,1385.042;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;54;-13527.06,163.734;Inherit;False;52;bumpMap;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.LerpOp;62;-9913.486,1242.236;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;50;-13773.19,482.7679;Inherit;False;Property;_ClampBaseUV;Clamp Base UV;12;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;146;-8173.274,1593.763;Inherit;False;Property;_RimScale;Rim Scale;40;0;Create;True;0;0;0;False;0;False;1;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;-8196.563,1120.011;Inherit;False;84;mixMap;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;145;-8235.776,1206.963;Inherit;False;Property;_RimColor;Rim Color;38;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;0.7490196,0.7490196,0.7490196,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;149;-8183.873,1679.163;Inherit;False;Property;_InverseRim;Inverse Rim;39;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;148;-8177.075,1506.563;Inherit;False;Property;_RimPower;Rim Power;41;0;Create;True;0;0;0;False;0;False;5;4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;239;-14562.06,124.7727;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ComponentMaskNode;92;-12635.17,1560.471;Inherit;False;True;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;252;-14571.01,340.0376;Inherit;False;251;normalMap;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RelayNode;101;-12848.78,1782.747;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;237;-14640.68,238.1633;Inherit;False;Property;_UseTexcoord0zw;Use Texcoord0.zw;13;1;[Toggle];Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;66;-9920.894,1470.826;Inherit;False;Property;_MaskPower;Mask Power;33;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;235;-14344.31,52.48287;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;254;-12390.97,1634.841;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;10;-14367.58,344.3178;Inherit;False;MatcapUV;-1;;2;;0;0;0
Node;AmplifyShaderEditor.Vector2Node;256;-12399.77,1470.324;Inherit;False;Constant;_Vector2;Vector 2;46;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;242;-14205.79,437.0775;Inherit;False;Property;_BaseUSpeed;Base U Speed;14;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;244;-14011.65,466.0045;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;87;-12170.08,1470.129;Inherit;False;Property;_DistortionType;Distortion Type;23;0;Create;True;0;0;0;False;2;Header(UV Distortion);Space;False;0;0;0;True;;KeywordEnum;3;None;Simple;Normal;Create;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;233;-14103.8,233.6682;Inherit;False;Property;_BASEUV_MATCAP1;_BASEUV_MATCAP;39;0;Create;True;0;0;0;False;0;False;0;0;0;False;_BASEUV_MATCAP;Toggle;2;Key0;Key1;Fetch;True;True;9;1;FLOAT2;0,0;False;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;6;FLOAT2;0,0;False;7;FLOAT2;0,0;False;8;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PannerNode;47;-13756.06,239.5881;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;64;-9659.717,1242.726;Inherit;False;ScaleAndPower;-1;;5;;0;0;0
Node;AmplifyShaderEditor.SaturateNode;51;-13736.92,386.0677;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;52;-11924.18,1471.024;Inherit;False;bumpMap;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;243;-14203.65,522.0045;Inherit;False;Property;_BaseVSpeed;Base V Speed;15;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;-3856.771,779.9815;Half;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;Art/Effect/Transparent;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;True;5;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;2;0;False;True;2;5;False;-1;10;True;6;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;True;255;True;262;255;False;-1;255;False;-1;7;True;262;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;True;2;True;259;True;3;True;264;True;False;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;//Unlit/Transparent;0;0;Standard;22;Surface;1;  Blend;0;Two Sided;1;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;0;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;0;DOTS Instancing;0;Meta Pass;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;False;False;False;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;230;0;203;0
WireConnection;228;0;230;0
WireConnection;228;1;230;3
WireConnection;228;2;231;0
WireConnection;31;0;228;0
WireConnection;31;1;30;0
WireConnection;32;0;31;0
WireConnection;26;0;25;0
WireConnection;26;1;24;0
WireConnection;26;2;32;0
WireConnection;33;0;26;0
WireConnection;33;1;36;0
WireConnection;59;1;78;0
WireConnection;85;0;33;0
WireConnection;77;0;59;0
WireConnection;160;0;121;1
WireConnection;160;1;121;2
WireConnection;160;2;121;3
WireConnection;232;0;81;0
WireConnection;232;1;82;0
WireConnection;83;0;232;0
WireConnection;83;1;160;0
WireConnection;84;0;83;0
WireConnection;186;0;190;0
WireConnection;188;1;189;0
WireConnection;188;0;186;0
WireConnection;141;1;143;0
WireConnection;150;0;141;0
WireConnection;123;0;121;4
WireConnection;123;1;122;0
WireConnection;123;2;188;0
WireConnection;124;0;123;0
WireConnection;200;0;136;0
WireConnection;202;0;200;3
WireConnection;202;1;196;0
WireConnection;201;0;200;0
WireConnection;201;1;200;1
WireConnection;201;2;200;2
WireConnection;201;3;202;0
WireConnection;103;1;201;0
WireConnection;134;0;103;0
WireConnection;111;0;110;0
WireConnection;175;0;171;0
WireConnection;173;0;172;0
WireConnection;97;0;98;0
WireConnection;97;1;99;0
WireConnection;117;0;118;0
WireConnection;172;0;171;0
WireConnection;194;0;93;0
WireConnection;194;1;193;0
WireConnection;194;2;195;0
WireConnection;96;0;94;0
WireConnection;96;2;97;0
WireConnection;91;0;89;0
WireConnection;91;1;96;0
WireConnection;91;5;194;0
WireConnection;250;1;53;0
WireConnection;250;0;91;0
WireConnection;42;0;39;0
WireConnection;90;0;89;0
WireConnection;90;1;96;0
WireConnection;13;0;16;0
WireConnection;13;1;12;0
WireConnection;13;2;15;0
WireConnection;109;0;108;0
WireConnection;154;0;86;0
WireConnection;37;0;86;0
WireConnection;120;0;119;0
WireConnection;115;0;117;0
WireConnection;115;1;120;0
WireConnection;115;2;116;0
WireConnection;170;0;179;0
WireConnection;170;2;173;0
WireConnection;157;0;156;1
WireConnection;157;1;177;0
WireConnection;157;2;175;0
WireConnection;193;0;93;0
WireConnection;193;1;192;2
WireConnection;41;0;39;0
WireConnection;63;3;60;1
WireConnection;49;0;47;0
WireConnection;49;1;51;0
WireConnection;49;2;50;0
WireConnection;60;0;58;0
WireConnection;60;1;72;0
WireConnection;55;0;54;0
WireConnection;55;1;49;0
WireConnection;8;0;7;0
WireConnection;8;1;55;0
WireConnection;72;0;71;0
WireConnection;72;2;75;0
WireConnection;11;0;7;0
WireConnection;11;1;49;0
WireConnection;251;0;250;0
WireConnection;75;0;73;0
WireConnection;75;1;74;0
WireConnection;12;1;8;0
WireConnection;12;2;11;0
WireConnection;16;0;12;0
WireConnection;71;0;67;0
WireConnection;71;1;68;0
WireConnection;71;2;70;0
WireConnection;62;0;63;0
WireConnection;62;1;60;0
WireConnection;62;2;61;0
WireConnection;239;0;41;0
WireConnection;239;1;42;0
WireConnection;92;0;90;0
WireConnection;101;0;194;0
WireConnection;235;0;41;0
WireConnection;235;1;239;0
WireConnection;235;2;237;0
WireConnection;254;0;92;0
WireConnection;254;1;90;4
WireConnection;254;2;101;0
WireConnection;244;0;242;0
WireConnection;244;1;243;0
WireConnection;87;1;256;0
WireConnection;87;0;254;0
WireConnection;87;2;256;0
WireConnection;233;1;235;0
WireConnection;47;0;233;0
WireConnection;47;2;244;0
WireConnection;51;0;233;0
WireConnection;52;0;87;0
WireConnection;1;2;154;0
WireConnection;1;3;37;0
ASEEND*/
//CHKSM=B18D9F9F86A74D93C3FE12C93D680DDD0D746FD6