// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "BOXOPHOBIC/Atmospherics/Height Fog Preset"
{
	Properties
	{
		[HideInInspector]_HeightFogPreset("_HeightFogPreset", Float) = 1
		[HideInInspector]_IsHeightFogShader("_IsHeightFogShader", Float) = 1
		[HideInInspector]_IsUniversalPipeline("_IsUniversalPipeline", Float) = 1
		[BBanner(Height Fog, Preset)]_TITLE("< TITLE >", Float) = 1
		[BCategory(Fog)]_FOGG("[ FOGG]", Float) = 1
		_FogIntensity("Fog Intensity", Range( 0 , 1)) = 1
		[Space(10)]_FogColor("Fog Color", Color) = (0.4411765,0.722515,1,1)
		_FogDistanceStart("Fog Distance Start", Float) = 0
		_FogDistanceEnd("Fog Distance End", Float) = 30
		_FogHeightStart("Fog Height Start", Float) = 0
		_FogHeightEnd("Fog Height End", Float) = 5
		[BCategory(Skybox)]_SKYBOXX("[ SKYBOXX ]", Float) = 1
		_SkyboxFogHeight("Skybox Fog Height", Range( 0 , 1)) = 1
		_SkyboxFogFill("Skybox Fog Fill", Range( 0 , 1)) = 0
		[BCategory(Directional)]_DIRECTIONALL("[ DIRECTIONALL ]", Float) = 1
		[Enum(Off,0,On,1)]_DirectionalMode("Directional Mode", Float) = 1
		[HideInInspector]_DirectionalModeBlend("_DirectionalModeBlend", Float) = 1
		_DirectionalIntensity("Directional Intensity", Range( 0 , 1)) = 1
		_DirectionalColor("Directional Color", Color) = (1,0.7793103,0.5,1)
		[BCategory(Noise)]_NOISEE("[ NOISEE ]", Float) = 1
		[Enum(Off,0,Procedural 3D,2)]_NoiseMode("Noise Mode", Float) = 2
		[HideInInspector]_NoiseModeBlend("_NoiseModeBlend", Float) = 1
		_NoiseIntensity("Noise Intensity", Range( 0 , 1)) = 0.5
		_NoiseDistanceEnd("Noise Distance End", Float) = 30
		_NoiseScale("Noise Scale", Float) = 10
		_NoiseSpeed("Noise Speed", Vector) = (0.5,0.5,0,0)
	}

	SubShader
	{
		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		
		Cull Back
		HLSLINCLUDE
		#pragma target 2.0
		ENDHLSL

		
		Pass
		{
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero , One Zero
			ZWrite On
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#define ASE_SRP_VERSION 70102

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag


			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			

			half3 _FogDirectionalDirection;
			CBUFFER_START( UnityPerMaterial )
			half _FogHeightStart;
			half _FogIntensity;
			half _FogHeightEnd;
			half _NoiseIntensity;
			half _FogDistanceStart;
			half3 _NoiseSpeed;
			half _NOISEE;
			half _IsHeightFogShader;
			half _SKYBOXX;
			half _DIRECTIONALL;
			half _FOGG;
			half _DirectionalModeBlend;
			half _HeightFogPreset;
			half _IsUniversalPipeline;
			half4 _DirectionalColor;
			half _FogDistanceEnd;
			half _NoiseDistanceEnd;
			half4 _FogColor;
			half _SkyboxFogFill;
			half _NoiseModeBlend;
			half _TITLE;
			half _DirectionalMode;
			half _NoiseScale;
			half _DirectionalIntensity;
			half _NoiseMode;
			half _SkyboxFogHeight;
			CBUFFER_END

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#ifdef ASE_FOG
				float fogFactor : TEXCOORD0;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			
			VertexOutput vert ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
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

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.vertex.xyz);
				o.clipPos = vertexInput.positionCS;
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( vertexInput.positionCS.z );
				#endif
				return o;
			}

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = float3( 0.5, 0.5, 0.5 );
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#if _AlphaClip
					clip( Alpha - AlphaClipThreshold );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				return half4( Color, Alpha );
			}

			ENDHLSL
		}

	
	}
	
	
	
}
/*ASEBEGIN
Version=17008
-62;1;1906;997;4170.856;5201.434;1;True;False
Node;AmplifyShaderEditor.RangedFloatNode;103;-2336,-4352;Half;False;Property;_FogHeightStart;Fog Height Start;9;0;Create;True;0;0;True;0;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;88;-3328,-3840;Half;False;Property;_SkyboxFogHeight;Skybox Fog Height;12;0;Create;True;0;0;True;0;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;639;-3072,-3712;Half;False;Property;_NoiseMode;Noise Mode;20;1;[Enum];Create;True;2;Off;0;Procedural 3D;2;0;True;0;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;633;-2864,-4096;Half;False;Property;_DirectionalIntensity;Directional Intensity;17;0;Create;True;0;0;True;0;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;230;-2384,-3712;Half;False;Property;_NoiseScale;Noise Scale;24;0;Create;True;0;0;True;0;10;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;640;-3072,-4096;Half;False;Property;_DirectionalMode;Directional Mode;15;1;[Enum];Create;True;2;Off;0;On;1;0;True;0;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;558;-3328,-4864;Half;False;Property;_TITLE;< TITLE >;3;0;Create;True;0;0;True;1;BBanner(Height Fog, Preset);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;635;-2224,-3712;Half;False;Property;_NoiseModeBlend;_NoiseModeBlend;21;1;[HideInInspector];Create;True;0;0;True;0;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;266;-3040,-3840;Half;False;Property;_SkyboxFogFill;Skybox Fog Fill;13;0;Create;True;0;0;True;0;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;137;-3328,-4352;Half;False;Property;_FogColor;Fog Color;6;0;Create;True;0;0;True;1;Space(10);0.4411765,0.722515,1,1;0.4411765,0.722515,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector3Node;625;-1536,-4352;Half;False;Global;_FogDirectionalDirection;_FogDirectionalDirection;24;0;Create;True;0;0;True;0;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;349;-2608,-3712;Half;False;Property;_NoiseDistanceEnd;Noise Distance End;23;0;Create;True;0;0;True;0;30;30;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;102;-3328,-4096;Half;False;Property;_DirectionalColor;Directional Color;18;0;Create;True;0;0;True;0;1,0.7793103,0.5,1;1,0.7793103,0.5,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;642;-2864,-4736;Half;False;Property;_IsUniversalPipeline;_IsUniversalPipeline;2;1;[HideInInspector];Create;False;0;0;True;0;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;107;-2560,-4352;Half;False;Property;_FogDistanceEnd;Fog Distance End;8;0;Create;True;0;0;True;0;30;30;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;634;-2592,-4096;Half;False;Property;_DirectionalModeBlend;_DirectionalModeBlend;16;1;[HideInInspector];Create;True;0;0;True;0;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;278;-3072,-4352;Half;False;Property;_FogIntensity;Fog Intensity;5;0;Create;True;0;0;True;0;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;74;-2128,-4352;Half;False;Property;_FogHeightEnd;Fog Height End;10;0;Create;True;0;0;True;0;5;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;345;-2896,-3712;Half;False;Property;_NoiseIntensity;Noise Intensity;22;0;Create;True;0;0;True;0;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;644;-3328,-4736;Half;False;Property;_HeightFogPreset;_HeightFogPreset;0;1;[HideInInspector];Create;False;0;0;True;0;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;227;-3328,-3712;Half;False;Property;_NoiseSpeed;Noise Speed;25;0;Create;True;0;0;True;0;0.5,0.5,0;0.5,0.5,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;629;-2640,-4864;Half;False;Property;_NOISEE;[ NOISEE ];19;0;Create;True;0;0;True;1;BCategory(Noise);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;106;-2784,-4352;Half;False;Property;_FogDistanceStart;Fog Distance Start;7;0;Create;True;0;0;True;0;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;627;-3024,-4864;Half;False;Property;_SKYBOXX;[ SKYBOXX ];11;0;Create;True;0;0;True;1;BCategory(Skybox);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;628;-2848,-4864;Half;False;Property;_DIRECTIONALL;[ DIRECTIONALL ];14;0;Create;True;0;0;True;1;BCategory(Directional);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;626;-3168,-4864;Half;False;Property;_FOGG;[ FOGG];4;0;Create;True;0;0;True;1;BCategory(Fog);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;632;-2480,-4864;Half;False;Property;_ADVANCEDD;[ ADVANCEDD ];26;0;Create;True;0;0;False;1;BCategory(Advanced);1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;643;-3104,-4736;Half;False;Property;_IsHeightFogShader;_IsHeightFogShader;1;1;[HideInInspector];Create;False;0;0;True;0;1;1;1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;647;-1088,-4352;Float;False;False;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;2;DepthOnly;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;True;False;False;False;False;0;False;-1;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;645;-1088,-4352;Float;False;True;2;;0;3;BOXOPHOBIC/Atmospherics/Height Fog Preset;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;0;Forward;7;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;False;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;10;Surface;0;  Blend;0;Two Sided;1;Cast Shadows;0;Receive Shadows;0;GPU Instancing;0;LOD CrossFade;0;Built-in Fog;0;Meta Pass;0;Vertex Position,InvertActionOnDeselection;1;0;4;True;False;False;False;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;646;-1088,-4352;Float;False;False;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;1;ShadowCaster;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;648;-1088,-4352;Float;False;False;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;3;Meta;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;True;2;False;-1;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.CommentaryNode;557;-3328,-4992;Inherit;False;1022.024;100;Drawers / Settings;0;;1,0.4980392,0,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;612;-3328,-4480;Inherit;False;2427.742;100;Props;0;;0.497,1,0,1;0;0
ASEEND*/
//CHKSM=0D88C398CF1478D5E1889914012E03101108B080