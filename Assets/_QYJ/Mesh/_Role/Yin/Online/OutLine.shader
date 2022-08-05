Shader "Custom/Genshin/outLine"
{
    Properties
    {
        //��¶�ڲ��������������

        _MainTex ("Texture", 2D) = "white" {}
        _OutlineWidth ("Outline Width", Range(0, 10)) = 1.0
        _OutlineColor ("Outline Color", color) = (0.5, 0.5, 0.5, 1.0)

    }
    
    //����ɫ�����������Ĭ�Ϲ��ߺ�URP�������еĻ�������дһ��URP��SubShader��Ĭ�Ϲ��ߵ�SubShader
    SubShader
    {
        //SubShader�����õı�ǩ
        Tags
        {
            "RenderPipeline" = "UniversalPipeline" //��RenderPipeline����ΪUniversalPipeline��ָ��URP����Ⱦ
            //�����ı�ǩ��Ĭ�Ϲ���һ��
            "RenderType" = "Opaque"
            "Queue" = "Geometry"
        }
        pass
        {
            //��pass������������������
            Name "OutLine"

            //pass��ı�ǩ
            Tags
            {
                //"LightMode" = "UniversalForward"
            }
            
            //������Ⱦ״̬
            Cull Front //�޳�����
            ZTest LEqual //Ĭ�Ͼ���LEqual
            ZWrite On //Ĭ�Ͼ���On
            Blend One Zero //���û��ģʽ,One ZeroΪֱ�Ӹ���
            
            //URP��ʹ��HLSL��������д������Ҫ������HLSLPROGRAM��ENDHLSL֮��
            HLSLPROGRAM

            //---------------------------------------------------------------------------------

            //����ָ����ö�����ɫ����ƬԪ��ɫ��������
            #pragma vertex vert
            #pragma fragment frag

            // Unity�Դ�
            #pragma multi_compile_fog

            //---------------------------------------------------------------------------------

            //���õĿ��ļ�������ʹ��Unity.cginc��
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            //---------------------------------------------------------------------------------
            //������ɫ��������ṹ�壬�������������ž�Ҫ���ķ��
            struct a2v
            {
                //Ϊ�˷���ʶ�������������ʲô�ռ䣬��֮��������¹�����������
                //��׺:
                //OS��object space����ʾģ�Ϳռ�
                //WS��world space,��ʾ����ռ�
                //VS��view space����ʾ��ͼ�ռ�
                //CS��clip space����ʾ�ü��ռ�
                float3 positionOS   : POSITION;
                float2 uv           : TEXCOORD0;
                half3 normalOS      : NORMAL;
            };

            //ƬԪ��ɫ��������ṹ��
            struct v2f
            {
                float2 uv                       : TEXCOORD0;
                float4 positionCS               : SV_POSITION;
            };

            //cbuffer��constant buffers
            //���������������������CBUFFER_START��CBUFFER_END֮��
            //����SRP����������
            CBUFFER_START(UnityPerMaterial)
                
                //baseColor
                half4 _BaseColor;
                float4 _MainTex_ST;

                //Outline
                half3 _OutlineColor;
                float _OutlineWidth;

            CBUFFER_END

            //cbuffer�Ŀռ������޵�
            //��ͼ����Ҫ����cbuffer��
            //��URP����ͼҪ����Ϊһ������(TEXTURE2D)��һ��������(SAMPLER)

            //baseColor
            TEXTURE2D(_MainTex);
            SAMPLER(sampler_MainTex);

            //---------------------------------------------------------------------------------

            //---------------------------------------------------------------------------------

            //������ɫ��
            v2f vert (a2v input)
            {
                v2f output;
                
                float4 positionCS = TransformObjectToHClip(input.positionOS.xyz);
                float3 normalVS = mul((float3x3)UNITY_MATRIX_IT_MV, input.normalOS);//�ѷ��߱任����ͼ�ռ�
                float3 normalNDC = mul((float3x3)UNITY_MATRIX_P, normalVS) * positionCS.w;//�ѷ��߱任��NDC�ռ�     
                positionCS.xy += 0.01 * _OutlineWidth * normalNDC.xy;//��ndc�ռ���������

                output.positionCS = positionCS;

                output.uv = TRANSFORM_TEX(input.uv, _MainTex);

                return output;
            }

            //ƬԪ��ɫ��������ֵʹ��half4������fixed4
            half4 frag (v2f input) : SV_Target
            {

                half3 albedo = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, input.uv);

                half3 color = albedo * _OutlineColor;              

                return float4(color,1.0);
            }

            //---------------------------------------------------------------------------------
            ENDHLSL
        }
    }
}
