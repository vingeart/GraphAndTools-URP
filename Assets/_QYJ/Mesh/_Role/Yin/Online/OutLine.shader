Shader "Custom/Genshin/outLine"
{
    Properties
    {
        //暴露在材质面板的输入变量

        _MainTex ("Texture", 2D) = "white" {}
        _OutlineWidth ("Outline Width", Range(0, 10)) = 1.0
        _OutlineColor ("Outline Color", color) = (0.5, 0.5, 0.5, 1.0)

    }
    
    //子着色器，如果想在默认管线和URP都能运行的话，可以写一个URP的SubShader和默认管线的SubShader
    SubShader
    {
        //SubShader里设置的标签
        Tags
        {
            "RenderPipeline" = "UniversalPipeline" //把RenderPipeline设置为UniversalPipeline，指定URP来渲染
            //其他的标签与默认管线一样
            "RenderType" = "Opaque"
            "Queue" = "Geometry"
        }
        pass
        {
            //给pass命名，自由命名即可
            Name "OutLine"

            //pass里的标签
            Tags
            {
                //"LightMode" = "UniversalForward"
            }
            
            //设置渲染状态
            Cull Front //剔除正面
            ZTest LEqual //默认就是LEqual
            ZWrite On //默认就是On
            Blend One Zero //设置混合模式,One Zero为直接覆盖
            
            //URP里使用HLSL代码来编写，代码要包含在HLSLPROGRAM和ENDHLSL之间
            HLSLPROGRAM

            //---------------------------------------------------------------------------------

            //编译指令，设置顶点着色器和片元着色器的名称
            #pragma vertex vert
            #pragma fragment frag

            // Unity自带
            #pragma multi_compile_fog

            //---------------------------------------------------------------------------------

            //引用的库文件，不再使用Unity.cginc等
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            //---------------------------------------------------------------------------------
            //顶点着色器的输入结构体，命名保留《入门精要》的风格
            struct a2v
            {
                //为了方便识别各个变量处于什么空间，我之后会以如下规则来命名：
                //后缀:
                //OS：object space，表示模型空间
                //WS：world space,表示世界空间
                //VS：view space，表示视图空间
                //CS：clip space，表示裁剪空间
                float3 positionOS   : POSITION;
                float2 uv           : TEXCOORD0;
                half3 normalOS      : NORMAL;
            };

            //片元着色器的输入结构体
            struct v2f
            {
                float2 uv                       : TEXCOORD0;
                float4 positionCS               : SV_POSITION;
            };

            //cbuffer：constant buffers
            //将输入变量的声明包含在CBUFFER_START和CBUFFER_END之间
            //方便SRP进行批处理
            CBUFFER_START(UnityPerMaterial)
                
                //baseColor
                half4 _BaseColor;
                float4 _MainTex_ST;

                //Outline
                half3 _OutlineColor;
                float _OutlineWidth;

            CBUFFER_END

            //cbuffer的空间是有限的
            //贴图不需要放在cbuffer里
            //在URP里贴图要声明为一张纹理(TEXTURE2D)和一个采样器(SAMPLER)

            //baseColor
            TEXTURE2D(_MainTex);
            SAMPLER(sampler_MainTex);

            //---------------------------------------------------------------------------------

            //---------------------------------------------------------------------------------

            //顶点着色器
            v2f vert (a2v input)
            {
                v2f output;
                
                float4 positionCS = TransformObjectToHClip(input.positionOS.xyz);
                float3 normalVS = mul((float3x3)UNITY_MATRIX_IT_MV, input.normalOS);//把法线变换报视图空间
                float3 normalNDC = mul((float3x3)UNITY_MATRIX_P, normalVS) * positionCS.w;//把法线变换到NDC空间     
                positionCS.xy += 0.01 * _OutlineWidth * normalNDC.xy;//在ndc空间外扩法线

                output.positionCS = positionCS;

                output.uv = TRANSFORM_TEX(input.uv, _MainTex);

                return output;
            }

            //片元着色器，返回值使用half4而不是fixed4
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
