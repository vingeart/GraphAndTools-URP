Shader "VG/Custom/Unlit/ShowNormal"
{
    //变量接口
    Properties
    {
        //线段长度
        _LineLength("Length",Range(0,0.1)) = 0.05
        //线段颜色
        _LineColor("Color",COLOR) = (0,1,0,1)
    }
    //着色器正文
    SubShader
    {
        //着色器程序块
        Pass
        {
            Tags { "RenderType" = "Opaque" }
            LOD 200
 
            CGPROGRAM
                #pragma target 5.0
                //顶点着色器，用于处理位置信息
                #pragma vertex vert
                //片元着色器，用于处理颜色
                #pragma fragment frag
                //几何着色器，DirectX 10新增的，介于顶点和片元着色器之间的可选着色器
                #pragma geometry geo
                //引入空间转换宏
                #include "UnityCG.cginc"
 
                float _LineLength;
                fixed4 _LineColor;
                 
                //输入到顶点着色器的数据
                struct v2g
                {
                    //顶点位置
                    float4    pos       : POSITION;
                    //法线
                    float3    normal    : NORMAL;
                    //纹理坐
                    float2  tex0        : TEXCOORD0;
                };
                //输入到片元着色器的数据
                struct g2f
                {
                    //像素位置
                    float4    pos       : POSITION;
                    //纹理
                    float2  tex0        : TEXCOORD0;
                };
                
                //顶点着色器正文appdata_base是默认输入信息，输出的是经过转化到世界桌标的顶点和法线信息
                v2g vert(appdata_base v)
                {
                    v2g output = (v2g)0;
                    //输出坐标 = 转换成世界坐标的顶点
                    output.pos = mul(unity_ObjectToWorld, v.vertex);
                    //输出坐标 = 转换成裁剪坐标的顶点
                    //output.pos = UnityObjectToClipPos(v.vertex);
                    //输出法线 = 转换后的世界坐标法线
                    float3 worldNormal = UnityObjectToWorldNormal(v.normal);
                    //输出法线 = 转换后的裁剪坐标法线
                    // float4 viewNormal = mul(UNITY_MATRIX_IT_MV, float4(v.normal, 0));
                    output.normal = normalize(worldNormal);
                    //初始化纹理信息
                    output.tex0 = float2(0, 0);
                    return output;
                }
                //几何着色器，放在顶点和片元之间。
                //maxvertexcount设置从顶点着色器到几何着色器每次输出最大顶点数量
                //输入修饰有point、line、triangle、lineadj、triangleadj分别对应点、线、三角形、有临接的线段、有邻接的三角形
			    //inout输出修饰类型有PointStream、LineStream、TriangleStream，分别显示顶点、线段、三角面
                //输入顶点的信息，inout这里是将一个数据传入函数，在函数中的修改会返回到函数外的数据
                //一次接受point 的1个顶点数据，输出为LineStream的4条线段
                [maxvertexcount(4)]
                void geo(point v2g p[1], inout LineStream<g2f> triStream)
                {
                    //初始化一个片元变量
                    g2f pIn;
                    //UNITY_MATRIX_VP：当前的观察投影矩阵，用于将顶点/方向矢量从世界空间变换到裁剪空间
                    //顶点的位置（线段起点） = 从世界坐标转换到裁剪空间的坐标信息
                    pIn.pos = mul(UNITY_MATRIX_VP, p[0].pos);// UnityObjectToClipPos(p[0].pos);
                    // if(pIn.pos)
                    //初始化纹理
                    pIn.tex0 = float2(0.0f, 0.0f);
                    //并入几何渲染器输出流
                    triStream.Append(pIn);

                    //初始化第二个片元变量
                    g2f pIn1;
                    //顶点的位置（线段末端的位置） = 顶点位置 + 法线 * 长度
                    float4 pos = p[0].pos + float4(p[0].normal,0) *_LineLength;
                    //末端顶点位置 = 从世界坐标转换到裁剪空间的坐标信息
                    pIn1.pos = mul(UNITY_MATRIX_VP, pos);
                    //初始化纹理
                    pIn1.tex0 = float2(0.0f, 0.0f);
                    //并入几何渲染器输出流
                    triStream.Append(pIn1);
                }
 
                //片元着色器，用于上色
                fixed4 frag(g2f input) : COLOR
                {
                    return _LineColor;
                }
            ENDCG
        }
    }
}
