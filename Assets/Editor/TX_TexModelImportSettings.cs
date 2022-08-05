using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
public enum SetGenerateMipMaps
{
    NoSet,
    SetON,
    Setoff
}
public  enum SetWrapMode
{
    NoSet,
    Repeat,
    Clamp
}
public enum SetFilterMode
{
    Point,
    Bilinear,
    Trilinear
}
public enum SetTangents
{
    None,
    Import,
    CalculateLegacy,
    CalculateLegacyWithSplitTangents,
    CalculateMikktspace
}
public class TX_TexModelImportSettings : MonoBehaviour
{
    [Header("设置贴图的Generate Mip Maps")]
    public SetGenerateMipMaps setGenerateMipMaps = SetGenerateMipMaps.NoSet;
    [Header("设置贴图的平铺模式（Wrap Mode）")]
    public SetWrapMode setWrapMode = SetWrapMode.NoSet;
    [Header("设置贴图的质量模式（Filter Mode）")]
    public SetFilterMode setFilterMode = SetFilterMode.Bilinear;
    [Header("判断贴图是否含有Alpha通道或透明像素，自动设置Alpha相关选项")]
    public bool isAutoSetAlpha = true;
    [Header("逐像素检查是否真的有透明像素（比较耗时）")]
    public bool isAutoSetAlpha1 = false;
    [Header("容差范围 (有像素低于此值才被判断为透明贴图)")]
    [Range(0,1)]
    public float alphaRange = 0.95f;
    [Header("设置贴图为2D（Texture Shape）")]
    public bool is2Dtexture = false;
    [Header("关闭模型的灯光、相机等导入")]
    [Space(50)]
    public bool isImports = true;
    [Header("关闭模型的允许读写")]
    public bool isRead_Write = true;
    [Header("Index Format设为16精度")]
    public bool isindexFormat = true;
    [Header("切线设置 Tangents")]
    public SetTangents setTangents = SetTangents.None;
    [Header("不导入材质")]
    public bool ismat = true;
    [Header("不导入动画")]
    public bool isanimation = false;
    List<Texture2D> texture2Ds = new List<Texture2D>();
    List<GameObject> models = new List<GameObject>();
    List<ModelImporter> modelImporters = new List<ModelImporter>();
    List<string> modelStrs = new List<string>();
    List<TextureImporter> textureImporters = new List<TextureImporter>();
    List<string> texStrs = new List<string>();
    private Object[] GetSelectedTextures()
    {
        return Selection.GetFiltered(typeof(Texture2D), SelectionMode.DeepAssets);
    }
    private Object[] GetSelectedGameObj()
    {
        return Selection.GetFiltered(typeof(GameObject), SelectionMode.DeepAssets);
    }
    [ContextMenu("添加选中的模型、贴图")]
    void Add_TXGO()
    {
        Object[] texs = GetSelectedTextures();
        Object[] gos = GetSelectedGameObj();
        foreach (Texture2D texture in texs)
        {
            texture2Ds.Add(texture);
        }
        foreach (GameObject mesh in gos)
        {
            models.Add(mesh);
        }
        for (int i = 0; i < texs.Length; i++)
        {
            Texture2D mod = texs[i] as Texture2D; 
            string path = AssetDatabase.GetAssetPath(mod);
            texStrs.Add(path);
            TextureImporter tex = ModelImporter.GetAtPath(path) as TextureImporter;
            textureImporters.Add(tex);
        }
        for (int i = 0; i < gos.Length; i++)
        {
            GameObject mod = gos[i] as GameObject; 
            string path = AssetDatabase.GetAssetPath(mod);
            modelStrs.Add(path);
            ModelImporter meshsh = ModelImporter.GetAtPath(path) as ModelImporter;
            modelImporters.Add(meshsh);
        }
        Debug.Log("共：" + texture2Ds.Count + "个图片");
        Debug.Log("共：" + models.Count + "个模型");
    }
    [ContextMenu("开始修改")]
    void TexImporterSet()
    {
        OnisAlpha();
        OnisMesh();
    }
    void OnisMesh()
    {
        for (int i = 0; i < modelImporters.Count; i++)
        {
            if (isImports == true)
            {
                modelImporters[i].importBlendShapes = false;
                modelImporters[i].importVisibility = false;
                modelImporters[i].importCameras = false;
                modelImporters[i].importLights = false;
                modelImporters[i].preserveHierarchy = false;
            }
            else
            {
                modelImporters[i].importBlendShapes = true;
                modelImporters[i].importVisibility = true;
                modelImporters[i].importCameras = true;
                modelImporters[i].importLights = true;
            }
            if(isRead_Write == true)
            {
                modelImporters[i].isReadable = false;
            }
            else
            {
                modelImporters[i].isReadable = true;
            }
            if (isindexFormat == true)
            {
                modelImporters[i].indexFormat = ModelImporterIndexFormat.UInt16;
            }
            modelImporters[i].optimizeMesh = true;
            modelImporters[i].weldVertices = true;
            if (setTangents == SetTangents.None)
            {
                modelImporters[i].importTangents = ModelImporterTangents.None;
            }
            if (setTangents == SetTangents.Import)
            {
                modelImporters[i].importTangents = ModelImporterTangents.Import;
            }
            if (setTangents == SetTangents.CalculateMikktspace)
            {
                modelImporters[i].importTangents = ModelImporterTangents.CalculateMikk;
            }
            if (setTangents == SetTangents.CalculateLegacyWithSplitTangents)
            {
                modelImporters[i].importTangents = ModelImporterTangents.CalculateLegacyWithSplitTangents;
            }
            if (setTangents == SetTangents.CalculateLegacy)
            {
                modelImporters[i].importTangents = ModelImporterTangents.CalculateLegacy;
            }
            // if(ismat == true)
            // {
            //     modelImporters[i].materialImportMode = false;
            // }
            // else
            // {
            //     modelImporters[i].materialImportMode = true;
            // }
            if(isanimation == true)
            {
                modelImporters[i].importAnimation = false;
            }
            else
            {
                modelImporters[i].importAnimation = true;
            }
            modelImporters[i].swapUVChannels = false;
            AssetDatabase.ImportAsset(modelStrs[i]);
            AssetDatabase.Refresh();
        }
    }
        void OnisAlpha()
    {
        for (int i = 0; i < textureImporters.Count; i++)
        {
            if (textureImporters[i].textureType == TextureImporterType.Default)
            {
                if (isAutoSetAlpha == true)
                {
                    if(isAutoSetAlpha1 == true)
                    {
                        textureImporters[i].isReadable = true;
                        textureImporters[i].alphaSource = TextureImporterAlphaSource.FromInput;
                        textureImporters[i].alphaIsTransparency = true;
                        AssetDatabase.ImportAsset(texStrs[i]);
                        float h1 = texture2Ds[i].height;
                        float w1 = texture2Ds[i].width;
                        float cishu = 0;
                        for (int a = 0; a < Mathf.Min(h1, w1); a++)
                        {
                            Vector4 v4 = texture2Ds[i].GetPixel((int)Random.Range(0, h1), (int)Random.Range(0, w1));
                            if (v4.w <= alphaRange)
                            {
                                textureImporters[i].alphaSource = TextureImporterAlphaSource.FromInput;
                                textureImporters[i].alphaIsTransparency = true;
                                Debug.Log(texture2Ds[i].name + "透明的");
                                cishu = 0;
                                break;
                            }
                            else
                            {
                                cishu += 1;
                            }
                        }
                        if (cishu == Mathf.Min(h1, w1))
                        {
                            Debug.Log(texture2Ds[i].name + "不透明的");
                            textureImporters[i].alphaIsTransparency = false;
                            textureImporters[i].alphaSource = TextureImporterAlphaSource.None;
                        }
                        cishu = 0;
                    }
                    else
                    {
                        AssetDatabase.ImportAsset(texStrs[i]);
                        if (textureImporters[i].DoesSourceTextureHaveAlpha())
                        {
                            textureImporters[i].alphaSource = TextureImporterAlphaSource.FromInput;
                            textureImporters[i].alphaIsTransparency = true;
                        }
                        else
                        {
                            textureImporters[i].alphaIsTransparency = false;
                            textureImporters[i].alphaSource = TextureImporterAlphaSource.None;
                        }
                    }
                }
                if (setGenerateMipMaps == SetGenerateMipMaps.SetON)
                {
                    textureImporters[i].mipmapEnabled = true;
                }
                if (setGenerateMipMaps == SetGenerateMipMaps.Setoff)
                {
                    textureImporters[i].mipmapEnabled = false;
                }
                if (setWrapMode == SetWrapMode.Repeat)
                {
                    textureImporters[i].wrapMode = TextureWrapMode.Repeat;
                }
                if (setWrapMode == SetWrapMode.Clamp)
                {
                    textureImporters[i].wrapMode = TextureWrapMode.Clamp;
                }
                if(setFilterMode == SetFilterMode.Bilinear)
                {
                    textureImporters[i].filterMode = FilterMode.Bilinear;
                }
                if (setFilterMode == SetFilterMode.Point)
                {
                    textureImporters[i].filterMode = FilterMode.Point;
                }
                if (setFilterMode == SetFilterMode.Trilinear)
                {
                    textureImporters[i].filterMode = FilterMode.Trilinear;
                }
                textureImporters[i].isReadable = false;
                AssetDatabase.ImportAsset(texStrs[i]);
            }
            AssetDatabase.Refresh();
        }
    }
}
