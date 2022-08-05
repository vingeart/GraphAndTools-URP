using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using UnityEditor;
using UnityEngine;

public class SVNUtils
{
    private static List<string> drives = new List<string>() { "c:", "d:", "e:", "f:"};
    private static string svnPath = @"\Program Files\TortoiseSVN\bin\";
    private static string svnProc = @"TortoiseProc.exe";
    private static string svnProcPath = "";
    
    [MenuItem("Tools/SVN/SVN更新 %&e")]
    public static void UpdateFormSVN()
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        var dir = new DirectoryInfo(Application.dataPath);
        var path = dir.Parent.FullName.Replace('/', '\\');
        var para =  "/command:update /path:\"" + path + "\" /closeonend:0";
        System.Diagnostics.Process.Start(svnProcPath, para);

    }

    [MenuItem("Tools/SVN/SVN提交 %&r")]
    public static void CommitToSVN()
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        var path = Application.dataPath.Replace('/', '\\');
        var para =  "/command:commit /path:\"" + path + "\"";
        System.Diagnostics.Process.Start(svnProcPath, para);

    }

    public static void CommitToSVN(string path)
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        path = path.Replace('/', '\\');
        var para = "/command:commit /path:\"" + path + "\"";
        System.Diagnostics.Process.Start(svnProcPath, para);
    }

    public static void CommitToSVNPath(string target_path = null)
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        var path = "";
        if (string.IsNullOrEmpty(target_path) == true)
        {
            path = Application.dataPath.Replace('/', '\\');
        }
        else
        {
            path = target_path.Replace('/', '\\');
        }
        var para =  "/command:commit /path:\"" + path + "\"";
        System.Diagnostics.Process.Start(svnProcPath, para);
    }

    [MenuItem("Tools/SVN/SVN回滚本地修改 %&t")]
    public static void RevretFormSVN()
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        var path = Application.dataPath.Replace('/', '\\');
        var para =  "/command:revert /path:\"" + path + "\"";
        System.Diagnostics.Process.Start(svnProcPath, para);
    }

    [MenuItem("Tools/SVN/清理SVN %&y")]
    public static void ClanupFromSVN()
    {
        if (string.IsNullOrEmpty(svnProcPath))
            svnProcPath = GetSVNProcPath();
        var path = Application.dataPath.Replace('/', '\\');
        var para =  "/command:cleanup /path:\"" + path + "\"";
        System.Diagnostics.Process.Start(svnProcPath, para);
    }

    private static string GetSVNProcPath()
    {
        foreach (var item in drives)
        {
            var path = string.Concat(item, svnPath, svnProc);
            if (File.Exists(path))
                return path;
        }
        return EditorUtility.OpenFilePanel("Select TortoiseProc.exe", "c:\\", "exe");
    }

}