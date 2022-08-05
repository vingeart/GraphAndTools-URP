using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEditor;
using UnityEngine;

 
public class ArtToolBox 
{
    [MenuItem("Tools/打组 %G")] // & alt  #shift 
    static void MakeGroup( )
    {
        GameObject group = new GameObject("group");
 
        foreach (var e in Selection.gameObjects.Where(e => e.activeInHierarchy))
        {
            e.transform.parent = group.transform;
        }
    }
}