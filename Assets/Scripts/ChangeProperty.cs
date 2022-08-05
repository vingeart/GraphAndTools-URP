using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChangeProperty : MonoBehaviour
{

    public float RimPower;
    public float Width;
    public Color Color;


    void Update()
    {
        //gameObject.GetComponent<MeshRenderer>().material.SetColor("_OutlineColor", new Color(1,0,1,1));

        //gameObject.GetComponent<MeshRenderer>().material.SetFloat("_RimPower", RimPower);

        gameObject.GetComponentInChildren<MeshRenderer>().material.SetFloat("_RimPower", RimPower);

        gameObject.GetComponentInChildren<MeshRenderer>().material.SetFloat("_Width", Width);

        gameObject.GetComponentInChildren<MeshRenderer>().material.SetColor("_Color", Color);
    }
}
