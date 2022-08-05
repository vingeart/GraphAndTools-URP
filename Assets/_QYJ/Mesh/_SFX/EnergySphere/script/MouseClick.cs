using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MouseClick : MonoBehaviour
{
    private Vector3 mouseVector;
    private float time = 1.0f;
    

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        Renderer render = GetComponent<Renderer>();
        if (Input.GetMouseButtonDown(0))//检测鼠标点击
        {
            Vector3 mousePosition = Input.mousePosition;//鼠标的屏幕空间位置，没有z轴信息
            Ray ray = Camera.main.ScreenPointToRay(mousePosition);//在鼠标位置发射一条射线
            RaycastHit hit;
            if (Physics.Raycast(ray, out hit))//检测与射线相交的物体，物体要有Collider才可以被检测到哦
            {
                mousePosition = hit.point;//与射线相交的位置，世界空间
                mouseVector = this.transform.position - mousePosition;//护盾的原点指向鼠标位置的向量
                mouseVector.Normalize();
                render.sharedMaterial.SetVector("_MouseVector", mouseVector);//传入shader
                time = 0.0f;//重置时间
            }
            
        }
        time += Time.deltaTime;
        render.sharedMaterial.SetFloat("_ClickTime", time);//传入时间

    }
}
