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
        if (Input.GetMouseButtonDown(0))//��������
        {
            Vector3 mousePosition = Input.mousePosition;//������Ļ�ռ�λ�ã�û��z����Ϣ
            Ray ray = Camera.main.ScreenPointToRay(mousePosition);//�����λ�÷���һ������
            RaycastHit hit;
            if (Physics.Raycast(ray, out hit))//����������ཻ�����壬����Ҫ��Collider�ſ��Ա���⵽Ŷ
            {
                mousePosition = hit.point;//�������ཻ��λ�ã�����ռ�
                mouseVector = this.transform.position - mousePosition;//���ܵ�ԭ��ָ�����λ�õ�����
                mouseVector.Normalize();
                render.sharedMaterial.SetVector("_MouseVector", mouseVector);//����shader
                time = 0.0f;//����ʱ��
            }
            
        }
        time += Time.deltaTime;
        render.sharedMaterial.SetFloat("_ClickTime", time);//����ʱ��

    }
}
