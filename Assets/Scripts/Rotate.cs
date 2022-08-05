using UnityEngine;
using System.Collections;

public class Rotate : MonoBehaviour
{
    public float Speed = 30;

    void Update()
    {
        this.transform.Rotate(Vector3.up, Time.deltaTime * Speed, Space.World);
    }
}