using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ASM_Terminal : MonoBehaviour {
    int lines = 64;



    void OnGUI() {
        if (show) {
            GUI.Box(new Rect(0, 0, Screen.width, Screen.height), "This is a box");
        }
        //Debug.Log("ON GUI");
    }




    // Use this for initialization
    void Start() {

    }

    float timer;
    bool show;

    // Update is called once per frame
    void Update() {
        timer += Time.deltaTime;
        if (timer < 1) { return; }
        timer = 0;
        show = !show;
    }
}
