using UnityEngine;
using System.Collections.Generic;
using System;

public class ASM_Test : MonoBehaviour {
    string[] simple_bios = {
        "org 0x0000",
        "b @reset",
        "b @udef",
        "b @swi",
        "b @irq",

        "@reset:",
        "mov  R1, 0x01",
        "mov  R0, 0x00",
        "b    R1:R0",

        "@udef:",
        "push {R0, R1}",
        "mov  R1, 0x02",
        "mov  R0, 0x00",
        "b    R1:R0",

        "@swi:",
        "push {R0, R1}",
        "mov  R1, 0x03",
        "mov  R0, 0x00",
        "b    R1:R0",

        "@irq:",
        "push {R0, R1}",
        "mov  R1, 0x04",
        "mov  R0, 0x00",
        "b    R1:R0",

        "; -- Reset handler --",
        "org 0x0100",
        "; Enter user mode",
        "mov R1, 0x00",
        "mov R0, 0x10",
        "msr R1:R0",
        "; Init SP",
        "mov R1, 0x30",
        "mov R0, 0x00",
        "mov SP, R1:R0",
        "; Begin user program",
        "mov R1, 0x80",
        "mov R0, 0x00",
        "b   R1:R0",

        "; -- Undefined Instruction Handler --",
        "org 0x0200",
        "; Hang",
        "@loop_udef:",
        "mov R0, R0",
        "b   @loop_udef",

        "; IRQ handler",
        "org 0x0400",
        "push {R2, R3}",
        "mov  R1, 0x30",
        "mov  R0, 0xFE",
        "ldr  R2, [R1:R0]",
        "mov  R0, 0xFF",
        "ldr  R3, [R1:R0]",
        "push PC + 2",
        "b    R3:R2",
        "pop  {R0, R1, R2, R3}",
        "pop  PC",

    };

    
    


    // Use this for initialization
    void Start() {
        ASM_RAM80.Start();

        List<string> comments;
        List<ASM_RAM80.ISlot> instructions;
        List<ASM_RAM80.OSlot> map;

        ASM_RAM80.Assemble(simple_bios, out comments, out instructions, out map);

        for (int i = 0; i < map.Count; ++i) {
            Debug.Log(map[i].address.ToString("X4") + ": " + map[i].opcode.ToString("X4") + " " + instructions[i].instruction);
        }



        //foreach (ASM_RAM80.OSlot slot in map) {
        //    Debug.Log(slot.address.ToString("X4") + ": " + slot.opcode.ToString("X4"));
        //}


        CPU_RAM80 cpu = new CPU_RAM80();
        MCU_RAM80 mcu = new MCU_RAM80();


        cpu.LDR = mcu.LDR;
        cpu.STR = mcu.STR;

       
        


        cpu.Reset();




        /*
        string inst1 = "ADDS   R0, R1, R2 ; add r1 and r2 and store in r0";
        string inst2 = "BEQ    R1:R0";
        string inst3 = "ADDS R5, 1Fh";
        string inst4 = "PUSH {R0, R1 ,R2, R3 ,R5}";
        string inst5 = "ADD SP,-33";
        string inst6 = "MULs R0, 10";
        string comment;
        */

        //uint opcode = ASM_RAM80.Assemble(inst6, out comment);
        //Debug.Log("Opcode: " + opcode.ToString("X4"));
        //Debug.Log("Comment: " + comment);

        //Debug.Log((int)Convert.ToUInt16("FFFF", 16) * -1);

    }

    // Update is called once per frame
    void Update() {

    }
}

/*
 byte[] BIOS = new byte[256];
    byte[] WRAM1 = new byte[256];
    byte[] WRAM2 = new byte[256];
    byte[] ROM = new byte[256];
    byte[] SRAM = new byte[256];
 *
 * 
 */




