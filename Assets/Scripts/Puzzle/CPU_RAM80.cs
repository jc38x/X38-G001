//*****************************************************************************
// ASM_RAM80
// jc38x (jcds38x@gmail.com)
//*****************************************************************************

using System;

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Instruction Set
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// --------------------------------------------------------------------
// Format 0-1: ALU operations (Register-Register)
// 00 OPC       S RD  RO1 RO2
//    0000  ADD X XXX XXX XXX
//    0001  SUB X XXX XXX XXX
//    0010   OR X XXX XXX XXX
//    0011  XOR X XXX XXX XXX
//    0100  AND X XXX XXX XXX
//    0101  ADC X XXX XXX XXX
//    0110  SBC X XXX XXX XXX
//    0111  BIC X XXX XXX XXX
//    1000  LSL X XXX XXX XXX
//    1001  LSR X XXX XXX XXX
//    1010  ASR X XXX XXX XXX
//                AOP RO1 RO2
//    1011  CMP 1 000 XXX XXX 
//          CMN 1 001 XXX XXX 
//          TST 1 010 XXX XXX 
//          TEQ 1 011 XXX XXX
//              S     RD  RO
//          NEG X 100 XXX XXX
//         SWAP X 101 XXX XXX
//         RBCD X 110 XXX XXX
//         BCDR X 111 XXX XXX
//                    RDH RDL
//         XCHG 0 000 XXX XXX
//                    RO  IU3
//         BTST 0 001 XXX XXX
//                    RDH RDL
//          MRS 0 010 XXX XXX
//          MSR 0 011 XXX XXX
//              S RD  RO  IU3 (0,7) + 1 = (1,8)
//    1100  LSL X XXX XXX XXX
//    1101  LSR X XXX XXX XXX
//    1110  ASR X XXX XXX XXX
//                AOP RD  RO
//    1111  MOV X 000 XXX XXX
//          NOT X 001 XXX XXX
//          INC X 010 XXX XXX
//          DEC X 011 XXX XXX
//          ROL X 100 XXX XXX
//          ROR X 101 XXX XXX
//          RLX X 110 XXX XXX
//          RRX X 111 XXX XXX
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 2.0-1: Branch
// 01000 OP COND     RH  RL
//       0  XXXX B   XXX XXX (absolute)
//       0  1111 MOV XXX XXX MOV RH:RL, PC
//                   IMM S6  ((-32,-1)U((0,31) + 1)) * 2 = (-64,-2)U(2,64) 
//       1  XXXX B   XXXXXX  (relative)
//                   IMM U6  (0,63)
//       1  1111 SWI XXXXXX
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 2.2: Memory load/store
// 010010 OP    RD  RBH RBL
//        0 STR XXX XXX XXX
//        1 LDR XXX XXX XXX
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 2.3-5: Multiply
// 010011 S     RD  OP1 OP2
//        X MUL XXX XXX XXX
// 01010        RD  IMM8
//          MUL XXX XXXXXXXX
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 2.6: SP-relative memory addressing 
// 010110 OPC       RLIST                   
//        00   PUSH XXXXXXXX
//        00   PUSH 00000000 PUSH PC
//        01   POP  XXXXXXXX
//        01   POP  00000000 POP PC
//                  IMM8
//        10   PUSH XXXXXXXX
//                  RH  RL
//        1100 MOV  XXX XXX  MOV RH:RL, SP
//        1101 MOV  XXX XXX  MOV SP, RH:RL
//                  IMM S6   (-32,-1)U((0,31) + 1) = (-32,-1)U(1,32)
//        1110 ADD  XXXXXX   ADD SP, imm
//                  IMM S6   ((-32,-1)U((0,31) + 1)) * 2 = (-64,-2)U(2,64)
//        1111 PUSH XXXXXX   PUSH PC + imm
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 2.7: SP-relative load/store
// 010111 OPC   RD IMM U6 (0,63)
//        0 STR XXX XXXXXX STR Rd, [SP + imm6]
//        1 LDR XXX XXXXXX LDR Rd, [SP + imm6]
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 3: FREE / UNDEFINED
// 011X XXXX XXXX XXXX
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Format 4-7: ALU operations (Register-Immediate)
// 1 OPC      RD  IMM8    
//   0000 ADD XXX XXXXXXXX
//   0001 SUB XXX XXXXXXXX
//   0010  OR XXX XXXXXXXX
//   0011 XOR XXX XXXXXXXX
//   0100 AND XXX XXXXXXXX
//   0101 ADC XXX XXXXXXXX
//   0110 SBC XXX XXXXXXXX
//   0111 BIC XXX XXXXXXXX
//   1000 RSB XXX XXXXXXXX
//   1001 RSC XXX XXXXXXXX
//   1010 CMR XXX XXXXXXXX
//   1011 CMP XXX XXXXXXXX
//   1100 CMN XXX XXXXXXXX
//   1101 TST XXX XXXXXXXX
//   1110 TEQ XXX XXXXXXXX
//   1111 MOV XXX XXXXXXXX
// --------------------------------------------------------------------

public class CPU_RAM80 {
    // --------------------------------------------------------------------
    // Utility
    // --------------------------------------------------------------------
    const uint  U8_MASK = Utility. U8_MASK;
    const uint U16_MASK = Utility.U16_MASK;
    const uint  PC_MASK = Utility.U16_MASK & ~1U;

    static uint Field(uint value, uint bit, uint mask)      { return Utility.Field(value, bit, mask); }
    static uint Make16(uint hi, uint lo)                    { return Utility.Make16(hi, lo); }
    static void HiLo16(uint word, out uint hi, out uint lo) {        Utility.HiLo16(word, out hi, out lo); }
    static uint Bit(uint bit)                               { return Utility.Bit(bit); }
    static bool BitTest(uint value, uint bit)               { return Utility.BitTest(value, bit); }    
    static uint SignExtend(uint value, uint bit)            { return Utility.SignExtend(value, bit); }    
    static uint ADSX(uint value, uint bit)                  { return Utility.ADSX(value, bit); }

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Registers
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    public uint[] R = new uint[8]; // 8x 8 bit 
    public uint PC;                // 16 bit halfword aligned
    public uint SP;                // 16 bit byte     aligned
    public uint OPCODE;            // 16 bit opcodes
    public uint F;                 // 16 bit NZCV 0000 000I MMMM

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Micro-operations
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // --------------------------------------------------------------------
    // LUTs
    // --------------------------------------------------------------------
    // OK
    uint[] bcd_u8 = {
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15,
        0x16, 0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30, 0x31,
        0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
        0x48, 0x49, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x60, 0x61, 0x62, 0x63,
        0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
        0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
        0x96, 0x97, 0x98, 0x99, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x10, 0x11,
        0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x40, 0x41, 0x42, 0x43,
        0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
        0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75,
        0x76, 0x77, 0x78, 0x79, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x90, 0x91,
        0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23,
        0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
        0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55,
    };

    // OK
    uint[,] truthtable = {
    //   Valor de CPSR bits 31 a 28 ->                    // | Valor de condicion bits 3 a 0 
    //   0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F   // V
        {0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1}, // 0 EQ    Z=1         igual o cero
        {1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0}, // 1 NE    Z=0         diferente o no cero
        {0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1}, // 2 CS/HS C=1         mayor o igual que (unsigned)
        {1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0}, // 3 CC/LO C=0         menor que (unsigned)
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1}, // 4 MI    N=1         negativo
        {1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0}, // 5 PL    N=0         positivo o cero
        {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1}, // 6 VS    V=1         overflow
        {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0}, // 7 VC    V=0         no overflow
        {0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}, // 8 HI    C=1 AND Z=0 mayor que (unsigned)
        {1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1}, // 9 LS    C=0 OR Z=1  menor o igual que (unsigned)
        {1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1}, // A GE    N=V         mayor o igual que (signed)
        {0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0}, // B LT    N!=V        menor que (signed)
        {1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0}, // C GT    Z=0 AND N=V mayor que (signed)
        {0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1}, // D LE    Z=1 OR N!=V menor o igual que (signed)
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, // E AL    1           siempre
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}  // F --    X           reservado
    };

    // --------------------------------------------------------------------
    // ALU
    // --------------------------------------------------------------------
    // OK
    uint  ADD(uint op1, uint op2, bool s) { uint sum = op1 + op2;             uint ret = sum & U8_MASK; if (s) { SetNZCV_ADD(op1, op2, sum, ret); } return ret; }
    uint  ADC(uint op1, uint op2, bool s) { uint sum = op1 + op2 + Cin();     uint ret = sum & U8_MASK; if (s) { SetNZCV_ADD(op1, op2, sum, ret); } return ret; }
    uint  SUB(uint op1, uint op2, bool s) { uint dif = op1 - op2;             uint ret = dif & U8_MASK; if (s) { SetNZCV_SUB(op1, op2, dif, ret); } return ret; }
    uint  SBC(uint op1, uint op2, bool s) { uint dif = op1 - op2 + Cin() - 1; uint ret = dif & U8_MASK; if (s) { SetNZCV_SUB(op1, op2, dif, ret); } return ret; }

    // OK
    uint  MUL(uint op1, uint op2, bool s) { uint ret = (op1 * op2) & U8_MASK; if (s) { SetNZ(ret); } return ret;}

    // OK
    uint   OR(uint op1, uint op2, bool s) { uint ret = op1 |  op2; if (s) { SetNZ(ret); } return ret; }
    uint  XOR(uint op1, uint op2, bool s) { uint ret = op1 ^  op2; if (s) { SetNZ(ret); } return ret; }
    uint  AND(uint op1, uint op2, bool s) { uint ret = op1 &  op2; if (s) { SetNZ(ret); } return ret; }
    uint  BIC(uint op1, uint op2, bool s) { uint ret = op1 & ~op2; if (s) { SetNZ(ret); } return ret; }

    // OK
    uint  MOV(uint op1, bool s) { uint ret = op1;           if (s) { SetNZ(ret); } return ret; }
    uint  NOT(uint op1, bool s) { uint ret = op1 ^ U8_MASK; if (s) { SetNZ(ret); } return ret; }

    // OK
    uint  INC(uint op1, bool s) { return ADD(op1, 1, s); }
    uint  DEC(uint op1, bool s) { return SUB(op1, 1, s); }
    uint  NEG(uint op1, bool s) { return SUB(0, op1, s); }

    // OK
    uint  ROL(uint op1, bool s) { bool c = (op1 & SIGN_BIT) != 0; uint ret = ((op1 << 1) | (c ?       1U : 0)) & U8_MASK; if (s) { SetNZC(ret, c); } return ret; }
    uint  ROR(uint op1, bool s) { bool c = (op1 &        1) != 0; uint ret =  (op1 >> 1) | (c ? SIGN_BIT : 0);            if (s) { SetNZC(ret, c); } return ret; }

    // OK
    uint  RLX(uint op1, bool s) { uint ret = ((op1 << 1) | Cin ()) & U8_MASK; if (s) { SetNZC(ret, (op1 & SIGN_BIT) != 0); } return ret; }
    uint  RRX(uint op1, bool s) { uint ret =  (op1 >> 1) | Cout();            if (s) { SetNZC(ret, (op1 &        1) != 0); } return ret; }

    // OK
    uint SWAP(uint op1, bool s) { uint ret = ((op1 >> 4) | (op1 << 4)) & U8_MASK; if (s) { SetNZ(ret); } return ret; }
    uint BCDR(uint op1, bool s) { uint ret = bcd_u8[op1];                         if (s) { SetNZ(ret); } return ret; }
    uint RBCD(uint op1, bool s) { uint ret = ((op1 >> 4) * 10) + (op1 & 0xF);     if (s) { SetNZ(ret); } return ret; }

    // OK
    void XCHG(ref uint op1, ref uint op2) { uint tmp = op2; op2 = op1; op1 = tmp; }
    void BTST(    uint op1,     uint op2) { SetC(BitTest(op1, op2)); }        
    void  MRS(out uint  fh, out uint  fl) {        HiLo16(F, out fh, out fl ); }
    void  MSR(    uint  fh,     uint  fl) { WriteF(Make16(       fh,     fl)); }

    // OK
    uint LSL(uint op1, uint op2, bool s) {
        uint ret;
             if (op2 <= 0) { ret =  op1;                        if (s) { SetNZC(ret, (F & FLAG_C) != 0); } }
        else if (op2 <= 8) { ret = (op1 << (int)op2) & U8_MASK; if (s) { SetNZC(ret, BitTest(op1, 8 - op2)); } }
        else               { ret =  0;                          if (s) { SetNZC(ret, false); } }
        return ret;
    }

    // OK
    uint LSR(uint op1, uint op2, bool s) {
        uint ret;
             if (op2 <= 0) { ret = op1;             if (s) { SetNZC(ret, (F & FLAG_C) != 0); } }
        else if (op2 <= 8) { ret = op1 >> (int)op2; if (s) { SetNZC(ret, BitTest(op1, op2 - 1)); } }
        else               { ret = 0;               if (s) { SetNZC(ret, false); } }
        return ret;
    }

    // OK
    uint ASR(uint op1, uint op2, bool s) {
        uint ret;
             if (op2 <= 0) { ret =                        op1;                            if (s) { SetNZC(ret, (F   & FLAG_C  ) != 0); } }
        else if (op2 <= 7) { ret = (uint)((int)SignExtend(op1, 7) >> (int)op2) & U8_MASK; if (s) { SetNZC(ret, BitTest(op1, op2 - 1)); } }
        else               { ret = (uint)((int)SignExtend(op1, 7) >> 7)        & U8_MASK; if (s) { SetNZC(ret, (op1 & SIGN_BIT) != 0); } }
        return ret;
    }

    // --------------------------------------------------------------------
    // ALU flags
    // --------------------------------------------------------------------
    const uint FLAG_N = 0x8000;
    const uint FLAG_Z = 0x4000;
    const uint FLAG_C = 0x2000;
    const uint FLAG_V = 0x1000;

    const uint FLAG_NZ   = FLAG_N   | FLAG_Z;
    const uint FLAG_NZC  = FLAG_NZ  | FLAG_C;
    const uint FLAG_NZCV = FLAG_NZC | FLAG_V;

    const uint SIGN_BIT = 0x80;

    // OK
    void SetNZ(uint ret) {
        F &= ~FLAG_NZ;
        if ((ret & SIGN_BIT) != 0) { F |= FLAG_N; }
        if (ret == 0)              { F |= FLAG_Z; }
    }

    // OK
    void SetNZC(uint ret, bool c) {
        F &= ~FLAG_NZC;
        if ((ret & SIGN_BIT) != 0) { F |= FLAG_N; }
        if (ret == 0)              { F |= FLAG_Z; }
        if (c)                     { F |= FLAG_C; }
    }

    // OK
    void SetNZCV_ADD(uint op1, uint op2, uint sum, uint ret) {
        F &= ~FLAG_NZCV;
        if ((ret & SIGN_BIT) != 0)                          { F |= FLAG_N; }
        if (ret == 0)                                       { F |= FLAG_Z; }
        if (sum > U8_MASK)                                  { F |= FLAG_C; }
        if (((~(op1 ^ op2) & (op1 ^ ret)) & SIGN_BIT) != 0) { F |= FLAG_V; }
    }

    // OK
    void SetNZCV_SUB(uint op1, uint op2, uint dif, uint ret) {
        F &= ~FLAG_NZCV;
        if ((ret & SIGN_BIT) != 0)                         { F |= FLAG_N; }
        if (ret == 0)                                      { F |= FLAG_Z; }
        if (dif <= U8_MASK)                                { F |= FLAG_C; }
        if ((((op1 ^ op2) & (op1 ^ ret)) & SIGN_BIT) != 0) { F |= FLAG_V; }
    }

    // OK
    void SetC(bool c) {
        F &= ~FLAG_C;
        if (c) { F |= FLAG_C; }
    }

    // OK
    uint Cin () { return ((F & FLAG_C) != 0) ?       1U : 0; }
    uint Cout() { return ((F & FLAG_C) != 0) ? SIGN_BIT : 0; }

    // OK
    bool TestCC(uint cc) { return truthtable[cc, Field(F, 12, 0xF)] != 0; }

    // --------------------------------------------------------------------
    // Memory interface
    // --------------------------------------------------------------------
    // 16 bit address bus
    public enum DATA_WIDTH {
        BYTE     = 1,
        HALFWORD = 2
    };

    int FETCH_S;

    public delegate void MEMORY_WRITE(uint address,     uint data, DATA_WIDTH width, out int DATA_S);
    public delegate void MEMORY_READ (uint address, out uint data, DATA_WIDTH width, out int DATA_S);

    public MEMORY_WRITE STR;
    public MEMORY_READ  LDR;

    // --------------------------------------------------------------------
    // Stack operations
    // --------------------------------------------------------------------
    // OK
    void   PUSH(    uint data, out int DATA_S) {
        SP = (SP - 1) & U16_MASK;
        STR(SP,     data, DATA_WIDTH.BYTE, out DATA_S);
    }

    // OK
    void    POP(out uint data, out int DATA_S) {
        LDR(SP, out data, DATA_WIDTH.BYTE, out DATA_S);
        SP = (SP + 1) & U16_MASK;
    }

    // OK
    void PUSH16(    uint data, out int DATA_S) {
        int DATA1_S;
        int DATA2_S;
        PUSH(Field(data, 8, 0xFF), out DATA1_S);
        PUSH(Field(data, 0, 0xFF), out DATA2_S);
        DATA_S = DATA1_S + DATA2_S;
    }

    // OK
    void  POP16(out uint data, out int DATA_S) {
        uint lo;
        uint hi;
        int DATA1_S;
        int DATA2_S;
        POP(out lo, out DATA1_S);
        POP(out hi, out DATA2_S);
        data   = Make16(hi, lo);
        DATA_S = DATA1_S + DATA2_S;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Instruction Set
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // OK
    int Execute() {
        switch (Field(OPCODE, 13, 7)) {
        case 0:
        case 1:  return Format_0_1();
        case 2:  return Format_2();
        case 3:  return Format_3();
        case 4:
        case 5:
        case 6:
        case 7:  return Format_4_5_6_7();
        default: throw new NotImplementedException();
        }
    }

    // --------------------------------------------------------------------
    // Format 0-1: ALU operations (Register-Register)
    // 00 OPC       S RD  RO1 RO2
    //    0000  ADD X XXX XXX XXX
    //    0001  SUB X XXX XXX XXX
    //    0010   OR X XXX XXX XXX
    //    0011  XOR X XXX XXX XXX
    //    0100  AND X XXX XXX XXX
    //    0101  ADC X XXX XXX XXX
    //    0110  SBC X XXX XXX XXX
    //    0111  BIC X XXX XXX XXX
    //    1000  LSL X XXX XXX XXX
    //    1001  LSR X XXX XXX XXX
    //    1010  ASR X XXX XXX XXX
    //                AOP RO1 RO2
    //    1011  CMP 1 000 XXX XXX 
    //          CMN 1 001 XXX XXX 
    //          TST 1 010 XXX XXX 
    //          TEQ 1 011 XXX XXX
    //              S     RD  RO
    //          NEG X 100 XXX XXX
    //         SWAP X 101 XXX XXX
    //         RBCD X 110 XXX XXX
    //         BCDR X 111 XXX XXX
    //                    RO1 RO2
    //         XCHG 0 000 XXX XXX
    //                    RO  IU3
    //         BTST 0 001 XXX XXX
    //                    RDH RDL
    //          MRS 0 010 XXX XXX
    //          MSR 0 011 XXX XXX
    //              S RD  RO  IU3 (0,7) + 1 = (1,8)
    //    1100  LSL X XXX XXX XXX
    //    1101  LSR X XXX XXX XXX
    //    1110  ASR X XXX XXX XXX
    //                AOP RD  RO
    //    1111  MOV X 000 XXX XXX
    //          NOT X 001 XXX XXX
    //          INC X 010 XXX XXX
    //          DEC X 011 XXX XXX
    //          ROL X 100 XXX XXX
    //          ROR X 101 XXX XXX
    //          RLX X 110 XXX XXX
    //          RRX X 111 XXX XXX
    // --------------------------------------------------------------------
    // OK
    int Format_0_1() {
        uint op2 = Field(OPCODE, 0, 7);
        uint op1 = Field(OPCODE, 3, 7);
        uint rd  = Field(OPCODE, 6, 7);
        bool s   = Field(OPCODE, 9, 1) != 0;

        switch (Field(OPCODE, 10, 0xF)) {
        case 0x0: R[rd] = ADD(R[op1], R[op2], s); break;
        case 0x1: R[rd] = SUB(R[op1], R[op2], s); break;
        case 0x2: R[rd] =  OR(R[op1], R[op2], s); break;
        case 0x3: R[rd] = XOR(R[op1], R[op2], s); break;
        case 0x4: R[rd] = AND(R[op1], R[op2], s); break;
        case 0x5: R[rd] = ADC(R[op1], R[op2], s); break;
        case 0x6: R[rd] = SBC(R[op1], R[op2], s); break;
        case 0x7: R[rd] = BIC(R[op1], R[op2], s); break;
        case 0x8: R[rd] = LSL(R[op1], R[op2], s); break;
        case 0x9: R[rd] = LSR(R[op1], R[op2], s); break;
        case 0xA: R[rd] = ASR(R[op1], R[op2], s); break;
        case 0xB:
            switch (rd) {
            case 0:  if (s) {  SUB(R[op1], R[op2], true); } else { XCHG(ref R[op1], ref R[op2]); } break;
            case 1:  if (s) {  ADD(R[op1], R[op2], true); } else { BTST(    R[op1],       op2 ); } break;
            case 2:  if (s) {  AND(R[op1], R[op2], true); } else {  MRS(out R[op1], out R[op2]); } break;
            case 3:  if (s) {  XOR(R[op1], R[op2], true); } else {  MSR(    R[op1],     R[op2]); } break;
            case 4:  R[op1] =  NEG(R[op2], s); break;
            case 5:  R[op1] = SWAP(R[op2], s); break;
            case 6:  R[op1] = RBCD(R[op2], s); break;
            case 7:  R[op1] = BCDR(R[op2], s); break;
            default: throw new NotImplementedException();
            }
            break;
        case 0xC: R[rd] = LSL(R[op1], op2 + 1, s); break;
        case 0xD: R[rd] = LSR(R[op1], op2 + 1, s); break;
        case 0xE: R[rd] = ASR(R[op1], op2 + 1, s); break;
        case 0xF:
            switch (rd) {
            case 0:  R[op1] = MOV(R[op2], s); break;
            case 1:  R[op1] = NOT(R[op2], s); break;
            case 2:  R[op1] = INC(R[op2], s); break;
            case 3:  R[op1] = DEC(R[op2], s); break;
            case 4:  R[op1] = ROL(R[op2], s); break;
            case 5:  R[op1] = ROR(R[op2], s); break;
            case 6:  R[op1] = RLX(R[op2], s); break;
            case 7:  R[op1] = RRX(R[op2], s); break;
            default: throw new NotImplementedException();
            }
            break;
        default:     throw new NotImplementedException();
        }

        return FETCH_S;
    }

    // --------------------------------------------------------------------
    // Format 4-7: ALU operations (Register-Immediate)
    // 1 OPC      RD  IMM8    
    //   0000 ADD XXX XXXXXXXX
    //   0001 SUB XXX XXXXXXXX
    //   0010  OR XXX XXXXXXXX
    //   0011 XOR XXX XXXXXXXX
    //   0100 AND XXX XXXXXXXX
    //   0101 ADC XXX XXXXXXXX
    //   0110 SBC XXX XXXXXXXX
    //   0111 BIC XXX XXXXXXXX
    //   1000 RSB XXX XXXXXXXX
    //   1001 RSC XXX XXXXXXXX
    //   1010 CMR XXX XXXXXXXX
    //   1011 CMP XXX XXXXXXXX
    //   1100 CMN XXX XXXXXXXX
    //   1101 TST XXX XXXXXXXX
    //   1110 TEQ XXX XXXXXXXX
    //   1111 MOV XXX XXXXXXXX
    // --------------------------------------------------------------------
    // OK
    int Format_4_5_6_7() {
        uint imm8 = Field(OPCODE, 0, 0xFF);
        uint rd   = Field(OPCODE, 8, 7);

        switch (Field(OPCODE, 11, 0xF)) {
        case 0x0: R[rd] = ADD(R[rd], imm8, true);  break;
        case 0x1: R[rd] = SUB(R[rd], imm8, true);  break;
        case 0x2: R[rd] =  OR(R[rd], imm8, true);  break;
        case 0x3: R[rd] = XOR(R[rd], imm8, true);  break;
        case 0x4: R[rd] = AND(R[rd], imm8, true);  break;
        case 0x5: R[rd] = ADC(R[rd], imm8, true);  break;
        case 0x6: R[rd] = SBC(R[rd], imm8, true);  break;
        case 0x7: R[rd] = BIC(R[rd], imm8, true);  break;
        case 0x8: R[rd] = SUB(imm8, R[rd], true);  break;
        case 0x9: R[rd] = SBC(imm8, R[rd], true);  break;
        case 0xA:         SUB(imm8, R[rd], true);  break;
        case 0xB:         SUB(R[rd], imm8, true);  break;
        case 0xC:         ADD(R[rd], imm8, true);  break;
        case 0xD:         AND(R[rd], imm8, true);  break;
        case 0xE:         XOR(R[rd], imm8, true);  break;
        case 0xF: R[rd] = MOV(       imm8, false); break;
        default:  throw new NotImplementedException();
        }

        return FETCH_S;
    }

    // --------------------------------------------------------------------
    // Format 2.0-1: Branch
    // 01000 OP COND     RH  RL
    //       0  XXXX B   XXX XXX (absolute)
    //       0  1111 MOV XXX XXX MOV RH:RL, PC
    //                   IMM S6  ((-32,-1)U((0,31) + 1)) * 2 = (-64,-2)U(2,64) 
    //       1  XXXX B   XXXXXXb (relative)
    //                   IMM U6  (0,63)
    //       1  1111 SWI XXXXXX
    // --------------------------------------------------------------------
    // OK
    int Format_2_0() {
        uint rl = Field(OPCODE, 0, 7);
        uint rh = Field(OPCODE, 3, 7);
        uint c  = Field(OPCODE, 6, 0xF);

             if (c == 0xF)  {        HiLo16(PC, out R[rh], out R[rl]); }
        else if (TestCC(c)) { Branch(Make16(        R[rh],     R[rl]) & PC_MASK); }
        return FETCH_S;
    }

    // OK
    int Format_2_1() {
        uint imm6 = Field(OPCODE, 0, 0x3F);
        uint c    = Field(OPCODE, 6, 0xF);

        if (c == 0xF)  { return FETCH_S + SWI(imm6); }
        if (TestCC(c)) { Branch((PC + (ADSX(imm6, 5) << 1) - 2) & PC_MASK); }
        return FETCH_S;
    }

    // --------------------------------------------------------------------
    // Format 2.2: Memory load/store
    // 010010 OP    RD  RBH RBL
    //        0 STR XXX XXX XXX
    //        1 LDR XXX XXX XXX
    // --------------------------------------------------------------------
    // OK
    int Format_2_2() {
        uint address = Make16(R[Field(OPCODE, 3, 7)], R[Field(OPCODE, 0, 7)]);
        uint rd      = Field(OPCODE, 6, 7);
        int  DATA_S;

        switch (Field(OPCODE, 9, 1)) {
        case 0:  STR(address,     R[rd], DATA_WIDTH.BYTE, out DATA_S); break;
        case 1:  LDR(address, out R[rd], DATA_WIDTH.BYTE, out DATA_S); break;
        default: throw new NotImplementedException();
        }

        return FETCH_S + DATA_S;
    }

    // --------------------------------------------------------------------
    // Format 2.3-5: Multiply
    // 010011 S     RD  OP1 OP2
    //        X MUL XXX XXX XXX
    // 01010        RD  IMM8
    //          MUL XXX XXXXXXXX
    // --------------------------------------------------------------------
    // OK
    int Format_2_3() {
        R[Field(OPCODE, 6, 7)] = MUL(R[Field(OPCODE, 3, 7)], R[Field(OPCODE, 0, 7)], Field(OPCODE, 9, 1) != 0);
        return FETCH_S + 4;
    }

    // OK
    int Format_2_4_5() {
        uint rd = Field(OPCODE, 8, 7);
        R[rd] = MUL(R[rd], Field(OPCODE, 0, 0xFF), true);
        return FETCH_S + 4;
    }

    // --------------------------------------------------------------------
    // Format 2.6: SP-relative memory addressing 
    // 010110 OPC       RLIST                   
    //        00   PUSH XXXXXXXX
    //        00   PUSH 00000000 PUSH PC
    //        01   POP  XXXXXXXX
    //        01   POP  00000000 POP PC
    //                  IMM8
    //        10   PUSH XXXXXXXX
    //                  RH  RL
    //        1100 MOV  XXX XXX  MOV RH:RL, SP
    //        1101 MOV  XXX XXX  MOV SP, RH:RL
    //                  IMM S6   (-32,-1)U((0,31) + 1) = (-32,-1)U(1,32)
    //        1110 ADD  XXXXXX   ADD SP, imm
    //                  IMM S6   ((-32,-1)U((0,31) + 1)) * 2 = (-64,-2)U(2,64)
    //        1111 PUSH XXXXXX   PUSH PC + imm
    // --------------------------------------------------------------------
    // OK
    int Format_2_6_0() {
        uint rlist  = Field(OPCODE, 0, 0xFF);
        int  DATA_S;

        if (rlist == 0) {
            PUSH16((PC - 2) & PC_MASK, out DATA_S);
            return FETCH_S + DATA_S;
        }

        int FULL_S = 0;

        for (int i = 7; i >= 0; --i) {
            if (!BitTest(rlist, (uint)i)) { continue; }
            PUSH(R[i], out DATA_S);
            FULL_S += DATA_S;
        }

        return FETCH_S + FULL_S;
    }

    // OK
    int Format_2_6_1() {
        uint rlist  = Field(OPCODE, 0, 0xFF);
        int  DATA_S;

        if (rlist == 0) {
            uint data;
            POP16(out data, out DATA_S);

            if (BitTest(data, 0)) {
                data &= PC_MASK;

                uint sF;
                int  DATA2_S;
                POP16(out sF, out DATA2_S);
                WriteF(sF);

                DATA_S += DATA2_S;                
            }

            Branch(data);
            return FETCH_S + DATA_S;
        }

        int FULL_S = 0;

        for (int i = 0; i <= 7; ++i) {
            if (!BitTest(rlist, (uint)i)) { continue; }
            POP(out R[i], out DATA_S);
            FULL_S += DATA_S;
        }

        return FETCH_S + FULL_S;
    }

    // OK
    int Format_2_6_2() {
        int  DATA_S;
        PUSH(Field(OPCODE, 0, 0xFF), out DATA_S);
        return FETCH_S + DATA_S;
    }

    // OK
    int Format_2_6_3_0() {
        uint rl = Field(OPCODE, 0, 7);
        uint rh = Field(OPCODE, 3, 7);

        switch (Field(OPCODE, 6, 1)) {
        case 0:       HiLo16(SP, out R[rh], out R[rl]); break;
        case 1:  SP = Make16(        R[rh],     R[rl]); break;
        default: throw new NotImplementedException();
        }

        return FETCH_S;
    }

    // OK
    int Format_2_6_3_1() {
        uint imm6 = Field(OPCODE, 0, 0x3F);

        switch (Field(OPCODE, 6, 1)) {
        case 0:                SP = (SP +  ADSX(imm6, 5))           & U16_MASK;             return FETCH_S;
        case 1:  int DATA_S; PUSH16((PC + (ADSX(imm6, 5) << 1) - 2) & PC_MASK, out DATA_S); return FETCH_S + DATA_S;
        default: throw new NotImplementedException();
        }
    }

    // OK
    int Format_2_6_3() {
        switch (Field(OPCODE, 7, 1)) {
        case 0:  return Format_2_6_3_0();
        case 1:  return Format_2_6_3_1();
        default: throw new NotImplementedException();
        }
    }

    // OK
    int Format_2_6() {
        switch (Field(OPCODE, 8, 3)) {
        case 0:  return Format_2_6_0();
        case 1:  return Format_2_6_1();
        case 2:  return Format_2_6_2();
        case 3:  return Format_2_6_3();
        default: throw new NotImplementedException();
        }
    }

    // --------------------------------------------------------------------
    // Format 2.7: SP-relative load/store
    // 010111 OPC   RD IMM U6 (0,63)
    //        0 STR XXX XXXXXX STR Rd, [SP + imm6]
    //        1 LDR XXX XXXXXX LDR Rd, [SP + imm6]
    // --------------------------------------------------------------------
    // OK
    int Format_2_7() {
        uint address = (SP + Field(OPCODE, 0, 0x3F)) & U16_MASK;
        uint rd      = Field(OPCODE, 6, 7);
        int  DATA_S;

        switch (Field(OPCODE, 9, 1)) {
        case 0:  STR(address,     R[rd], DATA_WIDTH.BYTE, out DATA_S); break;
        case 1:  LDR(address, out R[rd], DATA_WIDTH.BYTE, out DATA_S); break;
        default: throw new NotImplementedException();
        }

        return FETCH_S + DATA_S;
    }

    // --------------------------------------------------------------------
    // Format 2
    // Format 2.0-1: Branch
    // Format 2.2: Memory load/store
    // Format 2.3-5: Multiply
    // Format 2.6: SP-relative memory addressing
    // Format 2.7: SP-relative load/store
    // --------------------------------------------------------------------
    // OK
    int Format_2() {
        switch (Field(OPCODE, 10, 7)) {
        case 0:  return Format_2_0();
        case 1:  return Format_2_1();
        case 2:  return Format_2_2();
        case 3:  return Format_2_3();
        case 4:
        case 5:  return Format_2_4_5();
        case 6:  return Format_2_6();
        case 7:  return Format_2_7();
        default: throw new NotImplementedException();
        }
    }

    // --------------------------------------------------------------------
    // Format 3: FREE / UNDEFINED
    // 011X XXXX XXXX XXXX
    // --------------------------------------------------------------------
    // OK
    int Format_3() {
        return UDEF(OPCODE);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Control
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // OK
    public int SingleStep() {
        LDR(PC, out OPCODE, DATA_WIDTH.HALFWORD, out FETCH_S);
        PC = (PC + (uint)DATA_WIDTH.HALFWORD) & PC_MASK;
        return Execute();
    }

    // OK
    void Branch(uint address) { PC = address; }

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Exceptions
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    const uint FLAG_I    = 0x0010;
    const uint MODE_BITS = 0x000F;
    const uint USER_FLAG = FLAG_NZCV | FLAG_I;

    enum OPERATING_MODE {
        USER   = 0,
        UND    = 1,
        SVC    = 2,
        IRQ    = 3,
        SYSTEM = 15
    };

    enum EXCEPTION {
        RESET,
        UNDEFINED_INSTRUCTION,
        SOFTWARE_INTERRUPT,
        NORMAL_INTERRUPT
    }

    const uint VECTOR_RESET = 0x0000;
    const uint VECTOR_UDEF  = 0x0002;
    const uint VECTOR_SWI   = 0x0004;
    const uint VECTOR_IRQ   = 0x0006;

    int Save() {
        int DATA1_S;
        int DATA2_S;
        PUSH16(F,      out DATA1_S);
        PUSH16(PC | 1, out DATA2_S);
        return DATA1_S + DATA2_S;
    }

    int EnterException(EXCEPTION id) {
        uint mode;
        int  DATA_S;
        uint vector;

        switch (id) {
        case EXCEPTION.RESET:                 mode = (uint)OPERATING_MODE.SVC; DATA_S = 0;      vector = VECTOR_RESET; break;
        case EXCEPTION.UNDEFINED_INSTRUCTION: mode = (uint)OPERATING_MODE.UND; DATA_S = Save(); vector = VECTOR_UDEF;  break;
        case EXCEPTION.SOFTWARE_INTERRUPT:    mode = (uint)OPERATING_MODE.SVC; DATA_S = Save(); vector = VECTOR_SWI;   break;
        case EXCEPTION.NORMAL_INTERRUPT:      mode = (uint)OPERATING_MODE.IRQ; DATA_S = Save(); vector = VECTOR_IRQ;   break;
        default: throw new ArgumentException();
        }

        F = (F & ~MODE_BITS) | mode | FLAG_I;
        Branch(vector);
        return DATA_S;
    }

    public void Reset() { EnterException(EXCEPTION.RESET); }

    public delegate int UNDEFINED_INSTRUCTION(CPU_RAM80 cpu, uint OPCODE);
    public delegate int SOFTWARE_INTERRUPT   (CPU_RAM80 cpu, uint id);
    public delegate int NORMAL_INTERRUPT     (CPU_RAM80 cpu);

    public UNDEFINED_INSTRUCTION Hook_UDEF;
    public SOFTWARE_INTERRUPT    Hook_SWI;
    public NORMAL_INTERRUPT      Hook_IRQ;

    int UDEF(uint OPCODE) { return (Hook_UDEF != null) ? Hook_UDEF(this, OPCODE) : EnterException(EXCEPTION.UNDEFINED_INSTRUCTION); }
    int SWI (uint id)     { return (Hook_SWI  != null) ? Hook_SWI (this, id)     : EnterException(EXCEPTION.SOFTWARE_INTERRUPT);  }

    public int IRQ() { return (Hook_IRQ != null) ? Hook_IRQ(this) : (((F & FLAG_I) == 0) ? EnterException(EXCEPTION.NORMAL_INTERRUPT) : 0); }

    bool IsPrivileged() {
        switch (F & MODE_BITS) {
        case (uint)OPERATING_MODE.USER:   return false;
        case (uint)OPERATING_MODE.UND:    return true;
        case (uint)OPERATING_MODE.SVC:    return true;
        case (uint)OPERATING_MODE.IRQ:    return true;
        case (uint)OPERATING_MODE.SYSTEM: return true;
        default:                          return false;
        }
    }

    void WriteF(uint word) { F = IsPrivileged() ? word : ((F & ~USER_FLAG) | (word & USER_FLAG)); }
}
