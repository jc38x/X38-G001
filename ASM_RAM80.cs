//*****************************************************************************
// ASM_RAM80
// jc38x (jcds38x@gmail.com)
//*****************************************************************************

using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text.RegularExpressions;

public static class ASM_RAM80 {
    // ------------------------------------------------------------------------
    // Utility
    // ------------------------------------------------------------------------
    static uint Bit(uint bit) { return Utility.Bit(bit); }
    static bool BitTest(uint value, uint bit) { return Utility.BitTest(value, bit); }

    // ------------------------------------------------------------------------
    // Formats
    // ------------------------------------------------------------------------
    static uint Format_0_1(uint OPC, uint S, uint RD, uint RO1, uint RO2) { return 0x0000 | (OPC  << 10) | (S     << 9) | (RD   << 6) | (RO1 << 3) | RO2; } // OK
    static uint Format_4_5_6_7(uint OPC, uint RD, uint IMM8)              { return 0x8000 | (OPC  << 11) | (RD    << 8) |  IMM8; }                          // OK
    static uint Format_2_0(uint COND, uint RH, uint RL)                   { return 0x4000 | (COND <<  6) | (RH    << 3) |  RL; }                            // OK
    static uint Format_2_1(uint COND, uint IMM6)                          { return 0x4400 | (COND <<  6) |  IMM6; }                                         // OK
    static uint Format_2_2(uint OPC, uint RD, uint RBH, uint RBL)         { return 0x4800 | (OPC  <<  9) | (RD    << 6) | (RBH  << 3) |  RBL; }             // OK
    static uint Format_2_3(uint S, uint RD, uint OP1, uint OP2)           { return 0x4C00 | (S    <<  9) | (RD    << 6) | (OP1  << 3) |  OP2; }             // OK
    static uint Format_2_4_5(uint RD, uint IMM8)                          { return 0x5000 | (RD   <<  8) |  IMM8; }                                         // OK
    static uint Format_2_6_0_1_2(uint OPC, uint RLIST)                    { return 0x5800 | (OPC  <<  8) |  RLIST; }                                        // OK
    static uint Format_2_6_3_0_1(uint OPC, uint RH, uint RL)              { return 0x5B00 | (OPC  <<  6) | (RH    << 3) |  RL; }                            // OK
    static uint Format_2_6_3_2_3(uint OPC, uint IMM6)                     { return 0x5B80 | (OPC  <<  6) | IMM6; }                                          // OK
    static uint Format_2_7(uint OPC, uint RD, uint IMM6)                  { return 0x5C00 | (OPC  <<  9) | (RD    << 6) |  IMM6; }                          // OK

    // ------------------------------------------------------------------------
    // Mnemonics
    // ------------------------------------------------------------------------
    const string lead    = @"^\s*";
    const string mnend   = @"(\s+)";
    const string S       = @"([S])?";
    const string COND    = @"(EQ|NE|CS|HS|CC|LO|MI|PL|VS|VC|HI|LS|GE|LT|GT|LE|AL)?";

    static readonly Regex[] mnemonics = {
        new Regex(lead + "(ADC)"  + S    + mnend),
        new Regex(lead + "(ADD)"  + S    + mnend),
        new Regex(lead + "(AND)"  + S    + mnend),
        new Regex(lead + "(ASR)"  + S    + mnend),
        new Regex(lead + "(B)"    + COND + mnend),
        new Regex(lead + "(BCDR)" + S    + mnend),
        new Regex(lead + "(BIC)"  + S    + mnend),
        new Regex(lead + "(BTST)"        + mnend),
        new Regex(lead + "(CMN)"         + mnend),
        new Regex(lead + "(CMP)"         + mnend),
        new Regex(lead + "(DEC)"  + S    + mnend),
        new Regex(lead + "(INC)"  + S    + mnend),
        new Regex(lead + "(LDR)"         + mnend),
        new Regex(lead + "(LSL)"  + S    + mnend),
        new Regex(lead + "(LSR)"  + S    + mnend),
        new Regex(lead + "(MOV)"  + S    + mnend),
        new Regex(lead + "(MRS)"         + mnend),
        new Regex(lead + "(MSR)"         + mnend),
        new Regex(lead + "(MUL)"  + S    + mnend),
        new Regex(lead + "(NEG)"  + S    + mnend),
        new Regex(lead + "(NOT)"  + S    + mnend),
        new Regex(lead + "(OR)"   + S    + mnend),
        new Regex(lead + "(POP)"         + mnend),
        new Regex(lead + "(PUSH)"        + mnend),
        new Regex(lead + "(RBCD)" + S    + mnend),
        new Regex(lead + "(RLX)"  + S    + mnend),
        new Regex(lead + "(ROL)"  + S    + mnend),
        new Regex(lead + "(ROR)"  + S    + mnend),
        new Regex(lead + "(RRX)"  + S    + mnend),
        new Regex(lead + "(SBC)"  + S    + mnend),
        new Regex(lead + "(STR)"         + mnend),
        new Regex(lead + "(SUB)"  + S    + mnend),
        new Regex(lead + "(SWAP)" + S    + mnend),
        new Regex(lead + "(SWI)"         + mnend),
        new Regex(lead + "(TEQ)"         + mnend),
        new Regex(lead + "(TST)"         + mnend),
        new Regex(lead + "(XCHG)"        + mnend),
        new Regex(lead + "(XOR)"  + S    + mnend)
    };

    static readonly Type         type  = typeof(ASM_RAM80);
    const           BindingFlags flags = BindingFlags.Static | BindingFlags.NonPublic;

    static string GetOperand(string cmd, Match mr) {
        Group g = mr.Groups[mr.Groups.Count - 1];
        return cmd.Substring(g.Index + g.Length);
    }

    public static uint Assemble(string instruction, out string comment) {
        int    com = instruction.IndexOf(';');
        string cmd;

        if (com < 0) { comment = "";                         cmd = instruction; }
        else         { comment = instruction.Substring(com); cmd = instruction.Substring(0, com); }

        cmd = cmd.ToUpperInvariant() + " ";

        foreach (Regex mne in mnemonics) {
            Match mr = mne.Match(cmd);
            if (mr.Success) { return (uint)type.GetMethod("ASM_" + mr.Groups[1].Value, flags).Invoke(null, new object[] { GetOperand(cmd, mr), mr }); }
        }

        throw new Exception("unrecognized instruction (" + instruction + ")");
    }

    // ------------------------------------------------------------------------
    // Immediate
    // ------------------------------------------------------------------------
    const string imm_dec = @"\#?([\-\+])?([0-9]+)";
    const string imm_oct = @"\#?([\-\+])?([0-7]+)O";
    const string imm_hex = @"(?:\#([\-\+])?([0-9A-F]+)H)|(?:([\-\+])?([0-9][0-9A-F]*)H)|(?:\#?([\-\+])?(?:0X|\$)([0-9A-F]+)H?)";
    const string imm     = @"(?:(?:" + imm_dec + ")|(?:" + imm_hex + ")|(?:" + imm_oct + "))";

    static readonly int[] imm_rad = { 10, 16, 16, 16, 8 };

    static uint GetImm(Match mo, int offset) {        
        int p = 0;

        foreach (int radix in imm_rad) {
            string digits = mo.Groups[p + 2 + offset].Value;
            if (digits.Length > 0) { return (uint)(Convert.ToUInt16(digits, radix) * sign2int[mo.Groups[p + 1 + offset].Value]); }
            p += 2;
        }

        throw new Exception("malformed immediate operand (" + mo.Groups[0].Value + ")");
    }

    static uint CheckImm8(uint imm) {
        if (imm <= 255) { return imm; }
        throw new Exception("number (" + (int)imm + ") must be in range (0 to 255)");
    }

    static uint CheckImm3_1to8(uint imm) {
        if ((imm >= 1) && (imm <= 8)) { return imm - 1; }
        throw new Exception("number (" + (int)imm + ") must be in range (1 to 8)");
    }

    static uint CheckImm3_0to7(uint imm) {
        if (imm <= 7) { return imm; }
        throw new Exception("number (" + (int)imm + ") must be in range (0 to 7)");
    }

    static uint CheckImm6_m64to64nze(uint imm) {
        int simm = (int)imm;
        if ((simm != 0) && ((simm & 1) == 0) && (simm >= -64) && (simm <= 64)) { return (uint)((simm > 0) ? ((simm >> 1) - 1) : ((simm >> 1) & 0x3F)); }
        throw new Exception("number (" + simm + ") must be in range (-64 to 64), be even and not zero");
    }

    static uint CheckImm6_0to63(uint imm) {
        if (imm <= 63) { return imm; }
        throw new Exception("number (" + (int)imm + ") must be in range (0 to 63)");
    }

    static uint CheckImm6_m32to32nz(uint imm) {
        int simm = (int)imm;
        if ((simm != 0) && (simm >= -32) && (simm <= 32)) { return (uint)((simm > 0) ? (simm - 1) : (simm & 0x3F)); }
        throw new Exception("number (" + simm + ") must be in range (-32 to 32) and not zero");
    }

    // ------------------------------------------------------------------------
    // Operands
    // ------------------------------------------------------------------------
    const string oplead  = @"^";
    const string trail   = @"\s*$";
    const string R       = @"R([0-7])";
    const string rlist   = @"\{\s*((?:R[0-7])|(?:(?:R[0-7]\s*\,\s*)+R[0-7]))\s*\}";

    static Regex RinList = new Regex(R);

    const string comma   = @"\s*\,\s*";
    const string colon   = @"\s*\:\s*";
    const string plus    = @"\s*\+\s*";
    const string MPB     = @"\[\s*";
    const string MPE     = @"\s*\]";

    const string RhRl    = R   +        colon       + R;
    const string pointer = MPB +        RhRl        + MPE;
    const string pSP     = MPB + "SP" + plus  + imm + MPE;

    static Regex o_rrr   = new Regex(oplead + R     + comma + R       + comma + R   + trail);
    static Regex o_rimm  = new Regex(oplead + R     + comma + imm                   + trail);
    static Regex o_rr    = new Regex(oplead + R     + comma + R                     + trail);
    static Regex o_immr  = new Regex(oplead + imm   + comma + R                     + trail);
    static Regex o_rhrl  = new Regex(oplead + RhRl                                  + trail);
    static Regex o_rrimm = new Regex(oplead + R     + comma + R       + comma + imm + trail);
    static Regex o_imm   = new Regex(oplead + imm                                   + trail);
    static Regex o_rmem  = new Regex(oplead + R     + comma + pointer               + trail);
    static Regex o_rlist = new Regex(oplead + rlist                                 + trail);

    static Regex o_SPimm = new Regex(oplead + "SP"  + comma + imm                   + trail);
    static Regex o_SPmem = new Regex(oplead + R     + comma + pSP                   + trail);
    static Regex o_PC2HL = new Regex(oplead + RhRl  + comma + "PC"                  + trail);
    static Regex o_SP2HL = new Regex(oplead + RhRl  + comma + "SP"                  + trail);
    static Regex o_HL2SP = new Regex(oplead + "SP"  + comma + RhRl                  + trail);
    static Regex o_PC    = new Regex(oplead + "PC"                                  + trail);
    static Regex o_PCimm = new Regex(oplead + "PC"  + comma + imm                   + trail);    

    static Dictionary<string, uint> cond2uint = new Dictionary<string, uint>();
    static Dictionary<string, uint> r2uint    = new Dictionary<string, uint>();
    static Dictionary<string, uint> s2uint    = new Dictionary<string, uint>();
    static Dictionary<string,  int> sign2int  = new Dictionary<string,  int>();

    public static void Start() {
        cond2uint.Add("EQ", 0x0);
        cond2uint.Add("NE", 0x1);
        cond2uint.Add("CS", 0x2); cond2uint.Add("HS", 0x2);
        cond2uint.Add("CC", 0x3); cond2uint.Add("LO", 0x3);
        cond2uint.Add("MI", 0x4);
        cond2uint.Add("PL", 0x5);
        cond2uint.Add("VS", 0x6);
        cond2uint.Add("VC", 0x7);
        cond2uint.Add("HI", 0x8);
        cond2uint.Add("LS", 0x9);
        cond2uint.Add("GE", 0xA);
        cond2uint.Add("LT", 0xB);
        cond2uint.Add("GT", 0xC);
        cond2uint.Add("LE", 0xD);
        cond2uint.Add("AL", 0xE); cond2uint.Add(""  , 0xE);

        r2uint.Add("0", 0);
        r2uint.Add("1", 1);
        r2uint.Add("2", 2);
        r2uint.Add("3", 3);
        r2uint.Add("4", 4);
        r2uint.Add("5", 5);
        r2uint.Add("6", 6);
        r2uint.Add("7", 7);

        s2uint.Add("" , 0);
        s2uint.Add("S", 1);

        sign2int.Add("+",  1); sign2int.Add("" ,  1);
        sign2int.Add("-", -1);
    }

    static uint GetRList(string list) {
        uint rlist = 0;

        foreach (Match mm in RinList.Matches(list)) {
            uint bit = r2uint[mm.Groups[1].Value];
            if (BitTest(rlist, bit)) { throw new Exception("duplicate R" + bit + " in list"); }
            rlist |= Bit(bit);
        }

        return rlist;
    }

    // ------------------------------------------------------------------------
    // Translation
    // ------------------------------------------------------------------------
    static uint BadOperands(string operands,                 Match mr) { throw new Exception("unexpected operands (" + operands + ") for instruction " + mr.Groups[1].Value); }
    static uint BadVariant (string flag,     string comment, Match mr) { throw new Exception(flag + " is not supported for instruction " + mr.Groups[1].Value + " " + comment); }

    static uint Format_0_1_EOP_F(string operands, Match mr, uint EOP) {
        Match mo = o_rr.Match(operands); if (mo.Success) { return Format_0_1(0xF, s2uint[mr.Groups[2].Value], EOP, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        return BadOperands(operands, mr);
    }

    /*
     * NOT{S} Rd, Ro 
     * INC{S} Rd, Ro 
     * DEC{S} Rd, Ro
     * ROL{S} Rd, Ro
     * ROR{S} Rd, Ro
     * RLX{S} Rd, Ro
     * RRX{S} Rd, Ro
     */
    static uint ASM_NOT(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x1); }
    static uint ASM_INC(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x2); }
    static uint ASM_DEC(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x3); }
    static uint ASM_ROL(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x4); }
    static uint ASM_ROR(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x5); }
    static uint ASM_RLX(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x6); }
    static uint ASM_RRX(string operands, Match mr) { return Format_0_1_EOP_F(operands, mr, 0x7); }

    static uint Format_0_1_4_5_6_7_3R(string operands, Match mr, uint OPC) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rrr.Match (operands); if (mo.Success) {               return Format_0_1(OPC, S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_rimm.Match(operands); if (mo.Success) { if (S == 1) { return Format_4_5_6_7(OPC, r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); } return BadVariant("omitting S", "with immediate operand", mr); }
        return BadOperands(operands, mr);
    }

    /*
     *  OR{S} Rd, Ro1, Ro2
     *  ORS   Rd, Imm8
     * XOR{S} Rd, Ro1, Ro2
     * XORS   Rd, Imm8
     * AND{S} Rd, Ro1, Ro2
     * ANDS   Rd, Imm8
     * ADC{S} Rd, Ro1, Ro2
     * ADCS   Rd, Imm8
     * BIC{S} Rd, Ro1, Ro2
     * BICS   Rd, Imm8
     */
    static uint ASM_OR (string operands, Match mr) { return Format_0_1_4_5_6_7_3R(operands, mr, 0x2); }
    static uint ASM_XOR(string operands, Match mr) { return Format_0_1_4_5_6_7_3R(operands, mr, 0x3); }
    static uint ASM_AND(string operands, Match mr) { return Format_0_1_4_5_6_7_3R(operands, mr, 0x4); }
    static uint ASM_ADC(string operands, Match mr) { return Format_0_1_4_5_6_7_3R(operands, mr, 0x5); }
    static uint ASM_BIC(string operands, Match mr) { return Format_0_1_4_5_6_7_3R(operands, mr, 0x7); }

    /*
     * MUL{S} Rd, Ro1, Ro2
     * MULS   Rd, Imm8 
     */
    static uint ASM_MUL(string operands, Match mr) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rrr.Match (operands); if (mo.Success) {               return Format_2_3(S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_rimm.Match(operands); if (mo.Success) { if (S == 1) { return Format_2_4_5(r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); } return BadVariant("omitting S", "with immediate operand", mr); }
        return BadOperands(operands, mr);
    }
    
    static uint Format_0_1_SHF(string operands, Match mr, uint OPC) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rrr.Match  (operands); if (mo.Success) { return Format_0_1(OPC,     S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_rrimm.Match(operands); if (mo.Success) { return Format_0_1(OPC + 4, S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], CheckImm3_1to8(GetImm(mo, 2))); }
        return BadOperands(operands, mr);
    }

    /*
     * LSL{S} Rd, Ro1, Ro2
     * LSL{S} Rd, Ro, UImm3 (1,8)
     * LSR{S} Rd, Ro1, Ro2
     * LSR{S} Rd, Ro, UImm3 (1,8)
     * ASR{S} Rd, Ro1, Ro2
     * ASR{S} Rd, Ro, UImm3 (1,8)
     */
    static uint ASM_LSL(string operands, Match mr) { return Format_0_1_SHF(operands, mr, 0x8); }
    static uint ASM_LSR(string operands, Match mr) { return Format_0_1_SHF(operands, mr, 0x9); }
    static uint ASM_ASR(string operands, Match mr) { return Format_0_1_SHF(operands, mr, 0xA); }

    static uint Format_0_1_4_5_6_7_CMN(string operands, Match mr, uint EOP, uint OPC) {
        Match mo;
        mo = o_rr.Match  (operands); if (mo.Success) { return Format_0_1(0xB, 1, EOP, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        mo = o_rimm.Match(operands); if (mo.Success) { return Format_4_5_6_7(OPC, r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); }
        return BadOperands(operands, mr);
    }

    /*
     * CMN Ro1, Ro2
     * CMN Rd, Imm8
     * TST Ro1, Ro2
     * TST Rd, Imm8
     * TEQ Ro1, Ro2
     * TEQ Rd, Imm8
     */
    static uint ASM_CMN(string operands, Match mr) { return Format_0_1_4_5_6_7_CMN(operands, mr, 0x1, 0xC); }
    static uint ASM_TST(string operands, Match mr) { return Format_0_1_4_5_6_7_CMN(operands, mr, 0x2, 0xD); }
    static uint ASM_TEQ(string operands, Match mr) { return Format_0_1_4_5_6_7_CMN(operands, mr, 0x3, 0xE); }

    /*
     * CMP Ro1, Ro2
     * CMP Imm8, Rd
     * CMP Rd, Imm8
     */
    static uint ASM_CMP(string operands, Match mr) {
        Match mo;
        mo = o_rr.Match  (operands); if (mo.Success) { return Format_0_1(0xB, 1, 0x0, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        mo = o_rimm.Match(operands); if (mo.Success) { return Format_4_5_6_7(0xB, r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); }
        mo = o_immr.Match(operands); if (mo.Success) { return Format_4_5_6_7(0xA, r2uint[mo.Groups[mo.Groups.Count - 1].Value], CheckImm8(GetImm(mo, 0))); }
        return BadOperands(operands, mr);
    }

    static uint Format_0_1_4_5_6_7_SUB(string operands, Match mr, uint OPC1, uint OPC2) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rrr.Match (operands); if (mo.Success) {               return Format_0_1(OPC1, S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_rimm.Match(operands); if (mo.Success) { if (S == 1) { return Format_4_5_6_7(OPC1, r2uint[mo.Groups[1].Value],                   CheckImm8(GetImm(mo, 1))); } return BadVariant("omitting S", "with immediate operand", mr); }
        mo = o_immr.Match(operands); if (mo.Success) { if (S == 1) { return Format_4_5_6_7(OPC2, r2uint[mo.Groups[mo.Groups.Count - 1].Value], CheckImm8(GetImm(mo, 0))); } return BadVariant("omitting S", "with immediate operand", mr); }
        return BadOperands(operands, mr);
    }

    /*
     * SBC{S} Rd, Ro1, Ro2
     * SBCS   Rd, Imm8
     * SBCS   Imm8, Rd
     * SUB{S} Rd, Ro1, Ro2
     * SUBS   Rd, Imm8
     * SUBS   Imm8, Rd
     */
    static uint ASM_SBC(string operands, Match mr) { return Format_0_1_4_5_6_7_SUB(operands, mr, 0x6, 0x9); }
    static uint ASM_SUB(string operands, Match mr) { return Format_0_1_4_5_6_7_SUB(operands, mr, 0x1, 0x8); }

    /*
     * ADD{S} Rd, Ro1, Ro2
     * ADDS   Rd, Imm8
     * ADD    SP, Imm6 (-32,-1)U(1,32)
     */
    static uint ASM_ADD(string operands, Match mr) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rrr.Match  (operands); if (mo.Success) {               return Format_0_1(0x0, S, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_rimm.Match (operands); if (mo.Success) { if (S == 1) { return Format_4_5_6_7(0x0, r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); } return BadVariant("omitting S", "with immediate operand", mr); }
        mo = o_SPimm.Match(operands); if (mo.Success) { if (S == 0) { return Format_2_6_3_2_3(0x2, CheckImm6_m32to32nz(GetImm(mo, 0))); }                 return BadVariant(         "S", "with SP operand",        mr); }
        return BadOperands(operands, mr);
    }

    static uint Format_0_1_EOP_B(string operands, Match mr, uint EOP) {
        Match mo = o_rr.Match(operands); if (mo.Success) { return Format_0_1(0xB, s2uint[mr.Groups[2].Value], EOP, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        return BadOperands(operands, mr);
    }

    /*
     *  NEG{S} Rd, Ro
     * SWAP{S} Rd, Ro
     * RBCD{S} Rd, Ro
     * BCDR{S} Rd, Ro
     */
    static uint ASM_NEG (string operands, Match mr) { return Format_0_1_EOP_B(operands, mr, 0x4); }
    static uint ASM_SWAP(string operands, Match mr) { return Format_0_1_EOP_B(operands, mr, 0x5); }
    static uint ASM_RBCD(string operands, Match mr) { return Format_0_1_EOP_B(operands, mr, 0x6); }
    static uint ASM_BCDR(string operands, Match mr) { return Format_0_1_EOP_B(operands, mr, 0x7); }

    /*
     * XCHG Ro, Ro 
     */
    static uint ASM_XCHG(string operands, Match mr) {
        Match mo = o_rr.Match(operands); if (mo.Success) { return Format_0_1(0xB, 0, 0x0, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        return BadOperands(operands, mr);
    }

    /*
     * BTST Ro, UImm3 (0,7) 
     */
    static uint ASM_BTST(string operands, Match mr) {
        Match mo = o_rimm.Match(operands); if (mo.Success) { return Format_0_1(0xB, 0, 0x1, r2uint[mo.Groups[1].Value], CheckImm3_0to7(GetImm(mo, 1))); }
        return BadOperands(operands, mr);
    }

    static uint Format_0_1_MRS(string operands, Match mr, uint EOP) {
        Match mo = o_rhrl.Match(operands); if (mo.Success) { return Format_0_1(0xB, 0, EOP, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        return BadOperands(operands, mr);
    }

    /*
     *  MRS Rh:Rl
     *  MSR Rh:Rl
     */
    static uint ASM_MRS(string operands, Match mr) { return Format_0_1_MRS(operands, mr, 0x2); }
    static uint ASM_MSR(string operands, Match mr) { return Format_0_1_MRS(operands, mr, 0x3); }

    /*
     * MOV{S} Rd, Ro
     * MOV Rd, Imm8
     * MOV Rh:Rl, PC
     * MOV Rh:Rl, SP
     * MOV SP, Rh:Rl 
     */
    static uint ASM_MOV(string operands, Match mr) {
        uint  S = s2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rr.Match   (operands); if (mo.Success) {               return Format_0_1(0xF, S, 0x0, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        mo = o_rimm.Match (operands); if (mo.Success) { if (S == 0) { return Format_4_5_6_7(0xF, r2uint[mo.Groups[1].Value], CheckImm8(GetImm(mo, 1))); }     return BadVariant("S", "with immediate operand", mr); }
        mo = o_PC2HL.Match(operands); if (mo.Success) { if (S == 0) { return Format_2_0(0xF, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }       return BadVariant("S", "with PC operand",        mr); }
        mo = o_SP2HL.Match(operands); if (mo.Success) { if (S == 0) { return Format_2_6_3_0_1(0x0, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); } return BadVariant("S", "with SP operand",        mr); }
        mo = o_HL2SP.Match(operands); if (mo.Success) { if (S == 0) { return Format_2_6_3_0_1(0x1, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); } return BadVariant("S", "with SP operand",        mr); }
        return BadOperands(operands, mr);
    }

    /*
     * B{cc} Rh:Rl
     * B{cc} Imm6 (-64,-2)U(2,64) 
     */
    static uint ASM_B(string operands, Match mr) {
        uint  COND = cond2uint[mr.Groups[2].Value];
        Match mo;
        mo = o_rhrl.Match(operands); if (mo.Success) { return Format_2_0(COND, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value]); }
        mo = o_imm.Match (operands); if (mo.Success) { return Format_2_1(COND, CheckImm6_m64to64nze(GetImm(mo, 0))); }
        return BadOperands(operands, mr);
    }

    /*
     * SWI Imm6 (0,63) 
     */
    static uint ASM_SWI(string operands, Match mr) {
        Match mo = o_imm.Match(operands); if (mo.Success) { return Format_2_1(0xF, CheckImm6_0to63(GetImm(mo, 0))); }
        return BadOperands(operands, mr);
    }

    static uint Format_2_2_7(string operands, Match mr, uint OPC) {
        Match mo;
        mo = o_rmem.Match (operands); if (mo.Success) { return Format_2_2(OPC, r2uint[mo.Groups[1].Value], r2uint[mo.Groups[2].Value], r2uint[mo.Groups[3].Value]); }
        mo = o_SPmem.Match(operands); if (mo.Success) { return Format_2_7(OPC, r2uint[mo.Groups[1].Value], CheckImm6_0to63(GetImm(mo, 1))); }
        return BadOperands(operands, mr);
    }

    /*
     * LDR Rd, [Rh:Rl]
     * LDR Rd, [SP + Imm6] (0,63)
     * STR Rd, [Rh:Rl]
     * STR Rd, [SP + Imm6] (0,63)
     */
    static uint ASM_LDR(string operands, Match mr) { return Format_2_2_7(operands, mr, 1); }
    static uint ASM_STR(string operands, Match mr) { return Format_2_2_7(operands, mr, 0); }

    /*
     * POP {Rlist}
     * POP PC
     */
    static uint ASM_POP(string operands, Match mr) {
        Match mo;        
        mo = o_rlist.Match(operands); if (mo.Success) { return Format_2_6_0_1_2(0x1, GetRList(mo.Groups[1].Value)); }
        mo = o_PC.Match   (operands); if (mo.Success) { return Format_2_6_0_1_2(0x1, 0); }
        return BadOperands(operands, mr);
    }

    /*
     * PUSH {Rlist}
     * PUSH PC
     * PUSH Imm8
     * PUSH PC, Imm6 (-64,-2)U(2,64)     
     */
    static uint ASM_PUSH(string operands, Match mr) {
        Match mo;
        mo = o_rlist.Match(operands); if (mo.Success) { return Format_2_6_0_1_2(0x0, GetRList(mo.Groups[1].Value)); }
        mo = o_PC.Match   (operands); if (mo.Success) { return Format_2_6_0_1_2(0x0, 0); }
        mo = o_imm.Match  (operands); if (mo.Success) { return Format_2_6_0_1_2(0x2, CheckImm8(GetImm(mo, 0))); }
        mo = o_PCimm.Match(operands); if (mo.Success) { return Format_2_6_3_2_3(0x3, CheckImm6_m64to64nze(GetImm(mo, 0))); }
        return BadOperands(operands, mr);
    }
}
