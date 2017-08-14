//*****************************************************************************
// ASM_RAM80
// jc38x (jcds38x@gmail.com)
//*****************************************************************************

using System;

public class MCU_RAM80 {
    static uint Field(uint value, uint bit, uint mask)      { return Utility.Field(value, bit, mask); }
    static uint Make16(uint hi, uint lo)                    { return Utility.Make16(hi, lo); }
    static void HiLo16(uint word, out uint hi, out uint lo) {        Utility.HiLo16(word, out hi, out lo); }

    public delegate void RD(uint offset, out uint data, out int DATA_S);
    public delegate void WR(uint offset,     uint data, out int DATA_S);

    public RD[] page_RD;
    public WR[] page_WR;

    static void Default_RD(uint offset, out uint data, out int DATA_S) { data = 0xFF; DATA_S = 1; }
    static void Default_WR(uint offset,     uint data, out int DATA_S) {              DATA_S = 1; }

    public MCU_RAM80() {
        page_RD = new RD[256];
        page_WR = new WR[256];
        for (int i = 0; i < 256; ++i) { page_RD[i] = Default_RD; page_WR[i] = Default_WR; }
    }

    static void HiLoAddress(uint address, CPU_RAM80.DATA_WIDTH width, out uint page, out uint offset) { HiLo16(address & ~((uint)width - 1), out page, out offset); }

    public void LDR(uint address, out uint data, CPU_RAM80.DATA_WIDTH width, out int DATA_S) {
        uint page;
        uint offset;

        HiLoAddress(address, width, out page, out offset);

        switch (width) {
        case CPU_RAM80.DATA_WIDTH.HALFWORD:
            uint lo,   hi;
            int  LO_S, HI_S;
            page_RD[page](offset,     out lo,   out LO_S);
            page_RD[page](offset + 1, out hi,   out HI_S);
            data   = Make16(hi, lo);
            DATA_S = LO_S + HI_S;
            break;
        case CPU_RAM80.DATA_WIDTH.BYTE:
            page_RD[page](offset,     out data, out DATA_S);
            break;
        default:
            throw new ArgumentException();
        }
    }

    public void STR(uint address,     uint data, CPU_RAM80.DATA_WIDTH width, out int DATA_S) {
        uint page;
        uint offset;

        HiLoAddress(address, width, out page, out offset);

        switch (width) {
        case CPU_RAM80.DATA_WIDTH.HALFWORD:
            uint lo,   hi;
            int  LO_S, HI_S;
            HiLo16(data, out hi, out lo);
            page_WR[page](offset,     hi,   out LO_S);
            page_WR[page](offset + 1, lo,   out HI_S);
            DATA_S = LO_S + HI_S;
            break;
        case CPU_RAM80.DATA_WIDTH.BYTE:
            page_WR[page](offset,     data, out DATA_S);
            break;
        default:
            throw new ArgumentException();
        }
    }
}
