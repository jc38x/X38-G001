using UnityEngine;
using System.Collections;

public class Utility {


    public static uint Bit(uint bit) {
        return (1U << (int)bit);
    }

    public static bool BitTest(uint value, uint bit) {
        return (value & Bit(bit)) != 0;
    }



    public const uint U8_MASK = 0xFF;
    public const uint U16_MASK = 0xFFFF;
    public const uint PC_MASK = 0xFFFE;

    // OK
    public static uint Field(uint value, uint bit, uint mask) { return (value >> (int)bit) & mask; }
    public static uint Make16(uint hi, uint lo) { return (hi << 8) | lo; }
    //public static uint Bit(int bit) { return 1U << bit; }
    //public static bool BitTest(uint value, int bit) { return (value & Bit(bit)) != 0; }
    public static uint ADSX(uint value, uint bit) { return (BitTest(value, (uint)bit) ? SignExtend(value, bit) : (value + 1)); }

    // OK
    public static uint SignExtend(uint value, uint bit) {
        uint m = Bit((uint)bit);
        return (value ^ m) - m;
    }

    // OK
    public static void HiLo16(uint word, out uint hi, out uint lo) {
        hi = Field(word, 8, 0xFF);
        lo = Field(word, 0, 0xFF);
    }









}
