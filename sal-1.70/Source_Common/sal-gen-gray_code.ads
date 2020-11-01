-- Abstract:
--
-- Generic n bit gray codes. Since most gray codes are used with hardware,
-- where individual bit access may be needed, we provide that too.
--
-- A "gray code" satisfies the condition that the binary representation of
-- any two successive values differ by only one bit.
--
-- This package implements this by defining:
--
-- Bi = Gi xor Gi+1 ... Gn-1
--
-- Where Bi is bit i from the n bit binary representation, and Gi is bit i
-- from the n bit gray code representation.
--
-- A lookup table is computed at elaboration time.
--
with Ada.Unchecked_Conversion;
generic
   Bits : in Natural;
   type Binary_Type is mod <>; -- <> must be 2**Bits
package SAL.Gen.Gray_Code is
   pragma Elaborate_Body; -- Elaboration code computes table

   function To_Gray_Code (Binary : in Binary_Type) return Binary_Type;
   pragma Inline (To_Gray_Code);

   function To_Binary (Gray_Code : in Binary_Type) return Binary_Type;
   function From_Gray_Code (Gray_Code : in Binary_Type) return Binary_Type renames To_Binary;
   pragma Inline (To_Binary);

   -----------
   -- Access to individual bits

   type Bit_Array_Type is array (Integer range 0 .. Bits - 1) of Boolean;
   pragma Pack (Bit_Array_Type);
   -- for Bit_Array_Type'Size use Binary_Type'size; -- not static by LRM:4.9(26)

   function To_Bit_Array is new Ada.Unchecked_Conversion
      (Source => Binary_Type,
       Target => Bit_Array_Type);

   function To_Binary is new Ada.Unchecked_Conversion
      (Source => Bit_Array_Type,
       Target => Binary_Type);

end SAL.Gen.Gray_Code;
