-- Abstract :
--
-- Define constants to reflect hardware bit and word endianness in record
-- representation clauses. Obviously, this file is highly system-dependent.
with System;
package SAL.Endianness is
   pragma Pure;

   -- this is for ObjectAda on an Intel 386 compatible processor
   System_Name : constant System.NAME := System.I80386;

   Endianness_Error : exception;

   Bit_Order      : constant := 1; -- 1 or -1
   High_Bit_First : constant := 0; -- 0 or 1
   Low_Bit_First  : constant := 1; -- opposite of High_Bit_First
   LSBit          : constant := 0; -- 0 or 7 or 15 (for word machines)

   -- typical usage of these constants, to make record spec independent of
   -- bit-endianness:
   --
   --    for Foo_Type use record
   --       User_Ephemeris_Needed       at 0 range LSBit + Bit_Order * 0 .. LSBit + Bit_Order * 0;
   --
   --       Operational_Status          at 0 range
   --          Low_Bit_First  * (LSBit + Bit_Order * 4) + High_Bit_First * (LSBit + Bit_Order * 7) ..
   --          High_Bit_First * (LSBit + Bit_Order * 4) + Low_Bit_First  * (LSBit + Bit_Order * 7);
   --    end record;

   type Byte_Order_Type is (BIG_ENDIAN, LITTLE_ENDIAN);
   Byte_Order : constant Byte_Order_Type := LITTLE_ENDIAN;

end SAL.Endianness;
