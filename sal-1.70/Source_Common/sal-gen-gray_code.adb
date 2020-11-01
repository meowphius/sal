-- Abstract:
--
-- see spec.
--
package body SAL.Gen.Gray_Code is

   type Map_Type is array (Binary_Type) of Binary_Type;

   Map,
   Inverse_Map : Map_Type; -- initialized by elaboration code below.

   function To_Gray_Code (Binary : in Binary_Type) return Binary_Type
   is begin
      return Map (Binary);
   end To_Gray_Code;

   function To_Binary (Gray_Code : in Binary_Type) return Binary_Type
   is begin
      return Inverse_Map (Gray_Code);
   end To_Binary;

begin
   declare
      Binary_Bits : Bit_Array_Type;
      Gray_Bits : Bit_Array_Type;
      Binary : Binary_Type;
   begin
      for Gray in Binary_Type loop
         Gray_Bits := To_Bit_Array (Gray);
         Binary_Bits := Gray_Bits;
         for I in Binary_Bits'Range loop
            for J in I + 1 .. Binary_Bits'Last loop
               Binary_Bits (I) := Binary_Bits (I) xor Gray_Bits (J);
            end loop;
         end loop;
         Binary := To_Binary (Binary_Bits);
         Map (Binary) := Gray;
         Inverse_Map (Gray) := Binary;
      end loop;
   end;
end SAL.Gen.Gray_Code;
