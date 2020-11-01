-- Abstract:
--
-- see spec
--
function SAL.Generic_Hex_Image (Item : in Number_Type) return String
is
   Temp : Number_Type := Item;
   Nibble : Number_Type;
   Image : String (1 .. Width);
begin
   for I in reverse Image'Range loop
      Nibble := Temp mod 16;
      Temp := Temp / 16;
      if Nibble > 9 then
         Image (I) := Character'Val (Character'Pos ('A') + Nibble - 10);
      else
         Image (I) := Character'Val (Character'Pos ('0') + Nibble);
      end if;
   end loop;
   return Image;
end Sal.Generic_Hex_Image;
