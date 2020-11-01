-- Abstract:
--
-- see spec
--
function SAL.Generic_Binary_Image (Item : in Number_Type) return String
is
   Temp : Number_Type := Item;
   Nibble : Number_Type;
   Image : String (1 .. Nibbles * 4 + Nibbles - 1);
begin
   for I in reverse Image'Range loop
      if I mod 5 = 0 then
         Image (I) := '_';
      else
         Nibble := Temp mod 2;
         Temp := Temp / 2;
         Image (I) := Character'Val (Character'Pos ('0') + Nibble);
      end if;
   end loop;
   return Image;
end Sal.Generic_Binary_Image;
