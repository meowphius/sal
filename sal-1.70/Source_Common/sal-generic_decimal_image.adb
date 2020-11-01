-- Abstract:
--
-- see spec
--
function SAL.Generic_Decimal_Image
   (Item  : in Number_Type;
    Width : in Natural)
return String
is
   Temp : Number_Type := abs Item;
   Digit : Number_Type;
   Image : String (1 .. Width);
begin
   for I in reverse Image'Range loop
      Digit := Temp mod 10;
      Temp := Temp / 10;
      Image (I) := Character'Val (Character'Pos ('0') + Digit);
   end loop;
   return Image;
end Sal.Generic_Decimal_Image;
