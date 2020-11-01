-- Abstract :
--
-- like the name says, man
--
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen.Gray_Code;
procedure Test_Gray_Code is

   type Unsigned_5 is mod 2**5;

   package Gray_Code_5 is new SAL.Gen.Gray_Code (5, Unsigned_5);
   use Gray_Code_5;

   package Unsigned_5_IO is new Ada.Text_IO.Modular_IO (Unsigned_5);
   use Unsigned_5_IO;

begin
   Put_Line ("Binary    Gray");
   for I in Unsigned_5 loop
      Put (I, Width => 8, Base => 2);
      Put (To_Gray_Code (I), Width => 10, Base => 2);
      New_Line;
   end loop;
end Test_Gray_Code;
