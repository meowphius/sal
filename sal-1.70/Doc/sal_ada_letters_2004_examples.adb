with Ada.Text_IO;
with Test_Poly_Lists_Double_Aux;
procedure Sal_Ada_Letters_2004_Examples
is
begin
   declare
      use Test_Poly_Lists_Double_Aux.Integers;
      use Lists;

      List     : List_Type;


      procedure Do_Something (Item : in Integer)
      is begin
         Ada.Text_IO.Put_Line ("Item => " & Integer'Image (Item));
      end Do_Something;

      procedure Process_List (List : in List_Type)
      is
         Iterator : Iterator_Type := First (List);
      begin
         loop
            exit when Is_Null (Iterator);
            Do_Something (Current (Iterator));
            Next (Iterator);
         end loop;
      end Process_List;

   begin
      Add (List, 1);
      Add (List, 2);
      Add (List, 3);

      Process_List (List);
   end;
end Sal_Ada_Letters_2004_Examples;
