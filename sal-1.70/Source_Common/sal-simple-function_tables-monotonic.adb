-- Abstract:
--
-- see spec
--
package body SAL.Simple.Function_Tables.Monotonic is

   procedure Initialize (Function_Table : in out Function_Table_Type)
   is
      Table : Table_Type renames Function_Table.Table.all;
   begin
      Function_Tables.Initialize (Function_Tables.Function_Table_Type (Function_Table));

      Function_Table.Increasing_Range :=
         Table (Table'First).Range_Value < Table (Table'First + 1).Range_Value;

      -- search for max, min
      Function_Table.Range_Max := Range_Type'First;
      Function_Table.Range_Min := Range_Type'Last;

      for I in Table'Range loop
         if I > Table'First + 1 then
            if Function_Table.Increasing_Range then
               if Table (I).Range_Value <= Table (I - 1).Range_Value then
                  raise Initialization_Error;
               end if;
            else
               if Table (I).Range_Value >= Table (I - 1).Range_Value  then
                  raise Initialization_Error;
               end if;
            end if;
         end if;

         if Table (I).Range_Value > Function_Table.Range_Max then
            Function_Table.Range_Max := Table (I).Range_Value;
         end if;
         if Table (I).Range_Value < Function_Table.Range_Min then
            Function_Table.Range_Min := Table (I).Range_Value;
         end if;
      end loop;
   end Initialize;

end SAL.Simple.Function_Tables.Monotonic;
