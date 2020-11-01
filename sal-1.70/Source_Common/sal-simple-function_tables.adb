-- Abstract:
--
-- see spec
--
package body SAL.Simple.Function_Tables is

   procedure Initialize (Function_Table : in out Function_Table_Type)
   is
      Table : Table_Type renames Function_Table.Table.all;
   begin
      Function_Table.Domain_Max := Domain_Type'First;
      Function_Table.Domain_Min := Domain_Type'Last;

      -- search for max, min
      for I in Table'Range loop
         if I > Table'First and then
            Table (I).Domain_Value <= Table (I - 1).Domain_Value
         then
            raise Initialization_Error;
         end if;

         if Table (I).Domain_Value > Function_Table.Domain_Max then
            Function_Table.Domain_Max := Table (I).Domain_Value;
         end if;
         if Table (I).Domain_Value < Function_Table.Domain_Min then
            Function_Table.Domain_Min := Table (I).Domain_Value;
         end if;
      end loop;
   end Initialize;

end SAL.Simple.Function_Tables;
