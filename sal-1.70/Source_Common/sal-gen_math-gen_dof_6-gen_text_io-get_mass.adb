--  Abstract :
--
--  Don't get redundant Inertia; compute it instead. Otherwise copied
--  from Auto_Text_IO generated body.
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
separate (SAL.Gen_Math.Gen_DOF_6.Gen_Text_IO)
procedure Get_Mass
  (File                        : in     Ada.Text_IO.File_Type;
   Item                        :    out Mass_Type;
   Named_Association_Record    : in     Boolean := False;
   Named_Association_Component : in     Boolean := False)
is
   Temp_Item : Mass_Type;
begin
   Check (File, "(");
   if Named_Association_Record then Check (File, "Total => "); end if;
   Math_Text_IO.
     Get (File, Temp_Item.Total);
   Check (File, ",");
   Skip_Whitespace (File);
   if Named_Association_Record then Check (File, "Center => "); end if;
   Math_DOF_3_Text_IO.
     Get_Item (File, Temp_Item.Center,
               Named_Association => Named_Association_Component);
   Check (File, ",");
   Skip_Whitespace (File);
   if Named_Association_Record then Check (File, "Center_Inertia => "); end if;
   Math_DOF_3_Text_IO.
     Get_Item (File, Temp_Item.Center_Inertia,
               Named_Association => Named_Association_Component);
   Check (File, ")");

   Item := To_Mass (Temp_Item.Total, Temp_Item.Center, Temp_Item.Center_Inertia);
end Get_Mass;

