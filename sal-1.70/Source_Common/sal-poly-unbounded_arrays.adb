-- Abstract:
--
-- see spec
--
-- Copyright (C) 1999, 2002, 2003 Stephen Leake.  All Rights Reserved.
--
-- SAL is free software; you can redistribute it and/or modify it
-- under terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2, or (at your option) any
-- later version. SAL is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details. You should have received a
-- copy of the GNU General Public License distributed with SAL; see
-- file COPYING. If not, write to the Free Software Foundation, 59
-- Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- SAL, or you link SAL object files with other files to produce
-- an executable, that does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.
--
with Ada.Unchecked_Deallocation;
package body SAL.Poly.Unbounded_Arrays is

   procedure Free is new Ada.Unchecked_Deallocation (Base_Array_Type, Base_Array_Access_Type);

   subtype Grow_Direction_Type is Growth_Type range Prepend .. Append;

   procedure Resize
      (Array_Obj : in out Array_Type;
       Direction : in     Grow_Direction_Type;
       Space     : in     Index_Type)
      -- Resize Array_Obj.Base to Space. Does not free items.
   is

      New_Base   : Base_Array_Access_Type;
      Base_Last  : Index_Type;
      Base_First : Index_Type;
   begin

      case Direction is
      when Prepend =>
         case Array_Obj.Growth is
         when Append =>
            raise Constraint_Error;
         when Prepend | Both =>
            Base_First := Array_Obj.Base'Last - Space + 1;
            Base_Last  := Array_Obj.Base'Last;
         end case;

      when Append =>
         case Array_Obj.Growth is
         when Prepend =>
            raise Constraint_Error;
         when Append | Both =>
            Base_First := Array_Obj.Base'First;
            Base_Last := Array_Obj.Base'First + Space - 1;
         end case;

      end case;

      New_Base := new Base_Array_Type (Base_First .. Base_Last);
      New_Base (Array_Obj.First .. Array_Obj.Last) := Array_Obj.Base (Array_Obj.First .. Array_Obj.Last);
      Free (Array_Obj.Base);
      Array_Obj.Base := New_Base;
   end Resize;

   procedure Grow
      (Array_Obj    : in out Array_Type;
       Direction    : in     Grow_Direction_Type;
       Needed_Space : in     Index_Type)
   is
      New_Space : Index_Type := Index_Type'Max (Array_Obj.Base.all'Length * 2, 1);
   begin
      loop
         exit when New_Space >= Needed_Space;
         New_Space := New_Space * 2;
      end loop;
      Resize (Array_Obj, Direction, New_Space);
      if Array_Obj.Base'Length > Array_Obj.Max_Space then
         Array_Obj.Max_Space := Array_Obj.Base'Length;
      end if;
   end Grow;

   procedure Shrink (Array_Obj : in out Array_Type; Direction : in Grow_Direction_Type)
   is begin
      Resize (Array_Obj, Direction, Index_Type'Max (Array_Obj.Base.all'Length / 2, Array_Obj.Initial_Space));
   end Shrink;

   ----------
   -- Public subprograms

   procedure Create
      (Array_Obj : in out Array_Type;
       Space     : in     Index_Type     := 0;
       Growth    : in     Growth_Type := Append;
       First     : in     Index_Type     := 1;
       Last      : in     Index_Type     := 0)
   is begin
      Finalize (Array_Obj);
      Array_Obj.Growth := Growth;
      Array_Obj.Initial_Space := Space;
      Array_Obj.Max_Space := Array_Obj.Initial_Space;
      case Growth is
      when Prepend =>
         Array_Obj.Last := Last;
         Array_Obj.First := Last + 1;
         Array_Obj.Base := new Base_Array_Type (Last - Space + 1 .. Last);
      when Append =>
         Array_Obj.First := First;
         Array_Obj.Last := First - 1;
         Array_Obj.Base := new Base_Array_Type (First .. First + Space - 1);
      when Both =>
         Array_Obj.First := First;
         Array_Obj.Last := First - 1;
         if Space = 0 then
            Array_Obj.Base := new Base_Array_Type (First .. First - 1);
         else
            if Space mod 2 = 1 then
               Array_Obj.Base := new Base_Array_Type (First - Space / 2 .. First + Space / 2);
            else
               Array_Obj.Base := new Base_Array_Type (First - Space / 2 + 1 .. First + Space / 2);
            end if;
         end if;
      end case;
   end Create;

   function First (Array_Obj : in Array_Type'class) return Index_Type
   is begin
      return Array_Obj.First;
   end First;

   function Last (Array_Obj : in Array_Type'class) return Index_Type
   is begin
      return Array_Obj.Last;
   end Last;

   function Length (Array_Obj : in Array_Type'class) return Index_Type
   is begin
      return Array_Obj.Last - Array_Obj.First + 1;
   end Length;

   function Space (Array_Obj : in Array_Type'class) return Index_Type
   is begin
      return Array_Obj.Base.all'Length;
   end Space;

   function Max_Space (Array_Obj : in Array_Type'class) return Index_Type
   is begin
      return Array_Obj.Max_Space;
   end Max_Space;

   ---------
   -- Override Limited_Controlled operations

   procedure Initialize (Array_Obj : in out Array_Type)
   is begin
      Create (Array_Obj);
   end Initialize;

   procedure Finalize (Array_Obj : in out Array_Type)
   is begin
      for I in Array_Obj.First .. Array_Obj.Last loop
         Free_Item (Array_Obj.Base (I));
      end loop;

      Array_Obj.First := 1;
      Array_Obj.Last := 0;

      -- Kludge for ObjectAda 7.1.2 with Test_Storage_Pools; raises Constraint_Error if Free (null).
      if Array_Obj.Base /= null then
         Free (Array_Obj.Base);
      end if;
   end Finalize;

   procedure Adjust (Array_Obj : in out Array_Type)
   is
      Temp_Base : constant Base_Array_Access_Type := new Base_Array_Type (Array_Obj.Base'Range);
   begin
      for I in Array_Obj.First .. Array_Obj.Last loop
         Temp_Base (I) := Copy_Item_Node (Array_Obj.Base (I));
      end loop;
      Array_Obj.Base := Temp_Base;
   end Adjust;

   ---------
   -- Dispatching operations on Array_Type

   function Get
      (Array_Obj : in Array_Type;
       Index     : in Index_Type)
       return Item_Node_Type
   is begin
      if Index < Array_Obj.First or Index > Array_Obj.Last then
         raise Constraint_Error;
      else
         return Array_Obj.Base (Index);
      end if;
   end Get;

   procedure Set
      (Array_Obj : in out Array_Type;
       Index     : in     Index_Type;
       Item      : in     Item_Type)
   is begin
      if Index < Array_Obj.First or Index > Array_Obj.Last then
         raise Constraint_Error;
      else
         Array_Obj.Base (Index) := To_Item_Node (Item);
      end if;
   end Set;

   procedure Set_Grow
      (Array_Obj : in out Array_Type;
       Index     : in     Index_Type;
       Item      : in     Item_Type)
   is begin
      if Index < Array_Obj.Base'First then
         Grow (Array_Obj, Prepend, Array_Obj.Base'Last - Index + 1);
      elsif Index > Array_Obj.Base'Last then
         Grow (Array_Obj, Append, Index - Array_Obj.Base'First + 1);
      end if;

      if Index < Array_Obj.First then
         Array_Obj.First := Index;
      elsif Index > Array_Obj.Last then
         Array_Obj.Last := Index;
      end if;

      Array_Obj.Base (Index) := To_Item_Node (Item);
   end Set_Grow;

   procedure Add_First
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type)
   is
      New_First : constant Index_Type := Array_Obj.First - 1;
   begin
      if New_First < Array_Obj.Base'First then
         Grow (Array_Obj, Prepend, Array_Obj.Base'Last - New_First + 1);
         -- Grow also checks Growth_Direction
      end if;
      Array_Obj.First := New_First;
      Array_Obj.Base (Array_Obj.First) := To_Item_Node (Item);
   end Add_First;

   procedure Add_Last
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type)
   is
      New_Last : constant Index_Type := Array_Obj.Last + 1;
   begin
      if New_Last > Array_Obj.Base'Last then
         Grow (Array_Obj, Append, New_Last - Array_Obj.Base'First + 1);
         -- Grow also checks Growth_Direction
      end if;
      Array_Obj.Last := New_Last;
      Array_Obj.Base (Array_Obj.Last) := To_Item_Node (Item);
   end Add_Last;

   procedure Delete_First (Array_Obj : in out Array_Type)
   is begin
      if Array_Obj.First > Array_Obj.Last then
         raise Constraint_Error;
      end if;
      Free_Item (Array_Obj.Base (Array_Obj.First));
      Array_Obj.First := Array_Obj.First + 1;
      -- Shrink if occupied space plus empty space on First end is less than 1/4 full.
      if Array_Obj.Last - Array_Obj.First < (Array_Obj.Last - Array_Obj.Base'First) / 4 then
         Shrink (Array_Obj, Prepend);
      end if;
   end Delete_First;

   procedure Delete_Last (Array_Obj : in out Array_Type)
   is begin
      if Array_Obj.First > Array_Obj.Last then
         raise Constraint_Error;
      end if;
      Free_Item (Array_Obj.Base (Array_Obj.Last));
      Array_Obj.Last := Array_Obj.Last - 1;
      -- Shrink if occupied space plus empty space on Last end is less than 1/4 full.
      if Array_Obj.Last - Array_Obj.First < (Array_Obj.Base'Last - Array_Obj.First) / 4 then
         Shrink (Array_Obj, Append);
      end if;
   end Delete_Last;

end SAL.Poly.Unbounded_Arrays;
