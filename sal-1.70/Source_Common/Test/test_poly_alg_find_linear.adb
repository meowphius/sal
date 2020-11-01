-- Abstract:
--
-- Test the Find_Linear algorithm instantiated with Poly containers.
--
-- Copyright (C) 1999, 2000, 2003 Stephen Leake.  All Rights Reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This program is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this program; see file COPYING. If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Poly.Alg.Find_Linear;
with Test_Poly_Lists_Single_Aux;
with Test_Storage_Pools;
procedure Test_Poly_Alg_Find_Linear is
begin
   Put_Line ("Testing Sal.Poly.Algs.Find_Linear");

   Put_Line ("Definite non-tagged non-limited items (integer_lists)");
   Integer_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Integers;
      use Lists;
      use Iterators;
      function Find_Integer is new Algorithms.Find_Linear (Item_Type => Integer);
      List : aliased List_Type;
   begin
      Insert_Tail (List, 1);
      Insert_Tail (List, 3);
      Insert_Tail (List, 5);
      Print_List (List);

      Put_Line ("Finding");
      Put_Line ("5 => " & Integer'Image (Current (Find_Integer (First (List), 5))));
      Put_Line ("3 => " & Integer'Image (Current (Find_Integer (First (List), 3))));
      if Is_Null (Find_Integer (First (List), 0)) then
         Put_Line ("ok, didn't find 0");
      else
         Put_Line ("Oops, found 0");
      end if;

   end Integer_Lists;
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);

   ------------

   New_Line;
   Put_Line ("Indefinite tagged non-limited items (symbols)");
   Symbol_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Symbols;
      use Test_Poly_Lists_Single_Aux.Symbols.Lists;
      use Test_Poly_Lists_Single_Aux.Symbols.Iterators;

      function Is_Equal (Left : in Symbol_Type'class; Right : in Symbol_Access_Type) return Boolean;
      -- True if symbols are equal.

      function Type_Is_Equal (Left : in Symbol_Type'class; Right : in Symbol_Access_Type) return Boolean;
      -- True if type of symbols is equal

      function Is_Equal (Left : in Symbol_Type'class; Right : in Symbol_Access_Type) return Boolean
      is begin
         return Left = Right.all;
      end Is_Equal;

      function Type_Is_Equal (Left : in Symbol_Type'class; Right : in Symbol_Access_Type) return Boolean
      is
         use type Ada.Tags.Tag;
      begin
         return Left'Tag = Right'Tag;
      end Type_Is_Equal;

      function Find_Symbol is new Test_Poly_Lists_Single_Aux.Symbols.Algorithms.Find_Linear
        (Item_Type => Symbol_Type'Class,
         "="       => Is_Equal);

      function Find_Type is new Test_Poly_Lists_Single_Aux.Symbols.Algorithms.Find_Linear
        (Item_Type => Symbol_Type'Class,
         "="       => Type_Is_Equal);

      List       : aliased List_Type;
      Float_5    : constant Floating_Point_Type  := (Significant_Digits => 5);
      Discrete_4 : constant Discrete_Number_Type := (First          => -4, Last => +4);
      Float_0    : constant Floating_Point_Type  := (Significant_Digits => 0);
   begin
      Insert_Tail (List, Float_5);
      Insert_Tail (List, Discrete_4);
      Print_List (List);

      Put_Line ("Finding exact symbols");
      Put ("Float_5    => "); Print_Symbol (Current (Find_Symbol (First (List), Float_5))); New_Line;
      Put ("Discrete_4 => "); Print_Symbol (Current (Find_Symbol (First (List), Discrete_4))); New_Line;
      if Is_Null (Find_Symbol (First (List), Float_0)) then
         Put_Line ("ok, didn't find Float_0");
      else
         Put_Line ("Oops, found Float_0");
      end if;

      Put_Line ("Finding types");
      Put ("Float    => "); Print_Symbol (Current (Find_Type (First (List), Float_0))); New_Line;
      Put ("Discrete => "); Print_Symbol (Current (Find_Type (First (List), Discrete_4))); New_Line;
   end Symbol_Lists;
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);

   -------------

   New_Line;
   Put_Line ("Indefinite tagged limited items (puppets)");
   Puppet_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Puppets;
      use Test_Poly_Lists_Single_Aux.Puppets.Lists;
      use Test_Poly_Lists_Single_Aux.Puppets.Iterators;

      List : aliased List_Type;

      -- comparision operations on Puppet_Type are dispatching, so
      -- they must be declared in Test_Poly_Lists_Single_Aux.Puppets,
      -- not here.

      function Is_Equal (Left : in Parameters_Type; Right : in Puppet_Access_Type) return Boolean
      is begin
         return Is_Equal (Left, Right.all);
      end Is_Equal;

      function Find_Puppet is new Test_Poly_Lists_Single_Aux.Puppets.Algorithms.Find_Linear
        (Item_Type => Parameters_Type,
         "="       => Is_Equal);

      function Type_Is_Equal (Left : in Parameters_Type; Right : in Puppet_Access_Type) return Boolean
      is begin
         case Left.Label is
         when Muppet =>
            return Right.all in Muppet_Type'Class;
         when Beanie =>
            return Right.all in Beanie_Type'Class;
         end case;
      end Type_Is_Equal;

      function Find_Type is new Test_Poly_Lists_Single_Aux.Puppets.Algorithms.Find_Linear
        (Item_Type => Parameters_Type,
         "="       => Type_Is_Equal);

      Muppet_2_5 : constant Parameters_Type := (Muppet, 2, 5);
      Beanie_4 : constant Parameters_Type := (Beanie, 4);
      Beanie_0 : constant Parameters_Type := (Beanie, 0);
   begin
      Insert_Tail (List, Muppet_2_5);
      Insert_Tail (List, Beanie_4);
      Print_List (List);

      Put_Line ("Finding exact Puppets");
      Put ("Muppet_2_5 => "); Print_Puppet (Current (Find_Puppet (First (List), Muppet_2_5))); New_Line;
      Put ("Beanie_4   => "); Print_Puppet (Current (Find_Puppet (First (List), Beanie_4))); New_Line;
      if Is_Null (Find_Puppet (First (List), Beanie_0)) then
         Put_Line ("ok, didn't find Beanie_0");
      else
         Put_Line ("Oops, found Beanie_0");
      end if;

      Put_Line ("Finding types");
      Put ("Muppet => "); Print_Puppet (Current (Find_Type (First (List), Muppet_2_5))); New_Line;
      Put ("Beanie => "); Print_Puppet (Current (Find_Type (First (List), Beanie_0))); New_Line;

   end Puppet_Lists;
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Puppets.Storage_Pool);

end Test_Poly_Alg_Find_Linear;
