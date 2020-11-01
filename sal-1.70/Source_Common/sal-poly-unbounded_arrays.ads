--  Abstract:
--
--  Arrays that grow and shrink.
--
--  Array size doubles or halves for growing and shrinking, to
--  minimize allocation/deallocation time.
--
--  We allow growing the array on either end, to allow fast
--  insert/delete operations at either end.
--
--  First is the least index that has been set, Last is the greatest
--  index that has been set.
--
--  Copyright (C) 1998 - 2000, 2002, 2003, 2004 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.
--
with System.Storage_Pools;
with Ada.Finalization;
generic
   type Index_Type is range <>;
   --  This is used for Space and Length, as well as item indexing. So
   --  it must include zero. There's very little reason for this to be
   --  a constrained type.
   type Item_Type (<>) is limited private;
   type Item_Node_Type is private;
   with function To_Item_Node (Item : in Item_Type) return Item_Node_Type;
   with procedure Free_Item (Item : in out Item_Node_Type);
   --  If Item_Type is definite non-limited, Item_Node_Type should
   --  just be Item_Type. Then To_Item_Node should just return Item,
   --  and Free_Item should be null (and both should be inlined). See
   --  SAL.Aux.Definite_Private_Items.
   --
   --  If Item_Type is indefinite, Item_Node_Type should be 'access
   --  Item_Type'. Then To_Item_Node should allocate Item and return a
   --  pointer to it, and Free_Item should be Unchecked_Deallocation.
   --  See SAL.Aux.Indefinite_Private_Items.
   --
   --  To create an array of limited objects (say of type
   --  Limited_Type), Item_Type can be a non-limited type holding the
   --  parameters needed to create an object (non-limited to allow the
   --  user to create aggregates of creation parameters), and
   --  Item_Node_Type can be access Limited_Type. Then To_Item_Node
   --  must allocate an object of type Limited_Type and initialize it
   --  using the parameters in Item_Type. See
   --  SAL.Aux.Indefinite_Limited_Items.
   --
   --  Other usages may be possible.

   with function Copy_Item_Node (Source : in Item_Node_Type) return Item_Node_Type is <>;
   --  Deep copy of Source. Used when array is copied, but not when
   --  array is grown; that uses simple assignment.

   Array_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global array type should be
   --  System.Storage_Pools.Root_Storage_Pool'Class
   --  (<some_global_access_type>'Storage_Pool). Default for a local
   --  array type, which should be reclaimed when it goes out of
   --  scope, is implementation defined (sigh).
package SAL.Poly.Unbounded_Arrays is
   pragma Elaborate_Body; --  Ada.Finalization is.

   type Growth_Type is (Prepend, Append, Both);

   type Array_Type is new Ada.Finalization.Controlled with private;

   ----------
   --  Class-wide operations on Array_Type

   procedure Create
     (Array_Obj : in out Array_Type;
      Space     : in     Index_Type  := 0;
      Growth    : in     Growth_Type := Append;
      First     : in     Index_Type  := 1;
      Last      : in     Index_Type  := 0);
   --  Finalize Array_Obj, then re-create an empty array with space for
   --  Space items. Allocated space will never get smaller than
   --  Space.
   --
   --  If Growth is Prepend, array only grows on First end. Last
   --  item is indexed by Last, First is set to Last + 1.
   --
   --  If Growth is Append, array only grows on Last end. First
   --  item is indexed by First, Last is set to First - 1.
   --
   --  If Growth is Both, space is always maintained on both
   --  ends for insertions. First item is indexed by First, Last
   --  is set to First - 1.

   function First (Array_Obj : in Array_Type'class) return Index_Type;
   --  Return index of first item in Array_Obj.

   function Last (Array_Obj : in Array_Type'class) return Index_Type;
   --  Return index of last item in Array_Obj.

   function Length (Array_Obj : in Array_Type'class) return Index_Type;
   --  Return Last - First.

   function Space (Array_Obj : in Array_Type'class) return Index_Type;
   --  Return number of items allocated for Array_Obj; always at least
   --  Length (Array_Obj).

   function Max_Space (Array_Obj : in Array_Type'class) return Index_Type;
   --  Return max number of items allocated for Array_Obj since
   --  Array_Obj was created.

   pragma Inline (First, Last, Length, Space, Max_Space);

   ----------
   --  Override Limited_Controlled operations

   procedure Initialize (Array_Obj : in out Array_Type);
   --  Calls Create with default parameters.

   procedure Finalize (Array_Obj : in out Array_Type);
   --  Free all added items, free array storage.

   procedure Adjust (Array_Obj : in out Array_Type);
   --  Deep copy.

   ----------
   --  Dispatching operations on Array_Type

   function Get
      (Array_Obj : in Array_Type;
       Index     : in Index_Type)
      return Item_Node_Type;
   pragma Inline (Get);
   --  Return Array_Obj (Index).
   --
   --  Raises Constraint_Error if Index < First or > Last.

   procedure Set
      (Array_Obj : in out Array_Type;
       Index     : in     Index_Type;
       Item      : in     Item_Type);
   pragma Inline (Set);
   --  Set Array_Obj (Index) to To_Item_Node (Item), freeing current
   --  item at Index, if any.
   --
   --  Raises Constraint_Error if Index < First or > Last.

   procedure Set_Grow
      (Array_Obj : in out Array_Type;
       Index     : in     Index_Type;
       Item      : in     Item_Type);
   --  Set Array_Obj (Index) to To_Item_Node (Item). If Index < First
   --  or > Last, Array_Obj is grown to include Index (if allowed by
   --  Growth), and First or Last is updated.
   --
   --  Raises Constraint_Error if Index > Last and Growth = Prepend.
   --  Raises Constraint_Error if Index < First and Growth = Append.

   procedure Add_First
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type);
   procedure Prepend
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type)
      renames Add_First;
   --  Set Array_Obj (First - 1) to To_Item_Node (Item), growing
   --  Array_Obj if necessary, update First.
   --
   --  Raises Constraint_Error if Growth = Append.

   procedure Add_Last
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type);
   procedure Append
      (Array_Obj : in out Array_Type;
       Item      : in     Item_Type)
      renames Add_Last;
   --  Set Array_Obj (Last + 1) to To_Item_Node (Item), growing
   --  Array_Obj if necessary, update Last.
   --
   --  Raises Constraint_Error if Growth = Prepend.

   procedure Delete_First (Array_Obj : in out Array_Type);
   --  Free Array_Obj (First), set First to First + 1, shrink array if
   --  necessary.
   --
   --  Raises Constraint_Error if Growth = Append.
   --  Raises Constraint_Error if First > Last.

   procedure Delete_Last (Array_Obj : in out Array_Type);
   --  Free Array_Obj (Last), set Last to Last - 1, shrink array if
   --  necessary.
   --
   --  Raises Constraint_Error if Growth = Prepend.
   --  Raises Constraint_Error if First > Last.

private
   type Base_Array_Type is array (Index_Type range <>) of Item_Node_Type;

   type Base_Array_Access_Type is access Base_Array_Type;
   for Base_Array_Access_Type'Storage_Pool use Array_Storage_Pool;

   type Array_Type is new Ada.Finalization.Controlled with record
      --  Default values permit Finalize before Create
      Growth        : Growth_Type := Append;
      Initial_Space : Index_Type  := 0;
      Max_Space     : Index_Type  := 0;
      First         : Index_Type  := 1;
      Last          : Index_Type  := 0;
      Base          : Base_Array_Access_Type;
   end record;

end SAL.Poly.Unbounded_Arrays;
