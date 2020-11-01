--  Abstract :
--
--  Text_IO for types in Ada.Streams.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;
with SAL.Gen_Array_Text_IO;
package Ada_Streams_Text_IO is
--   pragma Elaborate_Body; -- Ada.Text_IO is, but we don't need a body

   package Stream_Element_IO is new Ada.Text_IO.Modular_IO (Stream_Element);
   procedure Put
     (File  : in Ada.Text_IO.File_Type;
      Item  : in Stream_Element;
      Width : in Ada.Text_IO.Field;
      Base  : in Ada.Text_IO.Number_Base)
     renames Stream_Element_IO.Put;
   procedure Put
     (Item  : in Stream_Element;
      Width : in Ada.Text_IO.Field;
      Base  : in Ada.Text_IO.Number_Base)
     renames Stream_Element_IO.Put;
   procedure Get
     (File  : in     Ada.Text_IO.File_Type;
      Item  :    out Stream_Element;
      Width : in     Ada.Text_IO.Field := 0)
     renames Stream_Element_IO.Get;
   procedure Get
     (Item  :    out Stream_Element;
      Width : in     Ada.Text_IO.Field := 0)
     renames Stream_Element_IO.Get;

   package Stream_Element_Offset_IO is new Ada.Text_IO.Integer_IO (Stream_Element_Offset);
   procedure Put
     (File  : in Ada.Text_IO.File_Type;
      Item  : in Stream_Element_Offset;
      Width : in Ada.Text_IO.Field       := 0;
      Base  : in Ada.Text_IO.Number_Base := 10)
     renames Stream_Element_Offset_IO.Put;
   procedure Put
     (Item  : in Stream_Element_Offset;
      Width : in Ada.Text_IO.Field       := 0;
      Base  : in Ada.Text_IO.Number_Base := 10)
     renames Stream_Element_Offset_IO.Put;
   procedure Get
     (File  : in     Ada.Text_IO.File_Type;
      Item  :    out Stream_Element_Offset;
      Width : in     Ada.Text_IO.Field := 0)
     renames Stream_Element_Offset_IO.Get;
   procedure Get
     (Item  :    out Stream_Element_Offset;
      Width : in     Ada.Text_IO.Field := 0)
     renames Stream_Element_Offset_IO.Get;

   package Stream_Element_Array_IO is new SAL.Gen_Array_Text_IO.Unconstrained_Modular_1D
     (Element_Type             => Stream_Element,
      Index_Type               => Stream_Element_Offset,
      Index_Array_Element_Type => Stream_Element_Array,
      Element_Put              => Put,
      Element_Get              => Get);
   procedure Put
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Stream_Element_Array;
      Width             : in Ada.Text_IO.Field        := Stream_Element_Array_IO.Default_Width;
      Base              : in Ada.Text_IO.Field        := Stream_Element_Array_IO.Default_Base;
      Single_Line       : in Boolean                  := Stream_Element_Array_IO.Default_Single_Line;
      Named_Association : in Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Put;

   procedure Put
     (Item              : in Stream_Element_Array;
      Width             : in Ada.Text_IO.Field        := Stream_Element_Array_IO.Default_Width;
      Base              : in Ada.Text_IO.Field        := Stream_Element_Array_IO.Default_Base;
      Single_Line       : in Boolean                  := Stream_Element_Array_IO.Default_Single_Line;
      Named_Association : in Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Put;

   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Stream_Element_Array;
      Single_Line       : in Boolean                  := Stream_Element_Array_IO.Default_Single_Line;
      Named_Association : in Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Put_Item;

   procedure Get
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Stream_Element_Array;
      Width             : in     Ada.Text_IO.Field        := 0;
      Named_Association : in     Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Get;

   procedure Get
     (Item              :    out Stream_Element_Array;
      Width             : in     Ada.Text_IO.Field        := 0;
      Named_Association : in     Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Get;

   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Stream_Element_Array;
      Named_Association : in     Boolean                  := Stream_Element_Array_IO.Default_Named_Association)
     renames Stream_Element_Array_IO.Get_Item;

end Ada_Streams_Text_IO;
