--  Abstract :
--
--  Generic Text Output packages for various types of arrays, using Ada
--  positional aggregate syntax, based on Ada.Text_IO.
--
--  Design :
--
--  Compatible with Ada 83. Ada 83 does not have Text_IO.Look_Ahead,
--  so it cannot implement SAL.Text_IO_Utils.Skip_Whitespace. So no
--  Get subprograms are provided.
--
--  Each package declares a Put_Item that matches
--  Private_1D.Put_Element, for composing into higher-dimension
--  arrays. It uses the package defaults for the missing parameters.
--  It is not named Put, to avoid ambiguity with the other Put with
--  defaulted parameters.
--
--  Copyright (C) 2001, 2003 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Text_IO;
package Gen_Array_Text_IO_83 is

   ----------
   -- Integer_1D
   generic
      type Element_Type is range <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (File  : in Text_IO.File_Type;
         Item  : in Element_Type;
         Width : in Text_IO.Field;
         Base  : in Text_IO.Number_Base);

      Init_Default_Width             : in Text_IO.Field       := Element_Type'Width;
      Init_Default_Base              : in Text_IO.Number_Base := 10;
      Init_Default_Single_Line       : in Boolean             := True;
      Init_Default_Named_Association : in Boolean             := False;

   package Integer_1D is

      Default_Width             : Text_IO.Field       := Init_Default_Width;
      Default_Base              : Text_IO.Number_Base := Init_Default_Base;
      Default_Single_Line       : Boolean             := Init_Default_Single_Line;
      Default_Named_Association : Boolean             := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Integer_1D;

   ----------
   --  Unconstrained_Integer_1D
   generic
      type Element_Type is range <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (File  : in Text_IO.File_Type;
         Item  : in Element_Type;
         Width : in Text_IO.Field;
         Base  : in Text_IO.Number_Base);

      Init_Default_Width             : in Text_IO.Field       := Element_Type'Width;
      Init_Default_Base              : in Text_IO.Number_Base := 10;
      Init_Default_Single_Line       : in Boolean             := True;
      Init_Default_Named_Association : in Boolean             := False;

   package Unconstrained_Integer_1D is

      Default_Width             : Text_IO.Field       := Init_Default_Width;
      Default_Base              : Text_IO.Number_Base := Init_Default_Base;
      Default_Single_Line       : Boolean             := Init_Default_Single_Line;
      Default_Named_Association : Boolean             := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Unconstrained_Integer_1D;

   ----------
   -- Enumeration_1D
   generic
      type Element_Type is (<>);

      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (File  : in Text_IO.File_Type;
         Item  : in Element_Type;
         Width : in Text_IO.Field;
         Set   : in Text_IO.Type_Set);

      Init_Default_Width             : in Text_IO.Field    := Element_Type'Width;
      Init_Default_Setting           : in Text_IO.Type_Set := Text_IO.Upper_Case;
      Init_Default_Single_Line       : in Boolean          := True;
      Init_Default_Named_Association : in Boolean          := False;

   package Enumeration_1D is

      Default_Width             : Text_IO.Field    := Init_Default_Width;
      Default_Setting           : Text_IO.Type_Set := Init_Default_Setting;
      Default_Single_Line       : Boolean          := Init_Default_Single_Line;
      Default_Named_Association : Boolean          := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Enumeration_1D;

   ----------
   -- Unconstrained_Enumeration_1D
   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (File  : in Text_IO.File_Type;
         Item  : in Element_Type;
         Width : in Text_IO.Field;
         Set   : in Text_IO.Type_Set);

      Init_Default_Width             : in Text_IO.Field    := Element_Type'Width;
      Init_Default_Setting           : in Text_IO.Type_Set := Text_IO.Upper_Case;
      Init_Default_Single_Line       : in Boolean          := True;
      Init_Default_Named_Association : in Boolean          := False;

   package Unconstrained_Enumeration_1D is

      Default_Width             : Text_IO.Field    := Init_Default_Width;
      Default_Setting           : Text_IO.Type_Set := Init_Default_Setting;
      Default_Single_Line       : Boolean          := Init_Default_Single_Line;
      Default_Named_Association : Boolean          := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Unconstrained_Enumeration_1D;

   ----------
   --  Float_1D
   generic
      type Element_Type is digits <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;

      with procedure Element_Put
        (File : in Text_IO.File_Type;
         Item : in Element_Type;
         Fore : in Text_IO.Field;
         Aft  : in Text_IO.Field;
         Exp  : in Text_IO.Field);

      -- Same defaults as Ada.Text_IO.Float_IO
      Init_Default_Fore              : in Text_IO.Field := 2;
      Init_Default_Aft               : in Text_IO.Field := Element_Type'Digits - 1;
      Init_Default_Exp               : in Text_IO.Field := 3;
      Init_Default_Single_Line       : in Boolean       := True;
      Init_Default_Named_Association : in Boolean       := False;

   package Float_1D is

      Default_Fore              : Text_IO.Field := Init_Default_Fore;
      Default_Aft               : Text_IO.Field := Init_Default_Aft;
      Default_Exp               : Text_IO.Field := Init_Default_Exp;
      Default_Single_Line       : Boolean       := Init_Default_Single_Line;
      Default_Named_Association : Boolean       := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Float_1D;

   ----------
   --  Unconstrained_Float_1D
   generic
      type Element_Type is digits <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;

      with procedure Element_Put
        (File : in Text_IO.File_Type;
         Item : in Element_Type;
         Fore : in Text_IO.Field;
         Aft  : in Text_IO.Field;
         Exp  : in Text_IO.Field);

      -- Same defaults as Ada.Text_IO.Float_IO
      Init_Default_Fore              : in Text_IO.Field := 2;
      Init_Default_Aft               : in Text_IO.Field := Element_Type'Digits - 1;
      Init_Default_Exp               : in Text_IO.Field := 3;
      Init_Default_Single_Line       : in Boolean       := True;
      Init_Default_Named_Association : in Boolean       := False;

   package Unconstrained_Float_1D is

      Default_Fore              : Text_IO.Field := Init_Default_Fore;
      Default_Aft               : Text_IO.Field := Init_Default_Aft;
      Default_Exp               : Text_IO.Field := Init_Default_Exp;
      Default_Single_Line       : Boolean       := Init_Default_Single_Line;
      Default_Named_Association : Boolean       := Init_Default_Named_Association;

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);
      procedure Put
         (Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association);

   end Unconstrained_Float_1D;

   ----------
   --  Private_1D
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;

      with procedure Element_Put
        (File              : in Text_IO.File_Type;
         Item              : in Element_Type;
         Single_Line       : in Boolean           := False;
         Named_Association : in Boolean           := False);
      -- If Single_Line, try to fit Item on a single line. If
      -- Named_Association, use named association for components.

      Init_Default_Single_Line_Array         : in Boolean := True;
      Init_Default_Named_Association_Array   : in Boolean := False;
      Init_Default_Single_Line_Element       : in Boolean := True;
      Init_Default_Named_Association_Element : in Boolean := False;

   package Private_1D is

      Default_Single_Line_Array         : Boolean := Init_Default_Single_Line_Array;
      Default_Named_Association_Array   : Boolean := Init_Default_Named_Association_Array;
      Default_Single_Line_Element       : Boolean := Init_Default_Single_Line_Element;
      Default_Named_Association_Element : Boolean := Init_Default_Named_Association_Element;

      procedure Put
         (File                      : in Text_IO.File_Type;
          Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element);

      procedure Put
         (Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean := Default_Single_Line_Array;
          Named_Association : in Boolean := Default_Named_Association_Array);

   end Private_1D;

   ----------
   --  Unconstrained_Private_1D
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;

      with procedure Element_Put
        (File              : in Text_IO.File_Type;
         Item              : in Element_Type;
         Single_Line       : in Boolean           := False;
         Named_Association : in Boolean           := False);
      -- If Single_Line, try to fit Item on a single line. If
      -- Named_Association, use named association for components.

      Init_Default_Single_Line_Array         : in Boolean := True;
      Init_Default_Named_Association_Array   : in Boolean := False;
      Init_Default_Single_Line_Element       : in Boolean := True;
      Init_Default_Named_Association_Element : in Boolean := False;

   package Unconstrained_Private_1D is

      Default_Single_Line_Array         : Boolean := Init_Default_Single_Line_Array;
      Default_Named_Association_Array   : Boolean := Init_Default_Named_Association_Array;
      Default_Single_Line_Element       : Boolean := Init_Default_Single_Line_Element;
      Default_Named_Association_Element : Boolean := Init_Default_Named_Association_Element;

      procedure Put
         (File                      : in Text_IO.File_Type;
          Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element);

      procedure Put
         (Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element);

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean := Default_Single_Line_Array;
          Named_Association : in Boolean := Default_Named_Association_Array);

   end Unconstrained_Private_1D;

end Gen_Array_Text_IO_83;
