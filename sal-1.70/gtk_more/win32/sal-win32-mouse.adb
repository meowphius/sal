-- Abstract:
--
-- see spec.
--
-- Copyright (C) 1998, 1999, 2004 Stephen Leake.  All Rights Reserved.
--
-- Windex is free software; you can redistribute it and/or modify it
-- under terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2, or (at your option) any
-- later version. Windex is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details. You should have received a
-- copy of the GNU General Public License distributed with Windex; see
-- file COPYING. If not, write to the Free Software Foundation, 59
-- Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- Windex, or you link Windex object files with other files to produce
-- an executable, that does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Interfaces.C;
with SAL.Interfaces_More;
package body SAL.Win32.Mouse is

   type Coordinate_Metric_Label_Type is new Interfaces.C.int;

   Screen_X : constant Coordinate_Metric_Label_Type :=  0; -- SM_CXSCREEN
   Screen_Y : constant Coordinate_Metric_Label_Type :=  1; -- SM_CYSCREEN

   function Win32_GetSystemMetrics_Coordinate
      (NIndex : in Coordinate_Metric_Label_Type)
      return Interfaces.C.long;
   pragma Import (StdCall, Win32_GetSystemMetrics_Coordinate, "GetSystemMetrics");

   function Get
      (Item : in Coordinate_Metric_Label_Type)
      return Interfaces.C.long
      renames Win32_GetSystemMetrics_Coordinate;

   type Mickeys_Point_Type is record
      X : Interfaces.C.long;
      Y : Interfaces.C.long;
   end record;

   function To_Mickeys (Point : in Gdk.Test_Events.Point_Type) return Mickeys_Point_Type
   is
      use Interfaces.C;
   begin
      return
         (X => (long (Point.X) * 65_536) / Get (Screen_X),
          Y => (long (Point.Y) * 65_536) / Get (Screen_Y));
   end To_Mickeys;

   type Win32_Mouse_Event_Flags_Type is record
      Move        : Boolean;
      Left_Down   : Boolean;
      Left_Up     : Boolean;
      Right_Down  : Boolean;
      Right_Up    : Boolean;
      Middle_Down : Boolean;
      Middle_Up   : Boolean;
      Reserved_1  : Interfaces_More.Unsigned_4;
      Wheel       : Boolean;
      Absolute    : Boolean;
      Reserved_2  : Interfaces_More.Unsigned_3;
   end record;

   for Win32_Mouse_Event_Flags_Type use record
      Move        at 0 range  0 ..  0;
      Left_Down   at 0 range  1 ..  1;
      Left_Up     at 0 range  2 ..  2;
      Right_Down  at 0 range  3 ..  3;
      Right_Up    at 0 range  4 ..  4;
      Middle_Down at 0 range  5 ..  5;
      Middle_Up   at 0 range  6 ..  6;
      Reserved_1  at 0 range  7 ..  10;
      Wheel       at 0 range 11 .. 11;
      Reserved_2  at 0 range 12 .. 14;
      Absolute    at 0 range 15 .. 15;
   end record;
   pragma Convention (C_Pass_By_Copy, Win32_Mouse_Event_Flags_Type);

   procedure Win32_Mouse_Event
      (DwFlags     : in Win32_Mouse_Event_Flags_Type;
       Dx          : in Interfaces.C.long;
       Dy          : in Interfaces.C.long;
       CButtons    : in Interfaces.C.unsigned_long   := 0;
       DwExtraInfo : in Interfaces.C.unsigned_long);
   -- Winuser.h has unsigned_long for Dx, Dy; but relative motion has to be signed!
   pragma Import (StdCall, Win32_Mouse_Event, "mouse_event");

   procedure Mouse_Event
      (Move        : in Boolean := False;
       Left_Down   : in Boolean := False;
       Left_Up     : in Boolean := False;
       Right_Down  : in Boolean := False;
       Right_Up    : in Boolean := False;
       Middle_Down : in Boolean := False;
       Middle_Up   : in Boolean := False;
       Wheel       : in Boolean := False;
       Absolute    : in Boolean := False;
       Motion      : in Gdk.Test_Events.Point_Type := (0, 0))
   is
      Mickeys : constant Mickeys_Point_Type := To_Mickeys (Motion);
   begin
      Win32_Mouse_Event
         (DwFlags =>
             (Move, Left_Down, Left_Up, Right_Down, Right_Up, Middle_Down, Middle_Up, 0,
              Wheel, Absolute, 0),
          Dx => Mickeys.X,
          Dy => Mickeys.Y,
          CButtons => 0,
          DwExtraInfo => 0);
   end Mouse_Event;

end SAL.Win32.Mouse;
