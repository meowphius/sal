--  Abstract :
--
--  Generate low-level user input events (keyboard, mouse). Primarily
--  used for testing GUI interfaces.
--
--  The body of this package is operating system and window system dependent.
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

package Gdk.Test_Events is
   pragma Preelaborate;

   Default_Delay : Duration := 0.02;
   --  Good for my 1 GHz laptop.

   type Key_Type is (Up, Down, Left, Right, Enter, Alt, Shift, Tab);
   --  Generic non-printable keys; these are the only non-printable
   --  keys we support in Gdk user interfaces.

   procedure Key_Event
     (Key       : in Key_Type;
      Key_Up    : in Boolean;
      Key_Delay : in Duration := Default_Delay);
   --  Generate a key-down or key-up event for Key, followed by
   --  Key_Delay, to allow the GUI to react.

   procedure Key_Stroke
     (Key       : in Key_Type;
      Key_Delay : in Duration := Default_Delay);
   --  Generate key-down and key-up events for Key. Each event is
   --  followed by Key_Delay, to allow the GUI to react.

   procedure Alt_Key_Stroke
     (Key : in Character;
      Key_Delay : in Duration := Default_Delay);
   --  Generate Alt key-down, key key-down, key-up, Alt key-up events,
   --  with small delay between and Key_Delay after.

   procedure Key_Stroke
     (Keys      : in String;
      Key_Delay : in Duration := Default_Delay);
   --  Generate a sequence of key-down, key-up events to type Keys,
   --  with Key_Delay after each event.
   --
   --  Ignores non-ASCII characters.

   procedure Shift_Tab (Key_Delay : in Duration := Default_Delay);
   --  Generate a "shift tab" key sequence.

   ----------
   --  Mouse events
   --
   --  Mouse buttons:
   --  1 => Left
   --  2 => Middle
   --  3 => Right

   --  Gdk is conflicted about what type to use to represent screen
   --  coordinates; Gdk.Window uses Gint (32 bit integer), Gdk.Event
   --  uses Gdouble (64 bit float).
   --
   --  Since the primary use of this package will be for generating
   --  mouse events in test drivers, which will use Gdk.Window
   --  positions as the starting point, we use Gint as the coordinate
   --  type.

   type Point_Type is record
      X : Glib.Gint;
      Y : Glib.Gint;
   end record;

   function "+" (Left, Right : in Point_Type) return Point_Type;
   function "-" (Left, Right : in Point_Type) return Point_Type;

   function "*" (Left : in Point_Type; Right : in Glib.Gint) return Point_Type;
   function "*" (Left : in Glib.Gint; Right : in Point_Type) return Point_Type;
   function "/" (Left : in Point_Type; Right : in Glib.Gint) return Point_Type;

   function Image (Item : in Point_Type) return String;
   --  Positional aggregate syntax

   procedure Mouse_Button
     (Button    : in Glib.Gint;
      Button_Up : in Boolean);

   procedure Mouse_Move (Point : in Point_Type);
   --  Move the mouse to Point (no buttons are down).

   procedure Mouse_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay);
   --  Click the indicated button; delay for Mouse_Delay after the
   --  down and up events.

   procedure Mouse_Double_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay);
   --  Double Click the indicated button; delay for Mouse_Delay after
   --  the second down and up events.

end Gdk.Test_Events;
