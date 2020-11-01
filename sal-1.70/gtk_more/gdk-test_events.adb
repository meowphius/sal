--  Abstract :
--
--  See spec
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

package body Gdk.Test_Events is

   Small_Delay : constant Duration := 0.01;

   procedure Key_Event_Special
     (Key    : in Key_Type;
      Key_Up : in Boolean)
      is separate;
   --  Generate key event, _not_ followed by key_delay. If multiple
   --  events are required, use Small_Delay as delay between them.

   procedure Key_Stroke_ASCII (Key : in Character) is separate;
   --  Generate key event, _not_ followed by key_delay. If multiple
   --  events are required, use Small_Delay as delay between them.

   procedure Key_Event
     (Key       : in Key_Type;
      Key_Up    : in Boolean;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Key, Key_Up);
      delay Key_Delay;
   end Key_Event;

   procedure Key_Stroke
     (Key       : in Key_Type;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Key, Key_Up => False);
      delay Small_Delay;
      Key_Event_Special (Key, Key_Up => True);
      delay Key_Delay;
   end Key_Stroke;

   procedure Alt_Key_Stroke
     (Key : in Character;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event (Alt, Key_Up => False, Key_Delay => Small_Delay);
      Key_Stroke_ASCII (Key);
      Key_Event (Alt, Key_Up => True, Key_Delay => Key_Delay);
   end Alt_Key_Stroke;

   procedure Key_Stroke
     (Keys      : in String;
      Key_Delay : in Duration := Default_Delay)
   is begin
      for I in Keys'Range loop
         if Keys (I) in ' ' .. '~' then
            Key_Stroke_ASCII (Keys (I));
            delay Key_Delay;
         end if;
      end loop;
   end Key_Stroke;

   procedure Shift_Tab (Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Shift, Key_Up => False);
      delay Small_Delay;
      Key_Event_Special (Tab, Key_Up => False);
      delay Small_Delay;
      Key_Event_Special (Tab, Key_Up => True);
      delay Small_Delay;
      Key_Event_Special (Shift, Key_Up => True);
      delay Key_Delay;
   end Shift_Tab;

   -----------
   --  Mouse events

   procedure Mouse_Move_Event (Point : in Point_Type) is separate;
   --  Move mouse to absolute screen position Point.

   procedure Mouse_Button_Event
     (Button    : in Glib.Gint;
      Button_Up : in Boolean)
     is separate;

   function "+" (Left, Right : in Point_Type) return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : in Point_Type) return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function "*"
     (Left : in Point_Type;
      Right : in Glib.Gint)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X * Right, Left.Y * Right);
   end "*";

   function "*"
     (Left : in Glib.Gint;
      Right : in Point_Type)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left * Right.X, Left * Right.Y);
   end "*";

   function "/"
     (Left : in Point_Type;
      Right : in Glib.Gint)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X / Right, Left.Y / Right);
   end "/";

   function Image (Item : in Point_Type) return String is
   begin
      return "(" & Glib.Gint'Image (Item.X) &
         ", " & Glib.Gint'Image (Item.Y) & ")";
   end Image;

   procedure Mouse_Button
     (Button    : in Glib.Gint;
      Button_Up : in Boolean)
     renames Mouse_Button_Event;

   procedure Mouse_Move (Point : in Point_Type) renames Mouse_Move_Event;

   procedure Mouse_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay)
   is begin
      Mouse_Button_Event (Button, Button_Up => False);
      delay Mouse_Delay;
      Mouse_Button_Event (Button, Button_Up => True);
      delay Mouse_Delay;
   end Mouse_Click;

   procedure Mouse_Double_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay)
   is begin
      Mouse_Click (Button, 0.0);
      Mouse_Click (Button, Mouse_Delay);
   end Mouse_Double_Click;

end Gdk.Test_Events;
