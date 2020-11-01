-- Abstract:
--
-- Facilities for dealing directly with the mouse.
--
-- For handling mouse events in windows, see Windex.Windows.User.
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

with Gdk.Test_Events;
package SAL.Win32.Mouse is
   pragma Preelaborate;

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
       Motion      : in Gdk.Test_Events.Point_Type := (0, 0));

end SAL.Win32.Mouse;
