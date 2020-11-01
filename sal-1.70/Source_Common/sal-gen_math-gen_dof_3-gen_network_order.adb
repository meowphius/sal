--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Math.Gen_Dof_3.Gen_Network_Order is

   procedure To_Network
     (Item   : in     Cart_Vector_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Count)
   is begin
      for I in Item'Range loop
         To_Network (Item (I), Buffer, Last);
      end loop;
   end To_Network;

   procedure To_Network
     (Item   : in     Unit_Vector_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Count)
   is begin
      for I in Item'Range loop
         To_Network (Item (I), Buffer, Last);
      end loop;
   end To_Network;

   procedure To_Network
     (Item   : in     Unit_Quaternion_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Count)
   is begin
      To_Network (Item.X, Buffer, Last);
      To_Network (Item.Y, Buffer, Last);
      To_Network (Item.Z, Buffer, Last);
      To_Network (Item.S, Buffer, Last);
   end To_Network;

   procedure To_Network
     (Item   : in     Rot_Matrix_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Count)
   is begin
      for I in Item'Range loop
         for J in Item'Range loop
            To_Network (Item (I)(J), Buffer, Last);
         end loop;
      end loop;
   end To_Network;

   procedure To_Network
     (Item   : in     Inertia_Type;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : in out Ada.Streams.Stream_Element_Count)
   is begin
      for I in Item'Range loop
         To_Network (Item (I), Buffer, Last);
      end loop;
   end To_Network;

end SAL.Gen_Math.Gen_DOF_3.Gen_Network_Order;
