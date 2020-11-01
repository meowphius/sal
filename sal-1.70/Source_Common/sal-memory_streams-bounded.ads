-- Abstract:
--  A memory stream type, for obtaining raw byte images of types.
--
with System;
with Ada.Streams; use Ada.Streams;
package SAL.Memory_Streams.Bounded is
   pragma Preelaborate; -- SAL.Memory_Streams is.

   type Stream_Type (Max_Length : Stream_Element_Count)
   is new Root_Stream_Type with private;

   procedure Create (Stream : in out Stream_Type);
   -- create an empty Stream with direction Out_Stream, for writing.

   procedure Create
      (Stream : in out Stream_Type;
       Data : in Stream_Element_Array);
   -- create a Stream with data, with direction In_Stream, for reading.
   -- raises Constraint_Error if Data overflows Stream

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address);
   -- create a Stream with data from Address, copying Stream.Max_Length
   -- bytes, with direction In_Stream, for reading.

   function Length (Stream : in Stream_Type) return Stream_Element_Count;
   -- for an In_Stream, the amount of data left to be read.
   -- for an Out_Stream, the amount of data written.

   function Address (Stream : in Stream_Type) return System.Address;
   -- for an In_Stream, raises Status_Error.
   -- for an Out_Stream, the address of the first element of the raw
   -- Stream, for passing to system routines.

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   -- for an In_Stream, reads elements from Stream, storing them in
   -- Item. Stops when Item'Last or end of Stream is reached, setting Last to
   -- last element of Item written.
   --
   -- for an Out_Stream, raises Status_Error.

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array);
   -- for an In_Stream, raises Status_Error.
   --
   -- for an Out_Stream, writes elements from Item to the Stream, stopping
   -- when Item'last is reached. Raises End_Error if attempt
   -- to write past end of Stream.

private
   type Stream_Type (Max_Length : Stream_Element_Count)
   is new Ada.Streams.Root_Stream_Type with
   record
      -- Direction is not a discriminant, because we anticipate changing
      -- direction on some streams.
      Direction : Direction_Type;
      Last : Stream_Element_Offset := 0; -- last element of Raw that has been read/written
      Raw : Stream_Element_Array (1 .. Max_Length);
   end record;

end SAL.Memory_Streams.Bounded;
