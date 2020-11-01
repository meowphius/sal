-- Abstract:
--  A memory stream type that reads from an address, for processing data
--  returned by system calls.
--
--  These are input streams only, used when a system routine returns the
--  address of a buffer whose size is not known.
--
with System;
package SAL.Memory_Streams.Address is
   pragma Preelaborate; -- SAL.Memory_Streams is.

   type Stream_Type is new Root_Stream_Type with private;

   procedure Create
      (Stream  : in out Stream_Type;
       Address : in     System.Address);
   -- Create a Stream with data at Address, with direction In_Stream, for
   -- reading. The stream has no end. Data at Address is NOT copied.

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out    Stream_Element_Array;
      Last   : out    Stream_Element_Offset);
   -- reads elements from Stream, storing them in Item. Stops when
   -- Item'Last is reached, setting Last to Item'Last.

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Stream_Element_Array);
   -- raises Status_Error.

   -- the following are here because they involve peeking ahead in the
   -- stream, which is not supported for Ada.Streams.Root_Stream_Type

   function Null_Terminated_String_Length
      (Stream : in Stream_Type)
      return Natural;
   -- return number of characters to first 0 in Stream.
   -- Stream pointer is NOT advanced

   function Null_Terminated_Wide_String_Length
      (Stream : in Stream_Type)
      return Natural;
   -- return number of wide characters to first wide 0 in Stream.
   -- Stream pointer is NOT advanced

private
   type Stream_Type is new Root_Stream_Type with record
      -- Address is next Storage_Element to read; incremented with each Read.
      Address : System.Address;
   end record;

end SAL.Memory_Streams.Address;
