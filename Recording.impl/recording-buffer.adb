with Ada.Finalization;
with Ada.Streams;
with BC.Support.Memory_Streams;

--  Controls the storage of items to be recorded.

separate (Recording)
protected body Buffer is


   --  Called to indicate that the currently Fetched item has been
   --  dealt with.
   procedure Done is
      pragma Assert (Capacity > 0,
                     "Buffer.Done called before Initialize");
      pragma Assert (Fetched,
                     "Buffer.Done called when no Fetch outstanding");
   begin
      Recording_Support.Queues.Pop (Outstanding);
      Fetched := False;
   end Done;


   --  Blocks until there is an item ready for output to the recording
   --  medium.
   --  After recording is complete, user MUST call Done() to remove
   --  the item from the Outstanding queue.
   entry Fetch_Stream
     (S : out Stream) when Recording_Support.Queues.Length (Outstanding) > 0 is
      pragma Assert (Capacity > 0,
                     "Buffer.Fetch_Stream called before Initialize");
      pragma Assert (Fetched,
                     "Buffer.Fetch_Stream called when already Fetched");
   begin
      S := Recording_Support.Queues.Front (Outstanding).Str;
      Fetched := True;
   end Fetch_Stream;


   --  Returns an empty Memory Stream capable of holding up to
   --  Capacity bytes, or null if the Buffer is full. The designated
   --  stream is held within the Outstanding queue.
   procedure Get_Stream
     (Capacity : Positive := 1024;
      S : out Stream) is
      pragma Assert (Capacity > 0,
                     "Buffer.Get_Stream called before Initialize");
   begin
      if Capacity = 0 then
         raise Use_Error;
      end if;
      Count := Count + 1;
      if Recording_Support.Queues.Length (Outstanding) < Capacity then
         S := new BC.Support.Memory_Streams.Stream_Type
           (Ada.Streams.Stream_Element_Offset (Capacity));
         Recording_Support.Queues.Append
           (Outstanding, (Ada.Finalization.Controlled with Str => S));
      else
         Dropped := Dropped + 1;
         S := null;
      end if;
   end Get_Stream;


   --  Initializes the buffer.
   procedure Initialize
     (Capacity : Positive := 1024) is
      pragma Assert (Capacity = 0,
                     "Buffer.Initialize called before Initialize");
   begin
      Buffer.Capacity := Capacity;
   end Initialize;


   --  Provides information on the capacity of the Buffer, the number
   --  of items presented for recording, and the number that had to be
   --  dropped because the capacity was exceeded.
   procedure Statistics
     (Capacity : out Positive;
      Items : out Natural;
      Dropped : out Natural) is
   begin
      Capacity := Buffer.Capacity;
      Items := Buffer.Count;
      Dropped := Buffer.Dropped;
   end Statistics;


end Buffer;
