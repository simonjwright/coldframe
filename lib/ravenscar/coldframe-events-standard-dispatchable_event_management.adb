separate (ColdFrame.Events.Standard)
package body Dispatchable_Event_Management is

   protected body Dispatchable_Events is

      procedure Post (The_Event : not null Event_P) is
      begin
         if The_Event.all in Instance_Event_Base'Class then
            The_Instance_Events.Append (The_Event);
         else
            The_Class_Events.Append (The_Event);
         end if;
         Fetchable_Event := True;
      end Post;

      procedure Post_To_Self (The_Event : not null Event_P) is
         use type Ada.Task_Identification.Task_Id;
      begin
         --  We need to be sure that only event handlers called by our
         --  Dispatcher post events-to-self.
         if Owner /= The_Queue.The_Dispatcher'Identity
         then
            raise Exceptions.Use_Error
               with "posting to self outside event handler";
         elsif not (The_Event.all in Instance_Event_Base'Class) then
            raise Exceptions.Use_Error
              with "posting class event to self";
         else
            The_Self_Events.Append (The_Event);
         end if;
         Fetchable_Event := True;
      end Post_To_Self;

      entry Fetch (The_Event : out Event_P) when Fetchable_Event is
      begin
         The_Event := null;
         if not The_Self_Events.Is_Empty then
            The_Event := The_Self_Events.First_Element;
            The_Self_Events.Delete_First;
         elsif not The_Instance_Events.Is_Empty then
            The_Event := The_Instance_Events.First_Element;
            The_Instance_Events.Delete_First;
         elsif not The_Class_Events.Is_Empty then
            The_Event := The_Class_Events.First_Element;
            The_Class_Events.Delete_First;
         end if;
         pragma Assert (The_Event /= null,
                        "failed to fetch valid event");
         Check_Fetchable_Event;
      end Fetch;

      procedure Invalidate_Events
        (For_The_Instance : not null access Instance_Base'Class) is

         procedure Invalidate_Events
           (Using : in out Event_Queues.Cursor);
         procedure Invalidate_Events
           (Using : in out Event_Queues.Cursor) is
            use type Event_Queues.Cursor;
         begin
            while Using /= Event_Queues.No_Element loop
               Invalidate (Event_Queues.Element (Using),
                           If_For_Instance =>
                             Instance_Base_P (For_The_Instance));
               Event_Queues.Next (Using);
            end loop;
         end Invalidate_Events;

         Self_Iterator : Event_Queues.Cursor
           := The_Self_Events.First;
         Instance_Iterator : Event_Queues.Cursor
           := The_Instance_Events.First;

         --  We need to check held events, which will be on the class
         --  queue (since they aren't instance events).
         Class_Iterator : Event_Queues.Cursor
           := The_Class_Events.First;

      begin
         Invalidate_Events (Self_Iterator);
         Invalidate_Events (Instance_Iterator);
         Invalidate_Events (Class_Iterator);
         --  no need to Check_Fetchable_Event;
      end Invalidate_Events;

      procedure Done is
      begin
         null;
         Check_Fetchable_Event;
      end Done;

      procedure Check_Fetchable_Event is
      begin
         Fetchable_Event := not (The_Self_Events.Is_Empty
                                   and then The_Instance_Events.Is_Empty
                                   and then The_Class_Events.Is_Empty);
      end Check_Fetchable_Event;

   end Dispatchable_Events;

end Dispatchable_Event_Management;
