separate (ColdFrame.Events.Standard)
package body Held_Event_Management is

   function Time_Cell_Less (L, R : Time_Cell) return Boolean;
   function Time_Cell_Less (L, R : Time_Cell) return Boolean is
      use type Ada.Real_Time.Time;
   begin
      return L.Time_To_Fire < R.Time_To_Fire;
   end Time_Cell_Less;

   package Sorting_Time_Vectors is new Time_Vectors.Generic_Sorting
     ("<" => Time_Cell_Less);

   protected body Held_Events is

      procedure Running is
         procedure Transfer_Duration_Events
           (Using : in out Duration_Vectors.Cursor);
         procedure Transfer_Duration_Events
           (Using : in out Duration_Vectors.Cursor) is
            Clock : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            use type Duration_Vectors.Cursor;
            use type Ada.Real_Time.Time;
         begin
            while Using /= Duration_Vectors.No_Element loop
               declare
                  D : constant Duration_Cell :=
                    Duration_Vectors.Element (Using);
               begin
                  Held_Event_Queue.Append
                    ((Time_To_Fire => Clock +
                        Ada.Real_Time.To_Time_Span (D.Delay_To_Fire),
                      Event => D.Event));
               end;
            end loop;
         end Transfer_Duration_Events;
         Duration_Iterator : Duration_Vectors.Cursor := Duration_Queue.First;
      begin
         Transfer_Duration_Events (Duration_Iterator);
         Duration_Queue.Clear;
         Sorting_Time_Vectors.Sort (Held_Event_Queue.all);
         Started := True;
      end Running;

      procedure Fetch (An_Event        : out Event_P;
                       If_At_Or_Before :     Ada.Real_Time.Time) is
         use type Ada.Real_Time.Time;
      begin
         pragma Assert (Started, "Fetch called before Running");
         An_Event := null;
         if not Held_Event_Queue.Is_Empty
           and then If_At_Or_Before >=
           Held_Event_Queue.First_Element.Time_To_Fire
         then
            An_Event := Held_Event_Queue.First_Element.Event;
            Held_Event_Queue.Delete_First;
         end if;
      end Fetch;

      procedure Add_At_Event (The_Entry : not null Event_P;
                              To_Run_At : Ada.Real_Time.Time) is
      begin
         Held_Event_Queue.Append ((Time_To_Fire => To_Run_At,
                                   Event => The_Entry));
         Sorting_Time_Vectors.Sort (Held_Event_Queue.all);
      end Add_At_Event;

      procedure Add_After_Event (The_Entry    : not null Event_P;
                                 To_Run_After : Duration) is
         use type Ada.Real_Time.Time;
      begin
         if Started then
            Held_Event_Queue.Append
              ((Time_To_Fire => Ada.Real_Time.Clock +
                  Ada.Real_Time.To_Time_Span (To_Run_After),
                Event => The_Entry));
            Sorting_Time_Vectors.Sort (Held_Event_Queue.all);
         else
            Duration_Queue.Append ((Delay_To_Fire => To_Run_After,
                                    Event => The_Entry));
         end if;
      end Add_After_Event;

      procedure Invalidate_Events
        (For_The_Instance : not null Instance_Base_P) is
         procedure Invalidate_Duration_Events
           (Using : in out Duration_Vectors.Cursor);
         procedure Invalidate_Duration_Events
           (Using : in out Duration_Vectors.Cursor) is
            use type Duration_Vectors.Cursor;
         begin
            while Using /= Duration_Vectors.No_Element loop
               Duration_Vectors.Element (Using).Event.Invalidate
                 (If_For_Instance => For_The_Instance);
            end loop;
         end Invalidate_Duration_Events;
         Duration_Iterator : Duration_Vectors.Cursor := Duration_Queue.First;

         procedure Invalidate_Time_Events
           (Using : in out Time_Vectors.Cursor);
         procedure Invalidate_Time_Events
           (Using : in out Time_Vectors.Cursor) is
            use type Time_Vectors.Cursor;
         begin
            while Using /= Time_Vectors.No_Element loop
               Time_Vectors.Element (Using).Event.Invalidate
                 (If_For_Instance => For_The_Instance);
            end loop;
         end Invalidate_Time_Events;
         Time_Iterator : Time_Vectors.Cursor := Held_Event_Queue.First;
      begin
         Invalidate_Duration_Events (Duration_Iterator);
         Invalidate_Time_Events (Time_Iterator);
      end Invalidate_Events;

      procedure Remove_Event (An_Event : not null Event_P) is
         procedure Free_Event (E : Event_P);
         procedure Free_Event (E : Event_P) is
            Deletable : Event_P := E;
            Owned_Deletable : Event_P := Held_Event (Deletable.all).The_Event;
         begin
            Delete (Owned_Deletable);
            Delete (Deletable);
         end Free_Event;
      begin
         for J in reverse 1 .. Duration_Queue.Last_Index loop
            if Duration_Queue.Element (J).Event = An_Event then
               Free_Event (Duration_Queue.Element (J).Event);
               Duration_Queue.Delete (J);
            end if;
         end loop;
         for J in reverse 1 .. Held_Event_Queue.Last_Index loop
            if Held_Event_Queue.Element (J).Event = An_Event then
               Free_Event (Held_Event_Queue.Element (J).Event);
               Held_Event_Queue.Delete (J);
            end if;
         end loop;
      end Remove_Event;

   end Held_Events;

   task body Held_Event_Processing is
      use type Ada.Real_Time.Time;
      Clock : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Tick : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.To_Time_Span (Duration'(System.Tick));
   begin
      The_Queue.The_Held_Events.Running;
      loop
         delay until Clock + Tick;
         Clock := Ada.Real_Time.Clock;
         declare
            Ev : Event_P;
         begin
            loop
               The_Queue.The_Held_Events.Fetch (Ev, If_At_Or_Before => Clock);
               exit when Ev = null;
               The_Queue.The_Dispatchable_Events.Post (Ev);
            end loop;
         end;
      end loop;
   end Held_Event_Processing;

end Held_Event_Management;
