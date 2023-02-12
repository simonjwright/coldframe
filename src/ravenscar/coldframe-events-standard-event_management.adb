--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

separate (ColdFrame.Events.Standard)
package body Event_Management is

   --  Held events:
   ----------------

   function Time_Cell_Less (L, R : Time_Cell) return Boolean;
   function Time_Cell_Less (L, R : Time_Cell) return Boolean is
      use type Ada.Real_Time.Time;
   begin
      return L.Time_To_Fire < R.Time_To_Fire;
   end Time_Cell_Less;

   package Sorting_Time_Vectors is new Time_Vectors.Generic_Sorting
     ("<" => Time_Cell_Less);

   protected body All_Events is

      --  Immediately-actionable events:
      ----------------------------------

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

         Self_Iterator : Event_Queues.Cursor := The_Self_Events.First;
         Instance_Iterator : Event_Queues.Cursor := The_Instance_Events.First;

         --  We need to check actionable held events, which will be on
         --  the class queue (since they aren't instance events).
         Class_Iterator : Event_Queues.Cursor := The_Class_Events.First;

         --  We may not actually need to check Duration events
         --  (posted, before the Queue is actually started, to be run
         --  after an interval)? still, ...
         procedure Invalidate_Events
           (Using : in out Duration_Vectors.Cursor);
         procedure Invalidate_Events
           (Using : in out Duration_Vectors.Cursor) is
            use type Duration_Vectors.Cursor;
         begin
            while Using /= Duration_Vectors.No_Element loop
               Invalidate (Duration_Vectors.Element (Using).Event,
                           If_For_Instance =>
                             Instance_Base_P (For_The_Instance));
               Duration_Vectors.Next (Using);
            end loop;
         end Invalidate_Events;

         --  We need to check 'duration' held events.
         Duration_Iterator : Duration_Vectors.Cursor
           := The_Duration_Events.First;

         procedure Invalidate_Events
           (Using : in out Time_Vectors.Cursor);
         procedure Invalidate_Events
           (Using : in out Time_Vectors.Cursor) is
            use type Time_Vectors.Cursor;
         begin
            while Using /= Time_Vectors.No_Element loop
               Invalidate (Time_Vectors.Element (Using).Event,
                           If_For_Instance =>
                             Instance_Base_P (For_The_Instance));
               Time_Vectors.Next (Using);
            end loop;
         end Invalidate_Events;

         --  We need to check held held events.
         Held_Event_Iterator : Time_Vectors.Cursor := The_Held_Events.First;

      begin
         Invalidate_Events (Self_Iterator);
         Invalidate_Events (Instance_Iterator);
         Invalidate_Events (Class_Iterator);
         --  no need to Check_Fetchable_Event (XXX why not?)
         Invalidate_Events (Duration_Iterator);
         Invalidate_Events (Held_Event_Iterator);
      end Invalidate_Events;

      procedure Done is
      begin
         Check_Fetchable_Event;
      end Done;

      procedure Check_Fetchable_Event is
      begin
         Fetchable_Event := not (The_Self_Events.Is_Empty
                                   and then The_Instance_Events.Is_Empty
                                   and then The_Class_Events.Is_Empty);
      end Check_Fetchable_Event;

      --  Held events:
      ---------------

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
                  The_Held_Events.Append
                    ((Time_To_Fire => Clock +
                        Ada.Real_Time.To_Time_Span (D.Delay_To_Fire),
                      Event => D.Event));
               end;
            end loop;
         end Transfer_Duration_Events;
         Duration_Iterator : Duration_Vectors.Cursor
           := The_Duration_Events.First;
      begin
         Transfer_Duration_Events (Duration_Iterator);
         The_Duration_Events.Clear;
         Sorting_Time_Vectors.Sort (The_Held_Events.all);
         Started := True;
      end Running;

      procedure Add_At_Event (The_Entry : not null Event_P;
                              To_Run_At : Ada.Real_Time.Time) is
      begin
         The_Held_Events.Append ((Time_To_Fire => To_Run_At,
                                   Event => The_Entry));
         Sorting_Time_Vectors.Sort (The_Held_Events.all);
      end Add_At_Event;

      procedure Add_After_Event (The_Entry    : not null Event_P;
                                 To_Run_After : Duration) is
         use type Ada.Real_Time.Time;
      begin
         if Started then
            The_Held_Events.Append
              ((Time_To_Fire => Ada.Real_Time.Clock +
                  Ada.Real_Time.To_Time_Span (To_Run_After),
                Event => The_Entry));
            Sorting_Time_Vectors.Sort (The_Held_Events.all);
         else
            The_Duration_Events.Append ((Delay_To_Fire => To_Run_After,
                                    Event => The_Entry));
         end if;
      end Add_After_Event;

      procedure Remove_Held_Event (An_Event : not null Event_P) is
         procedure Free_Event (E : Event_P);
         procedure Free_Event (E : Event_P) is
            Deletable : Event_P := E;
            Owned_Deletable : Event_P := Held_Event (Deletable.all).The_Event;
         begin
            Delete (Owned_Deletable);
            Delete (Deletable);
         end Free_Event;
      begin
         for J in reverse 1 .. The_Duration_Events.Last_Index loop
            if The_Duration_Events.Element (J).Event = An_Event then
               Free_Event (The_Duration_Events.Element (J).Event);
               The_Duration_Events.Delete (J);
            end if;
         end loop;
         for J in reverse 1 .. The_Held_Events.Last_Index loop
            if The_Held_Events.Element (J).Event = An_Event then
               Free_Event (The_Held_Events.Element (J).Event);
               The_Held_Events.Delete (J);
            end if;
         end loop;
      end Remove_Held_Event;

      procedure Make_Events_Actionable
        (If_At_Or_Before : Ada.Real_Time.Time) is
         use type Ada.Real_Time.Time;
      begin
         pragma Assert (Started, "Fetch called before Running");
         while not The_Held_Events.Is_Empty
           and then The_Held_Events.First_Element.Time_To_Fire
           <= If_At_Or_Before
         loop
            declare
               Ev : constant Event_P := The_Held_Events.First_Element.Event;
            begin
               The_Held_Events.Delete_First;
               Post (Ev);
            end;
         end loop;
      end Make_Events_Actionable;

   end All_Events;

   task body Ticker is
      use type Ada.Real_Time.Time;
      Clock : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Tick : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.To_Time_Span (Duration'(System.Tick));
   begin
      The_Queue.The_Events.Running;
      --  This will move any Duration events to (Real_Time-based) Held
      --  events.
      loop
         delay until Clock + Tick;
         Clock := Ada.Real_Time.Clock;
         The_Queue.The_Events.Make_Events_Actionable
           (If_At_Or_Before => Clock);
      end loop;
   end Ticker;

end Event_Management;
