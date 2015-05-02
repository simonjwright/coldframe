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

with ColdFrame.Exceptions;
with ColdFrame.Times;

package body ColdFrame.Events.Standard is

   package body Dispatchable_Event_Management is separate;
   package body Held_Event_Management is separate;

   --  Remember, in the instance to which this event is directed,
   --  which event queue was used. This is so that (when the instance
   --  terminates) we can invalidate all events that were directed to
   --  it.
   --
   --  Because of this use, it's necessary that (if any events are
   --  posted at all) they should all be to the same event queue.
   procedure Note (The_Queue : access Event_Queue_Base;
                   Used_By_The_Instance_Of : Event_P);
   procedure Note (The_Queue : access Event_Queue_Base;
                   Used_By_The_Instance_Of : Event_P) is
   begin

      if Used_By_The_Instance_Of.all in Instance_Event_Base'Class then

         declare
            I : Events.Instance_Base'Class
              renames Instance_Event_Base
              (Used_By_The_Instance_Of.all).For_The_Instance.all;
         begin
            if I.Events_Posted_On = null then
               I.Events_Posted_On := Event_Queue_P (The_Queue);
            elsif I.Events_Posted_On /= Event_Queue_P (The_Queue) then
               raise Exceptions.Use_Error
                 with "attempt to post instance event on different queue";
            end if;
         end;

      end if;

   end Note;


   procedure Post (The_Event : not null Event_P;
                   On        : not null access Event_Queue_Base) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      On.The_Dispatchable_Events.Post (The_Event);

   end Post;


   procedure Post_To_Self (The_Event : not null Event_P;
                           On : not null access Event_Queue_Base) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      On.The_Dispatchable_Events.Post_To_Self (The_Event);

   end Post_To_Self;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_At : Ada.Real_Time.Time) is
      TEP : constant Event_P := new Held_Event (On_Timer => False);
      TE : Held_Event renames Held_Event (TEP.all);
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := To_Fire_At;
      TE.The_Event := The_Event;
      On.The_Held_Events.Add_At_Event (TEP, To_Fire_At);

   end Post;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_After : Natural_Duration) is
      TEP : constant Event_P := new Held_Event (On_Timer => False);
      TE : Held_Event renames Held_Event (TEP.all);
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := Times.From_Now (To_Fire_After);
      --  NB, this will be wrong if the queue isn't yet started.
      TE.The_Event := The_Event;
      On.The_Held_Events.Add_After_Event (TEP, To_Fire_After);

   end Post;


   task body Dispatcher is

      E : Event_P;

   begin

      loop

         The_Queue.The_Dispatchable_Events.Fetch (E);

         if not E.Invalidated then
            Start_Handling (E);
            Handler (E.all);
            Stop_Handling (E);
         end if;

         Delete (E);

         The_Queue.The_Dispatchable_Events.Done;

      end loop;

   end Dispatcher;


   procedure Set (The_Timer : in out          Timer;
                  On        : not null access Event_Queue_Base;
                  To_Fire   :                 not null Event_P;
                  At_Time   :                 Ada.Real_Time.Time) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => To_Fire);

      if The_Timer.The_Entry = null then

         The_Timer.The_Entry := new Held_Event (On_Timer => True);
         declare
            TE : Held_Event renames Held_Event (The_Timer.The_Entry.all);
         begin
            TE.On := Event_Queue_P (On);
            TE.Time_To_Fire := At_Time;
            TE.The_Event := To_Fire;
            TE.The_Timer := The_Timer'Unrestricted_Access;
         end;
         On.The_Held_Events.Add_At_Event (The_Timer.The_Entry, At_Time);

      else

         --  XXX may not be all ...
         raise Exceptions.Use_Error
           with "attempt to set an already-set timer";

      end if;

   end Set;


   procedure Set (The_Timer : in out          Timer;
                  On        : not null access Event_Queue_Base;
                  To_Fire   :                 not null Event_P;
                  After     :                 Natural_Duration) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => To_Fire);

      if The_Timer.The_Entry = null then

         The_Timer.The_Entry := new Held_Event (On_Timer => True);
         declare
            TE : Held_Event renames Held_Event (The_Timer.The_Entry.all);
         begin
            TE.On := Event_Queue_P (On);
            TE.Time_To_Fire := Times.From_Now (After);
            --  NB, this will be wrong if the queue isn't yet started.
            TE.The_Event := To_Fire;
            TE.The_Timer := The_Timer'Unrestricted_Access;
         end;
         On.The_Held_Events.Add_After_Event (The_Timer.The_Entry, After);

      else

         --  XXX may not be all ...
         raise Exceptions.Use_Error
           with "attempt to set an already-set timer";

      end if;

   end Set;


   procedure Unset (The_Timer : in out Timer;
                    On : not null access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin

      if The_Timer.The_Entry = null then

         raise Exceptions.Use_Error
           with "attempt to unset a timer that wasn't set";

      else

         declare
            Ev : constant Event_P := The_Timer.The_Entry;
            HE : Held_Event renames Held_Event (Ev.all);
         begin
            --  Cancel the Event
            HE.The_Event.Invalidated := True;

            --  Indicate the Timer's already unset
            HE.The_Timer := null;

            --  Unset the Timer
            The_Timer.The_Entry := null;

            --  Remove the Event from the held event queue (if it
            --  hasn't already made it to the Held Event Manager task
            --  or to the Dispatcher)
            On.The_Held_Events.Remove_Event (Ev);
         end;

      end if;

   end Unset;


   procedure Invalidate_Events
     (On               : not null access Event_Queue_Base;
      For_The_Instance : not null access Instance_Base'Class) is
   begin

      --  Called to mark all events for For_The_Instance as
      --  invalidated, so that they won't be dispatched.

      --  First, tell Held_Event_Management to mark all its held
      --  events that reference the departing instance;
      On.The_Held_Events.Invalidate_Events
        (Instance_Base_P (For_The_Instance));
      --  after which there can't be any more left (even one that was
      --  in the process of being posted must have gone, because it's
      --  processed elsewhere in the Held_Event_Manager task). XXX

      --  Next, tell Dispatchable_Event_Management the same.
      On.The_Dispatchable_Events.Invalidate_Events (For_The_Instance);

   end Invalidate_Events;


end ColdFrame.Events.Standard;
