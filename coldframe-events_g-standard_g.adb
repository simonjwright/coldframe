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

--  $RCSfile: coldframe-events_g-standard_g.adb,v $
--  $Revision: d2cf17389759 $
--  $Date: 2002/06/25 18:35:42 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Real_Time;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G.Standard_G is


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue) is
   begin

      if The_Event.all in Instance_Event_Base'Class then

         --  Remember, in the instance to which this event is
         --  directed, which event queue was used. This is so that
         --  (when the instance terminates) we can invalidate all
         --  events that were directed to it.
         --
         --  Because of this use, it's necessary that (if any events
         --  are posted at all) they should all be to the same event
         --  queue.
         declare
            I : Events_G.Instance_Base
              renames Instance_Event_Base (The_Event.all).For_The_Instance.all;
         begin
            if I.Events_Posted_On = null then
               I.Events_Posted_On := Event_Queue_P (On);
            elsif I.Events_Posted_On /= Event_Queue_P (On) then
               Ada.Exceptions.Raise_Exception
                 (Exceptions.Use_Error'Identity,
                  "attempt to post instance event on different queue");
            end if;
         end;

      end if;

      On.The_Excluder.Post (The_Event);

   end Post;


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue;
                   To_Fire_At : Time.Time) is
      TE : Timer_Queue_Entry_P := new Timer_Event;
   begin
      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := To_Fire_At;
      TE.The_Event := The_Event;
      On.The_Timer_Manager.Append (TE);
   end Post;


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue;
                   To_Fire_After : Natural_Duration) is
   begin

      Post (The_Event => The_Event,
            On => On,
            To_Fire_At => Time.From_Now (To_Fire_After));

   end Post;


   task body Dispatcher is

      E : Event_P;

   begin

      loop

         The_Queue.The_Excluder.Fetch (E); -- blocks until there's an event

         if not E.Invalidated then

            Log_Pre_Dispatch (The_Event => E, On => The_Queue);
            Handler (E.all);
            Log_Post_Dispatch (The_Event => E, On => The_Queue);

         end if;

         Delete (E);

      end loop;

   end Dispatcher;


   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  At_Time : Time.Time) is
   begin

      if The_Timer.The_Entry = null then

         The_Timer.The_Entry := new Timer_Event;
         The_Timer.The_Entry.On := Event_Queue_P (On);
         The_Timer.The_Entry.Time_To_Fire := At_Time;
         The_Timer.The_Entry.The_Event := To_Fire;
         The_Timer.The_Entry.The_Timer := The_Timer'Unrestricted_Access;
         On.The_Timer_Manager.Append (The_Timer.The_Entry);

      else

         --  XXX may not be all ...
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to set an already-set timer");

      end if;

   end Set;


   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration) is
   begin

      Set (The_Timer => The_Timer,
           On => On,
           To_Fire => To_Fire,
           At_Time => Time.From_Now (After));

   end Set;


   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue) is
      pragma Warnings (Off, On);
   begin

      if The_Timer.The_Entry = null then

         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to unset a timer that wasn't set");

      else

         --  Cancel the Event
         The_Timer.The_Entry.The_Event.Invalidated := True;

         --  Indicate the Timer's already unset
         The_Timer.The_Entry.The_Timer := null;

         --  Unset the Timer
         The_Timer.The_Entry := null;

      end if;

   end Unset;


   ------------------------------
   --  Timed event management  --
   ------------------------------

   task body Timer_Manager is

      The_Events : Timed_Event_Queues.Queue;

   begin

      loop

         if Timed_Event_Queues.Is_Empty (The_Events) then

            select
               accept Append (The_Entry : Timer_Queue_Entry_P) do
                  Timed_Event_Queues.Append (The_Events, The_Entry);
               end Append;

            or
               accept Invalidate
                 (For_The_Instance : Instance_Base_P);
               --  Invalidate is called "just in case" (the Instance
               --  being deleted can't tell whether there are any
               --  outstanding held Events). But if we get here, the
               --  queue was empty, so there can't be anything to do.

            end select;

         else

            select
               accept Append (The_Entry : Timer_Queue_Entry_P) do
                  Timed_Event_Queues.Append (The_Events, The_Entry);
               end Append;

            or
               accept Invalidate
                 (For_The_Instance : Instance_Base_P) do
                  declare
                     It : Abstract_Timed_Event_Containers.Iterator'Class
                       := Timed_Event_Queues.New_Iterator (The_Events);
                     use Abstract_Timed_Event_Containers;
                  begin
                     while not Is_Done (It) loop
                        if Current_Item (It).The_Event.all
                          in Instance_Event_Base'Class then
                           declare
                              E : Instance_Event_Base
                                renames Instance_Event_Base
                                (Current_Item (It).The_Event.all);
                           begin
                              if Instance_Base_P (E.For_The_Instance)
                                = For_The_Instance then
                                 E.Invalidated := True;
                              end if;
                           end;
                        end if;
                        Next (It);
                     end loop;
                  end;
               end Invalidate;

            or
               delay until Time.Equivalent
                 (Timed_Event_Queues.Front (The_Events).Time_To_Fire);
               declare
                  T : constant Timer_Queue_Entry_P
                    := Timed_Event_Queues.Front (The_Events);
               begin
                  Timed_Event_Queues.Pop (The_Events);
                  if not T.The_Event.Invalidated then
                     Post (Event_P (T), The_Queue);
                  else
                     Delete (T.The_Event);
                  end if;
               end;

            end select;

         end if;

      end loop;

   end Timer_Manager;


   function "<" (L, R : Timer_Queue_Entry_P) return Boolean is
      use type Ada.Real_Time.Time;
   begin
      return Time.Equivalent (L.Time_To_Fire)
        < Time.Equivalent (R.Time_To_Fire);
   end "<";


   protected body Excluder is

      procedure Post (The_Event : Event_P) is
      begin
         Unbounded_Posted_Event_Queues.Append (The_Queue.The_Events,
                                               The_Event);
      end Post;

      entry Fetch (The_Event : out Event_P)
      when not Unbounded_Posted_Event_Queues.Is_Empty (The_Queue.The_Events) is
      begin
         The_Event :=
           Unbounded_Posted_Event_Queues.Front (The_Queue.The_Events);
         Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Events);
      end Fetch;

      procedure Invalidate_Events
        (For_The_Instance : access Instance_Base'Class) is
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
                (The_Queue.The_Events);
         use Abstract_Posted_Event_Containers;
      begin
         while not Is_Done (It) loop
            if Current_Item (It).all in Instance_Event_Base'Class then
               declare
                  E : Instance_Event_Base
                    renames Instance_Event_Base (Current_Item (It).all);
               begin
                  if Instance_Base_P (E.For_The_Instance)
                    = Instance_Base_P (For_The_Instance) then
                     E.Invalidated := True;
                  end if;
               end;
            end if;
            Next (It);
         end loop;
      end Invalidate_Events;

   end Excluder;


   procedure Invalidate_Events
     (On : access Event_Queue;
      For_The_Instance : access Instance_Base'Class) is
   begin

      --  Called to mark all events for For_The_Instance as
      --  invalidated, so that they won't be dispatched.

      --  First, tell the Timer_Manager to mark all its held events
      --  that reference the departing instance;
      On.The_Timer_Manager.Invalidate (Instance_Base_P (For_The_Instance));
      --  after which there can't be any more left (even one that was
      --  in the process of being posted must have gone, because it's
      --  processed in another entry of the Timer_Manager task).

      --  Next, tell the Excluder the same.
      On.The_Excluder.Invalidate_Events (For_The_Instance);

   end Invalidate_Events;


end ColdFrame.Events_G.Standard_G;
