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
--  $Revision: 2616e0b373bd $
--  $Date: 2002/11/26 20:12:58 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Tags;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G.Standard_G is


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
            I : Events_G.Instance_Base
              renames Instance_Event_Base
              (Used_By_The_Instance_Of.all).For_The_Instance.all;
         begin
            if I.Events_Posted_On = null then
               I.Events_Posted_On := Event_Queue_P (The_Queue);
            elsif I.Events_Posted_On /= Event_Queue_P (The_Queue) then
               Ada.Exceptions.Raise_Exception
                 (Exceptions.Use_Error'Identity,
                  "attempt to post instance event on different queue");
            end if;
         end;

      end if;

   end Note;


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Add_Posted_Event (Event_Queue_P (On));

      On.The_Excluder.Post (The_Event);

   end Post;


   procedure Post_To_Self (The_Event : Event_P;
                           On : access Event_Queue_Base) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Add_Posted_Event (Event_Queue_P (On));

      On.The_Excluder.Post_To_Self (The_Event);

   end Post_To_Self;


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_At : Time.Time) is
      TE : Timer_Queue_Entry_P := new Timer_Event (On_Timer => False);
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Add_Held_Event (Event_Queue_P (On));

      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := To_Fire_At;
      TE.The_Event := The_Event;
      On.The_Timer_Manager.Append (TE);

   end Post;


   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_After : Natural_Duration) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      Post (The_Event => The_Event,
            On => On,
            To_Fire_At => Time.From_Now (To_Fire_After));

   end Post;


   task body Dispatcher is

      E : Event_P;
      Tearing_Down : Boolean;

   begin

      --  Wait to be told when to start, if not immediately
      if not The_Queue.Started then
         accept Start;
      end if;

      loop

         The_Queue.The_Excluder.Fetch (E, Tearing_Down);
         --  Blocks until there's an event or the queue needs
         --  tearing down.
         exit when  Tearing_Down;

         if not E.Invalidated then
            begin
               Log_Pre_Dispatch (The_Event => E, On => The_Queue);
               Handler (E.all);
               Log_Post_Dispatch (The_Event => E, On => The_Queue);
            exception
               when Ex : others =>
                  Logging.Log
                    (Severity => Logging.Error,
                     Message =>
                       Ada.Exceptions.Exception_Information (Ex) &
                       " in Dispatcher (event " &
                       Ada.Tags.Expanded_Name (E.all'Tag) &
                       ")");
            end;
         end if;

         Delete (E);
         Remove_Posted_Event (The_Queue);
         The_Queue.The_Excluder.Done;

      end loop;

   end Dispatcher;


   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  At_Time : Time.Time) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => To_Fire);

      if The_Timer.The_Entry = null then

         --  We need a dispatching call, in case the queue is actually
         --  a derived type.
         Add_Timer_Event (Event_Queue_P (On));

         The_Timer.The_Entry := new Timer_Event (On_Timer => True);
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
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  After : Natural_Duration) is
   begin

      Set (The_Timer => The_Timer,
           On => On,
           To_Fire => To_Fire,
           At_Time => Time.From_Now (After));

   end Set;


   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue_Base) is
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

      The_Events : Timed_Event_Queues.Queue renames The_Queue.The_Timed_Events;

      procedure Process_First_Event;
      pragma Inline (Process_First_Event);
      procedure Process_First_Event is
         T : constant Timer_Queue_Entry_P
           := Timed_Event_Queues.Front (The_Events);
         Held : constant Boolean := not T.On_Timer;
      begin
         Timed_Event_Queues.Pop (The_Events);
         Post (Event_P (T), The_Queue);
         if Held then
            Remove_Held_Event (The_Queue);
         else
            Remove_Timer_Event (The_Queue);
         end if;
      end Process_First_Event;

      use type Ada.Real_Time.Time;

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

            or
               accept Tear_Down;
               exit;

            end select;

         elsif Time.Equivalent
           (Timed_Event_Queues.Front (The_Events).Time_To_Fire)
           <= Ada.Real_Time.Clock then

            loop
               Process_First_Event;
               exit when Timed_Event_Queues.Is_Empty (The_Events);
               exit when Time.Equivalent
                 (Timed_Event_Queues.Front (The_Events).Time_To_Fire)
                 > Ada.Real_Time.Clock;
            end loop;

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
               accept Tear_Down;
               exit;

            or
               delay until Time.Equivalent
                 (Timed_Event_Queues.Front (The_Events).Time_To_Fire);
               Process_First_Event;

            end select;

         end if;

      end loop;

   exception
      when E : others =>
         Logging.Log
           (Severity => Logging.Error,
            Message =>
              Ada.Exceptions.Exception_Information (E) &
              " in Timer_Manager");
   end Timer_Manager;


   function "<" (L, R : Timer_Queue_Entry_P) return Boolean is
      use type Ada.Real_Time.Time;
   begin
      return Time.Equivalent (L.Time_To_Fire)
        < Time.Equivalent (R.Time_To_Fire);
   end "<";


   protected body Excluder is


      procedure Done is
         use type Ada.Task_Identification.Task_Id;
      begin
         if Unbounded_Posted_Event_Queues.Is_Empty
           (The_Queue.The_Self_Events) then
            Locks := 0;
            Owner := Ada.Task_Identification.Null_Task_Id;
         end if;
      end Done;


      entry Fetch (The_Event : out Event_P; Tearing_Down : out Boolean)
      --  If there are any events-to-self, we must have a lock already
      --  (the previous call to Done didn't release the lock, because
      --  we don't want external entities to see the intermediate
      --  states).
      when Excluder.Tearing_Down
        or else not Unbounded_Posted_Event_Queues.Is_Empty
                      (The_Queue.The_Self_Events)
        or else (Locks = 0
                   and then not Unbounded_Posted_Event_Queues.Is_Empty
                                  (The_Queue.The_Events)) is
      begin
         Tearing_Down := Excluder.Tearing_Down;
         if Tearing_Down then
            return;
         end if;
         Locks := 1;
         Owner := Fetch'Caller;
         if not Unbounded_Posted_Event_Queues.Is_Empty
                  (The_Queue.The_Self_Events) then
            The_Event :=
              Unbounded_Posted_Event_Queues.Front (The_Queue.The_Self_Events);
            Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Self_Events);
         else
            The_Event :=
              Unbounded_Posted_Event_Queues.Front (The_Queue.The_Events);
            Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Events);
         end if;
         pragma Assert (The_Event /= null,
                        "failed to fetch valid event");
      end Fetch;


      procedure Invalidate_Events
        (For_The_Instance : access Instance_Base'Class) is

         procedure Invalidate_Events
           (Using : in out Abstract_Posted_Event_Containers.Iterator'Class);
         procedure Invalidate_Events
           (Using : in out Abstract_Posted_Event_Containers.Iterator'Class) is
            use Abstract_Posted_Event_Containers;
         begin
            while not Is_Done (Using) loop
               if Current_Item (Using).all in Instance_Event_Base'Class then
                  declare
                     E : Instance_Event_Base
                       renames Instance_Event_Base (Current_Item (Using).all);
                  begin
                     if Instance_Base_P (E.For_The_Instance)
                       = Instance_Base_P (For_The_Instance) then
                        E.Invalidated := True;
                     end if;
                  end;
               end if;
               Next (Using);
            end loop;
         end Invalidate_Events;

         Self_Iterator : Abstract_Posted_Event_Containers.Iterator'Class :=
           Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Self_Events);
         Normal_Iterator : Abstract_Posted_Event_Containers.Iterator'Class :=
           Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Events);

      begin
         Invalidate_Events (Self_Iterator);
         Invalidate_Events (Normal_Iterator);
      end Invalidate_Events;


      entry Lock when True is
         use type Ada.Task_Identification.Task_Id;
      begin
         if Lock'Caller = Owner then
            Locks := Locks + 1;
         else
            requeue Waiting_For_Lock with abort;
         end if;
      end Lock;


      procedure Post (The_Event : Event_P) is
      begin
         Unbounded_Posted_Event_Queues.Append (The_Queue.The_Events,
                                               The_Event);
      end Post;


      procedure Post_To_Self (The_Event : Event_P) is
         use type Ada.Task_Identification.Task_Id;
      begin
         --  We need to be sure that only event handlers called by our
         --  Dispatcher post events-to-self.
         if Owner /= Standard_G.Event_Queue_Base (The_Queue.all).
                        The_Dispatcher'Identity then
            Ada.Exceptions.Raise_Exception
              (Exceptions.Use_Error'Identity,
               "posting to self outside event handler");
         else
            Unbounded_Posted_Event_Queues.Append (The_Queue.The_Self_Events,
                                                  The_Event);
         end if;
      end Post_To_Self;


      procedure Tear_Down is
      begin
         Tearing_Down := True;
      end Tear_Down;


      procedure Unlock is
      begin
         Locks := Locks - 1;
         if Locks = 0 then
            Owner := Ada.Task_Identification.Null_Task_Id;
         end if;
      end Unlock;


      entry Waiting_For_Lock when Locks = 0 is
      begin
         Locks := 1;
         Owner := Waiting_For_Lock'Caller;
      end Waiting_For_Lock;


   end Excluder;


   procedure Start_Queue (The_Queue : access Event_Queue_Base) is
   begin
      --  This will block indefinitely if called for a started Queue.
      --  But, the only way it's supposed to be called is via Start,
      --  which (at this level) doesn't in fact do so.
      The_Queue.The_Dispatcher.Start;
   end Start_Queue;


   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class) is
   begin

      --  Called to mark all events for For_The_Instance as
      --  invalidated, so that they won't be dispatched.

      --  First, tell the Timer_Manager to mark all its held events
      --  that reference the departing instance;
      On.The_Timer_Manager.Invalidate (Instance_Base_P (For_The_Instance));
      --  after which there can't be any more left (even one that was
      --  in the process of being posted must have gone, because it's
      --  processed elsewhere in the Timer_Manager task).

      --  Next, tell the Excluder the same.
      On.The_Excluder.Invalidate_Events (For_The_Instance);

   end Invalidate_Events;


   procedure Tear_Down (The_Queue : in out Event_Queue_Base) is
      use Abstract_Posted_Event_Containers;
      use Abstract_Timed_Event_Containers;
   begin

      --  Perhaps this could be neater, but at least doing it in this
      --  order we know that the Timer Manager can't post any more
      --  events on a dead Dispatcher.
      The_Queue.The_Timer_Manager.Tear_Down;
      while not The_Queue.The_Timer_Manager'Terminated loop
         delay 0.1;
      end loop;
      The_Queue.The_Excluder.Tear_Down;
      while not The_Queue.The_Dispatcher'Terminated loop
         delay 0.1;
      end loop;

      declare
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Self_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Current_Item (It);
            begin
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

      declare
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Current_Item (It);
            begin
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

      declare
         It : Abstract_Timed_Event_Containers.Iterator'Class
           := Timed_Event_Queues.New_Iterator
           (The_Queue.The_Timed_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Event_P (Current_Item (It));
            begin
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

   end Tear_Down;


   procedure Locker (The_Queue : access Event_Queue_Base) is
   begin
      The_Queue.The_Excluder.Lock;
   end Locker;


   procedure Unlocker (The_Queue : access Event_Queue_Base) is
   begin
      The_Queue.The_Excluder.Unlock;
   end Unlocker;


end ColdFrame.Events_G.Standard_G;
