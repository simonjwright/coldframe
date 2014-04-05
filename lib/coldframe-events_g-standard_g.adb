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
--  $Revision: fde6fd75a1a0 $
--  $Date: 2014/04/05 13:21:13 $
--  $Author: simonjwright $

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Tags;
with ColdFrame.Exceptions;
with ColdFrame.Project.Log_Error;

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
            I : Events_G.Instance_Base'Class
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


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base) is
   begin

      Log (The_Event, Event_Basis.Posting);

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Note_Addition_Of_Posted_Event (Event_Queue_P (On));

      On.The_Excluder.Post (The_Event);

   end Post;


   procedure Post_To_Self (The_Event : not null Event_P;
                           On : not null access Event_Queue_Base) is
   begin

      Log (The_Event, Event_Basis.Posting);

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Note_Addition_Of_Posted_Event (Event_Queue_P (On));

      On.The_Excluder.Post_To_Self (The_Event);

   end Post_To_Self;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_At : Time.Time) is
      TEP : constant Event_P := new Held_Event (Kind => To_Fire_At.Kind,
                                                On_Timer => False);
      TE : Held_Event renames Held_Event (TEP.all);
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Note_Addition_Of_Held_Event (Event_Queue_P (On));

      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := To_Fire_At;
      TE.The_Event := The_Event;
      On.The_Held_Event_Manager.Add_At_Event (TEP, To_Fire_At);

   end Post;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_After : Natural_Duration) is
      TEP : constant Event_P := new Held_Event (Kind => Time.Real_Time,
                                                On_Timer => False);
      TE : Held_Event renames Held_Event (TEP.all);
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => The_Event);

      --  We need a dispatching call, in case the queue is actually a
      --  derived type.
      Note_Addition_Of_Held_Event (Event_Queue_P (On));

      TE.On := Event_Queue_P (On);
      TE.Time_To_Fire := Time.From_Now (To_Fire_After);
      --  NB, this will be wrong if the queue isn't yet started.
      TE.The_Event := The_Event;
      On.The_Held_Event_Manager.Add_After_Event (TEP, To_Fire_After);

   end Post;


   task body Dispatcher is

      E : Event_P;
      Tearing_Down : Boolean := False;

   begin

      --  Wait to be told when to start, if not immediately. Note, we
      --  have to check Start_Started (the intention) rather than
      --  Started (the current state), in case of a race condition
      --  with the Start operation.
      if not The_Queue.Start_Started then

         select
            accept Start do
               --  Start processing events set or posted to run after
               --  a delay or at a time only after we have started
               --  ourselves; but don't return until we've done so, so
               --  that our caller can tell the Held_Event_Manager to
               --  rethink *after* we're ready
               Held_Events.Start_Processing_Events (The_Queue.The_Held_Events);
            end Start;

         or
            accept Finish;
            Tearing_Down := True;

         end select;

      else

         --  Start processing events set or posted to run after a
         --  delay or at a time immediately
         Held_Events.Start_Processing_Events (The_Queue.The_Held_Events);

      end if;

      if not Tearing_Down then

         loop

            The_Queue.The_Excluder.Fetch (E, Tearing_Down);
            --  Blocks until there's an event or the queue needs
            --  tearing down.
            exit when Tearing_Down;

            if not E.Invalidated then
               Log (E, Event_Basis.Dispatching);
               Log_Pre_Dispatch (The_Event => E, On => The_Queue);
               Start_Handling (E);
               begin
                  Handler (E.all);
               exception
                  when Ex : Exceptions.Cant_Happen =>
                     Project.Log_Error
                       ("Illegal event "
                          & Ada.Exceptions.Exception_Message (Ex));
                  when Ex : others =>
                     Project.Log_Error
                       (Ada.Exceptions.Exception_Information (Ex) &
                          " in Dispatcher (event " &
                          Ada.Tags.Expanded_Name (E.all'Tag) &
                          ")");
               end;
               Stop_Handling (E);
               Log_Post_Dispatch (The_Event => E, On => The_Queue);
               Log (E, Event_Basis.Finishing);
            end if;

            Delete (E);
            Note_Removal_Of_Posted_Event (The_Queue);
            The_Queue.The_Excluder.Done;

         end loop;

      end if;

   end Dispatcher;


   procedure Set (The_Timer : in out Timer;
                  On : not null access Event_Queue_Base;
                  To_Fire : not null Event_P;
                  At_Time : Time.Time) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => To_Fire);

      if The_Timer.The_Entry = null then

         --  We need a dispatching call, in case the queue is actually
         --  a derived type.
         Note_Addition_Of_Timer_Event (Event_Queue_P (On));

         The_Timer.The_Entry := new Held_Event (Kind => At_Time.Kind,
                                                On_Timer => True);
         declare
            TE : Held_Event renames Held_Event (The_Timer.The_Entry.all);
         begin
            TE.On := Event_Queue_P (On);
            TE.Time_To_Fire := At_Time;
            TE.The_Event := To_Fire;
            TE.The_Timer := The_Timer'Unrestricted_Access;
         end;
         On.The_Held_Event_Manager.Add_At_Event (The_Timer.The_Entry, At_Time);

      else

         --  XXX may not be all ...
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to set an already-set timer");

      end if;

   end Set;


   procedure Set (The_Timer : in out Timer;
                  On : not null access Event_Queue_Base;
                  To_Fire : not null Event_P;
                  After : Natural_Duration) is
   begin

      Note (The_Queue => On,
            Used_By_The_Instance_Of => To_Fire);

      if The_Timer.The_Entry = null then

         --  We need a dispatching call, in case the queue is actually
         --  a derived type.
         Note_Addition_Of_Timer_Event (Event_Queue_P (On));

         The_Timer.The_Entry := new Held_Event (Kind => Time.Real_Time,
                                                On_Timer => True);
         declare
            TE : Held_Event renames Held_Event (The_Timer.The_Entry.all);
         begin
            TE.On := Event_Queue_P (On);
            TE.Time_To_Fire := Time.From_Now (After);
            --  NB, this will be wrong if the queue isn't yet started.
            TE.The_Event := To_Fire;
            TE.The_Timer := The_Timer'Unrestricted_Access;
         end;
         On.The_Held_Event_Manager.Add_After_Event (The_Timer.The_Entry,
                                                    After);

      else

         --  XXX may not be all ...
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to set an already-set timer");

      end if;

   end Set;


   procedure Unset (The_Timer : in out Timer;
                    On : not null access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin

      if The_Timer.The_Entry = null then

         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to unset a timer that wasn't set");

      else

         declare
            TE : Held_Event renames Held_Event (The_Timer.The_Entry.all);
         begin

            --  Cancel the Event
            TE.The_Event.Invalidated := True;

            --  Indicate the Timer's already unset
            TE.The_Timer := null;

         end;

         --  Unset the Timer
         The_Timer.The_Entry := null;

      end if;

   end Unset;


   ------------------------------
   --  Timed event management  --
   ------------------------------

   task body Held_Event_Manager is

      The_Events : Held_Events.Queue renames The_Queue.The_Held_Events;

      procedure Process_First_Event;
      pragma Inline (Process_First_Event);
      procedure Process_First_Event is
         E : Event_P;
         Held : Boolean;
      begin
         Held_Events.Pop (The_Events, E);
         Held := not Held_Event (E.all).On_Timer;
         Post (E, The_Queue);
         if Held then
            Note_Removal_Of_Held_Event (The_Queue);
         else
            Note_Removal_Of_Timer_Event (The_Queue);
         end if;
      end Process_First_Event;

      use type Ada.Real_Time.Time;

   begin

      Outer : loop

         --  This loop encloses an exception-catching block, so that
         --  the whole event queue doesn't just die. Of course, the
         --  system will be fragile after any uncaught exception here
         --  (there is no user-written code here, but they might
         --  corrupt timers).

         begin

            Inner : loop

               --  This is the main loop.

               if Held_Events.Is_Empty (The_Events) then

                  select
                     accept Add_At_Event (The_Entry : Event_P;
                                          To_Run_At : Time.Time) do
                        Held_Events.Add_At_Event (The_Entry,
                                                  To_Run_At,
                                                  On => The_Events);
                     end Add_At_Event;

                  or
                     accept Add_After_Event (The_Entry : Event_P;
                                             To_Run_After : Duration) do
                        Held_Events.Add_After_Event (The_Entry,
                                                     To_Run_After,
                                                     On => The_Events);
                     end Add_After_Event;

                  or
                     accept Rethink;

                  or
                     accept Invalidate
                       (For_The_Instance : Instance_Base_P) do
                        pragma Unreferenced (For_The_Instance);
                        null;
                     end Invalidate;
                     --  Invalidate is called "just in case" (the Instance
                     --  being deleted can't tell whether there are any
                     --  outstanding held Events). But if we get here, the
                     --  queue was empty, so there can't be anything to do.

                  or
                     accept Stop;
                     exit Outer;

                  end select;

               elsif Held_Events.Next_Event_Time (The_Events)
                 <= Ada.Real_Time.Clock then

                  loop
                     Process_First_Event;
                     exit when Held_Events.Is_Empty (The_Events);
                     exit when Held_Events.Next_Event_Time (The_Events)
                       > Ada.Real_Time.Clock;
                  end loop;

               else

                  select
                     accept Add_At_Event (The_Entry : Event_P;
                                          To_Run_At : Time.Time) do
                        Held_Events.Add_At_Event (The_Entry,
                                                  To_Run_At,
                                                  On => The_Events);
                     end Add_At_Event;

                  or
                     accept Add_After_Event (The_Entry : Event_P;
                                             To_Run_After : Duration) do
                        Held_Events.Add_After_Event (The_Entry,
                                                     To_Run_After,
                                                     On => The_Events);
                     end Add_After_Event;

                  or
                     accept Rethink;

                  or
                     accept Invalidate
                       (For_The_Instance : Instance_Base_P) do
                        Held_Events.Invalidate_Events
                          (The_Events, For_The_Instance);
                     end Invalidate;

                  or
                     accept Stop;
                     exit Outer;

                  or
                     delay until Held_Events.Next_Event_Time (The_Events);
                     Process_First_Event;

                  end select;

               end if;

            end loop Inner;

         exception
            when E : others =>
               Project.Log_Error
                 (Ada.Exceptions.Exception_Information (E) &
                    " in Held_Event_Manager");

         end;

         delay 0.1;
         --  In case we end up in a tight, unstoppable loop. After
         --  all, we should never get here at all!

      end loop Outer;

      accept Stopped;
      --  This makes our readiness for being torn down visible without
      --  actually terminating; if we terminate, user tasks may fail
      --  when trying to post held events.

      --  Wait to be told to quit ... there may be tasks (eg, in other
      --  event queues) still alive, and we don't want them to get
      --  Tasking_Error.
      Final : loop
         select
            --  This is the call we're really interested in.
            accept Finish;
            exit Final;
         or
            --  However, we have to be prepared for others, because
            --  the Dispatcher may still be processing an event which
            --  is entitled to make any of the remaining calls; though
            --  of course there is nothing to do.
            accept Add_At_Event (The_Entry : Event_P;
                                 To_Run_At : Time.Time) do
               pragma Unreferenced (The_Entry);
               pragma Unreferenced (To_Run_At);
               null;
            end Add_At_Event;
         or
            accept Add_After_Event (The_Entry : Event_P;
                                    To_Run_After : Duration) do
               pragma Unreferenced (The_Entry);
               pragma Unreferenced (To_Run_After);
               null;
            end Add_After_Event;
         or
            accept Rethink;
         or
            accept Invalidate
              (For_The_Instance : Instance_Base_P) do
               pragma Unreferenced (For_The_Instance);
               null;
            end Invalidate;
         end select;
      end loop Final;

   end Held_Event_Manager;


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


      entry Fetch (The_Event : out Event_P; Stopping : out Boolean)
      --  If there are any events-to-self, we must have a lock already
      --  (the previous call to Done didn't release the lock, because
      --  we don't want external entities to see the intermediate
      --  states).
      when Excluder.Stopping
        or else not Unbounded_Posted_Event_Queues.Is_Empty
                      (The_Queue.The_Self_Events)
        or else (Locks = 0 and then not
                   (Unbounded_Posted_Event_Queues.Is_Empty
                      (The_Queue.The_Instance_Events)
                    and then Unbounded_Posted_Event_Queues.Is_Empty
                      (The_Queue.The_Class_Events))) is
      begin
         Stopping := Excluder.Stopping;
         if Stopping then
            return;
         end if;
         Locks := 1;
         Owner := Fetch'Caller;
         if not Unbounded_Posted_Event_Queues.Is_Empty
                  (The_Queue.The_Self_Events) then
            The_Event :=
              Unbounded_Posted_Event_Queues.Front (The_Queue.The_Self_Events);
            Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Self_Events);
         elsif not Unbounded_Posted_Event_Queues.Is_Empty
           (The_Queue.The_Instance_Events) then
            The_Event :=
              Unbounded_Posted_Event_Queues.Front
              (The_Queue.The_Instance_Events);
            Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Instance_Events);
         else
            The_Event :=
              Unbounded_Posted_Event_Queues.Front (The_Queue.The_Class_Events);
            Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Class_Events);
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
               Invalidate (Current_Item (Using),
                           If_For_Instance =>
                             Instance_Base_P (For_The_Instance));
               Next (Using);
            end loop;
         end Invalidate_Events;

         Self_Iterator : Abstract_Posted_Event_Containers.Iterator'Class :=
           Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Self_Events);
         Instance_Iterator : Abstract_Posted_Event_Containers.Iterator'Class :=
           Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Instance_Events);

      begin
         Invalidate_Events (Self_Iterator);
         Invalidate_Events (Instance_Iterator);
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


      entry Post (The_Event : Event_P) when not Stopping is
      begin
         if The_Event.all in Instance_Event_Base'Class then
            Unbounded_Posted_Event_Queues.Append
              (The_Queue.The_Instance_Events, The_Event);
         else
            Unbounded_Posted_Event_Queues.Append
              (The_Queue.The_Class_Events, The_Event);
         end if;
      end Post;


      entry Post_To_Self (The_Event : Event_P)  when not Stopping is
         use type Ada.Task_Identification.Task_Id;
      begin
         --  We need to be sure that only event handlers called by our
         --  Dispatcher post events-to-self.
         if Owner /=
           Standard_G.Event_Queue_Base (The_Queue.all).The_Dispatcher'Identity
         then
            Ada.Exceptions.Raise_Exception
              (Exceptions.Use_Error'Identity,
               "posting to self outside event handler");
         elsif not (The_Event.all in Instance_Event_Base'Class) then
            Ada.Exceptions.Raise_Exception
              (Exceptions.Use_Error'Identity,
               "posting class event to self");
         else
            Unbounded_Posted_Event_Queues.Append (The_Queue.The_Self_Events,
                                                  The_Event);
         end if;
      end Post_To_Self;


      procedure Stop is
      begin
         Stopping := True;
      end Stop;


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


   procedure Start_Queue (The_Queue : not null access Event_Queue_Base) is
   begin
      --  This will block indefinitely if called for a started Queue.
      --  But, the only way it's supposed to be called is via Start.
      --  The base implementation of Start checks first and doesn't
      --  make the call if it would be wrong to do so.
      The_Queue.The_Dispatcher.Start;
      The_Queue.The_Held_Event_Manager.Rethink;
   end Start_Queue;


   procedure Invalidate_Events
     (On : not null access Event_Queue_Base;
      For_The_Instance : not null access Instance_Base'Class) is
   begin

      --  Called to mark all events for For_The_Instance as
      --  invalidated, so that they won't be dispatched.

      --  First, tell the Held_Event_Manager to mark all its held events
      --  that reference the departing instance;
      On.The_Held_Event_Manager.Invalidate
        (Instance_Base_P (For_The_Instance));
      --  after which there can't be any more left (even one that was
      --  in the process of being posted must have gone, because it's
      --  processed elsewhere in the Held_Event_Manager task).

      --  Next, tell the Excluder the same.
      On.The_Excluder.Invalidate_Events (For_The_Instance);

   end Invalidate_Events;


   procedure Stop (The_Queue : in out Event_Queue_Base) is
      use Abstract_Posted_Event_Containers;
   begin

      --  Perhaps this could be neater, but at least doing it in this
      --  order we know that the Timer Manager can't post any more
      --  events on a dead Dispatcher.

      --  Stop processing held events ..
      The_Queue.The_Held_Event_Manager.Stop;
      --  .. waiting until the Held_Event_Manager has actually stopped
      --  (note, it's still alive).
      The_Queue.The_Held_Event_Manager.Stopped;

      --  Tell the Excluder we're finishing. This makes the next Fetch
      --  report that tear-down is in progress.
      The_Queue.The_Excluder.Stop;

      --  If the queue wasn't started, tell the Dispatcher we're
      --  finishing.
      if not The_Queue.Started then
         The_Queue.The_Dispatcher.Finish;
      end if;

      --  Wait until the Dispatcher has actually stopped.
      while not The_Queue.The_Dispatcher'Terminated loop
         delay 0.1;
      end loop;

      --  Delete all the outstanding events-to-self ..
      declare
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Self_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Current_Item (It);
            begin
               Tear_Down (E);
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

      --  .. and all the outstanding instance events ..
      declare
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Instance_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Current_Item (It);
            begin
               Tear_Down (E);
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

      --  .. and all the outstanding class events ..
      declare
         It : Abstract_Posted_Event_Containers.Iterator'Class
           := Unbounded_Posted_Event_Queues.New_Iterator
           (The_Queue.The_Class_Events);
      begin
         while not Is_Done (It) loop
            declare
               E : Event_P := Current_Item (It);
            begin
               Tear_Down (E);
               Delete (E);
            end;
            Next (It);
         end loop;
      end;

      --  .. and all the held events.
      Held_Events.Tear_Down (The_Queue.The_Held_Events);

   end Stop;


   procedure Tear_Down (The_Queue : in out Event_Queue_Base) is
   begin

      --  Tell the Held Event Manager to quit.
      The_Queue.The_Held_Event_Manager.Finish;

      --  Wait until the Held Event Manager has actually stopped.
      while not The_Queue.The_Held_Event_Manager'Terminated loop
         delay 0.1;
      end loop;

   end Tear_Down;


   procedure Locker (The_Queue : not null access Event_Queue_Base) is
   begin
      The_Queue.The_Excluder.Lock;
   end Locker;


   procedure Unlocker (The_Queue : not null access Event_Queue_Base) is
   begin
      The_Queue.The_Excluder.Unlock;
   end Unlocker;


end ColdFrame.Events_G.Standard_G;
