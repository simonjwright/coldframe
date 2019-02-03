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

pragma Profile (Ravenscar);  -- see ticket #11, I think

with Ada.Containers;

private with Ada.Containers.Bounded_Vectors;
private with Ada.Task_Identification;

package ColdFrame.Events.Standard is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base (Priority             : System.Priority;
                          Storage_Size         : Positive;
                          Secondary_Stack_Size : Natural;
                          Capacity             : Ada.Containers.Count_Type)
     is new Events.Event_Queue_Base with private;

   Default_Storage_Size : constant := 2048;
   --  The default secondary stack size is 10% of the storage size

   subtype Event_Queue is Event_Queue_Base
     (Priority             => System.Default_Priority,
      Storage_Size         => Default_Storage_Size,
      Secondary_Stack_Size => Default_Storage_Size / 10,
      Capacity             => 64);

   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base);

   procedure Post_To_Self (The_Event : not null Event_P;
                           On : not null access Event_Queue_Base);

   ----------------------
   --  Delayed events  --
   ----------------------

   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_At : Ada.Real_Time.Time);

   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_After : Natural_Duration);

   --------------
   --  Timers  --
   --------------

   procedure Set (The_Timer : in out          Timer;
                  On        : not null access Event_Queue_Base;
                  To_Fire   :                 not null Event_P;
                  At_Time   :                 Ada.Real_Time.Time);

   procedure Set (The_Timer : in out          Timer;
                  On        : not null access Event_Queue_Base;
                  To_Fire   :                 not null Event_P;
                  After     :                 Natural_Duration);

   procedure Unset (The_Timer : in out          Timer;
                    On        : not null access Event_Queue_Base);

private

   --  While one would like to make the Vectors' Element_Type involve
   --  'not null Event_P', we can't do that for *Bounded* vectors
   --  since the unused elements still exist.

   package Event_Queues is new Ada.Containers.Bounded_Vectors
     (Index_Type   => Positive,
      Element_Type => Event_P);

   package Event_Management is
      --  Made a package to reduce the complexity of the main body.

      --  Used before the queue is started; we reckon the delay from
      --  the time when the queue is started.
      type Duration_Cell is record
         Delay_To_Fire : Duration;
         Event         : Event_P;
      end record;

      package Duration_Vectors is new Ada.Containers.Bounded_Vectors
        (Index_Type   => Positive,
         Element_Type => Duration_Cell);

      --  Used afterwards (even for plain 'delay's).
      type Time_Cell is record
         Time_To_Fire : Ada.Real_Time.Time;
         Event        : Event_P;
      end record;

      --  This is maintained sorted by increasing time to fire.
      package Time_Vectors is new Ada.Containers.Bounded_Vectors
        (Index_Type   => Positive,
         Element_Type => Time_Cell);

      --  Holds both immediately-actionable and held events.
      protected type All_Events
        (The_Queue : not null access Event_Queue_Base'Class;
         Capacity  : Ada.Containers.Count_Type) is

         --  We need to constrain by 'Class so that internal calls to
         --  potentially dispatching operations (such as
         --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

         --  Immediately-actionable events:
         ----------------------------------

         procedure Post (The_Event : not null Event_P);
         --  Post an event.

         procedure Post_To_Self (The_Event : not null Event_P);
         --  Post an event-to-self.

         entry Fetch (The_Event : out Event_P);
         --  Blocks until the queue is unlocked and there is an event on
         --  it; when one is found, notes that execution is in progress,
         --  removes the event from the queue and stores it
         --  in "The_Event".

         procedure Invalidate_Events
           (For_The_Instance : not null access Instance_Base'Class);
         --  Marks all the events on the queue which are for
         --  For_The_Instance as invalid, so they won't be actioned when
         --  Fetched.

         procedure Done;
         --  Notes that execution is no longer in progress.

         --  Held events:
         ----------------

         procedure Running;
         --  The event queue has started; post all the events on the
         --  duration queue onto the held event queue (since we now
         --  know when the duration is from).

         procedure Add_At_Event (The_Entry : not null Event_P;
                                 To_Run_At : Ada.Real_Time.Time);

         procedure Add_After_Event (The_Entry    : not null Event_P;
                                    To_Run_After : Duration);

         procedure Remove_Held_Event (An_Event : not null Event_P);
         --  Called when a Timer is Unset. Removes An_Event (a
         --  Held_Event) from the queue.

         procedure Make_Events_Actionable
           (If_At_Or_Before : Ada.Real_Time.Time);
         --  Called every tick to transfer held events whose time has
         --  arrived to the immediately-actionable queue.

      private

         Fetchable_Event : Boolean := False;

         procedure Check_Fetchable_Event;
         --  Compute the new value of Fetchable_Event, so as to work
         --  round the Ravenscar requirement for simple entry
         --  conditions.

         Owner : Ada.Task_Identification.Task_Id :=
           Ada.Task_Identification.Null_Task_Id;
         --  Supports "recursive mutex" behaviour, and checks that
         --  Post_To_Self is called during event processing, not by some
         --  external task. XXX how to do this? is it needed?

         The_Self_Events     : access Event_Queues.Vector :=
           new Event_Queues.Vector (Capacity => Capacity);
         The_Instance_Events : access Event_Queues.Vector :=
           new Event_Queues.Vector (Capacity => Capacity);
         The_Class_Events    : access Event_Queues.Vector :=
           new Event_Queues.Vector (Capacity => Capacity);

         Started : Boolean := False;
         --  Set True by Running (called by Ticker; at which point
         --  tasking must have started).

         The_Duration_Events   : access Duration_Vectors.Vector :=
           new Duration_Vectors.Vector (Capacity => Capacity);
         The_Held_Events : access Time_Vectors.Vector :=
           new Time_Vectors.Vector (Capacity => Capacity);

      end All_Events;

      task type Ticker (The_Queue : access Event_Queue_Base'Class) is
      end Ticker;

   end Event_Management;
   use Event_Management;


   task type Dispatcher (The_Queue            : access Event_Queue_Base'Class;
                         Priority             : System.Priority;
                         Storage_Size         : Positive;
                         Secondary_Stack_Size : Natural) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      pragma Priority (Priority);
      pragma Storage_Size (Storage_Size);
      pragma Secondary_Stack_Size (Secondary_Stack_Size);

   end Dispatcher;


   type Event_Queue_Base (Priority             : System.Priority;
                          Storage_Size         : Positive;
                          Secondary_Stack_Size : Natural;
                          Capacity             : Ada.Containers.Count_Type)
     is new Events.Event_Queue_Base
       (Start_Started        => True,
        Priority             => Priority,
        Storage_Size         => Storage_Size,
        Secondary_Stack_Size => Secondary_Stack_Size)
      with record

         The_Events : All_Events (Event_Queue_Base'Access,
                                  Capacity => Capacity);

         The_Ticker : Ticker (Event_Queue_Base'Access);

         The_Dispatcher : Dispatcher
           (Event_Queue_Base'Access,
            Priority             => Priority,
            Storage_Size         => Storage_Size,
            Secondary_Stack_Size => Secondary_Stack_Size);
      end record;

   procedure Invalidate_Events
     (On               : not null access Event_Queue_Base;
      For_The_Instance : not null access Instance_Base'Class);
   --  Called to mark all events for For_The_Instance as
   --  invalidated, so that they won't be dispatched.

end ColdFrame.Events.Standard;
