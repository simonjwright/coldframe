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

pragma Profile (Ravenscar);

with Ada.Containers;

private with Ada.Containers.Bounded_Vectors;
private with Ada.Task_Identification;

package ColdFrame.Events.Standard is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base (Priority     : System.Priority;
                          Storage_Size : Positive;
                          Capacity     : Ada.Containers.Count_Type)
   is new Events.Event_Queue_Base with private;

   subtype Event_Queue is Event_Queue_Base
     (Priority     => System.Default_Priority,
      Storage_Size => 2048,
      Capacity     => 64);

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

   Test : Event_Queues.Vector (Capacity => 64);

   package Dispatchable_Event_Management is
      --  Made a package to reduce the complexity of the main body.

      --  Mutual exclusion between posters and the Dispatcher.
      protected type Dispatchable_Events
        (The_Queue : not null access Event_Queue_Base'Class;
         Capacity  : Ada.Containers.Count_Type) is

         --  We need to constrain by 'Class so that internal calls to
         --  potentially dispatching operations (such as
         --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

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

      private

         Fetchable_Event : Boolean := False;

         procedure Check_Fetchable_Event;

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

      end Dispatchable_Events;

   end Dispatchable_Event_Management;
   use Dispatchable_Event_Management;


   package Held_Event_Management is
      --  Made a package to reduce the complexity of the main body.

      --  Used before the queue is started; we reckon the delay from
      --  the time when the queue is started.
      type Duration_Cell is record
         Delay_To_Fire : Duration;
         Event         : Event_P;
      end record;

      pragma Warnings (Off, "*No_Exception_Propagation*");
      package Duration_Vectors is new Ada.Containers.Bounded_Vectors
        (Index_Type   => Positive,
         Element_Type => Duration_Cell);
      pragma Warnings (On, "*No_Exception_Propagation*");

      --  Used afterwards (even for plain 'delay's).
      type Time_Cell is record
         Time_To_Fire : Ada.Real_Time.Time;
         Event        : Event_P;
      end record;

      --  This is maintained sorted by increasing time to fire.
      pragma Warnings (Off, "*No_Exception_Propagation*");
      package Time_Vectors is new Ada.Containers.Bounded_Vectors
        (Index_Type   => Positive,
         Element_Type => Time_Cell);
      pragma Warnings (On, "*No_Exception_Propagation*");

      protected type Held_Events
        (The_Queue : not null access Event_Queue_Base'Class;
         Capacity  : Ada.Containers.Count_Type) is

         procedure Running;
         --  The event queue has started; post all the events on the
         --  duration queue onto the held event queue (since we now
         --  know when the duration is from).

         --  We need to constrain by 'Class so that internal calls to
         --  potentially dispatching operations (such as
         --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

         --  No need to specify priority (because we only deal with
         --  timed events anyway) or stack size (if anything, we could
         --  reduce it, since there's no user code to call).

         procedure Fetch (An_Event        : out Event_P;
                          If_At_Or_Before :     Ada.Real_Time.Time);
         --  Returns an event which has become due (null if
         --  none). Running must have been called.

         procedure Add_At_Event (The_Entry : not null Event_P;
                                 To_Run_At : Ada.Real_Time.Time);

         procedure Add_After_Event (The_Entry    : not null Event_P;
                                    To_Run_After : Duration);

         procedure Invalidate_Events
           (For_The_Instance : not null Instance_Base_P);
         --  Marks all the events on the queue which are for
         --  For_The_Instance as invalid, so they won't be actioned when
         --  their time arrives.

      private

         Started : Boolean := False;
         --  Set True by Running (at which point tasking must have
         --  started).

         Duration_Queue   : access Duration_Vectors.Vector :=
           new Duration_Vectors.Vector (Capacity => Capacity);
         Held_Event_Queue : access Time_Vectors.Vector :=
           new Time_Vectors.Vector (Capacity => Capacity);

      end Held_Events;

      task type Held_Event_Processing
        (The_Queue : access Event_Queue_Base'Class) is
      end Held_Event_Processing;

   end Held_Event_Management;
   use Held_Event_Management;


   task type Dispatcher (The_Queue    : access Event_Queue_Base'Class;
                         Priority     : System.Priority;
                         Storage_Size : Positive) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      pragma Priority (Priority);
      pragma Storage_Size (Storage_Size);

   end Dispatcher;


   type Event_Queue_Base (Priority     : System.Priority;
                          Storage_Size : Positive;
                          Capacity     : Ada.Containers.Count_Type)
     is new Events.Event_Queue_Base (Start_Started => True,
                                     Priority      => Priority,
                                     Storage_Size  => Storage_Size)
      with record

         The_Dispatchable_Events :
           Dispatchable_Events (Event_Queue_Base'Access,
                                Capacity => Capacity);

         The_Held_Events :
           Held_Events (Event_Queue_Base'Access,
                        Capacity => Capacity);

         The_Held_Event_Processing :
           Held_Event_Processing (Event_Queue_Base'Access);

         The_Dispatcher : Dispatcher (Event_Queue_Base'Access,
                                      Priority     => Priority,
                                      Storage_Size => Storage_Size);
      end record;

   procedure Invalidate_Events
     (On               : not null access Event_Queue_Base;
      For_The_Instance : not null access Instance_Base'Class);

end ColdFrame.Events.Standard;
