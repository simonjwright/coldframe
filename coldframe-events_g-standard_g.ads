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

--  $RCSfile: coldframe-events_g-standard_g.ads,v $
--  $Revision: cc2d36d988d5 $
--  $Date: 2002/03/22 05:57:51 $
--  $Author: simon $

with BC.Containers.Queues.Unbounded;
with BC.Containers.Queues.Ordered.Unbounded;

generic
package ColdFrame.Events_G.Standard_G is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue is new Event_Queue_Base with private;

   procedure Post (The : Event_P;
                   On : access Event_Queue);

   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  At_Time : Time.Time);

   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration);

   procedure Unset (The : in out Timer;
                    On : access Event_Queue);


private

   package Abstract_Posted_Event_Containers
   is new BC.Containers (Event_P);
   package Abstract_Posted_Event_Queues
   is new Abstract_Posted_Event_Containers.Queues;
   package Unbounded_Posted_Event_Queues
   is new Abstract_Posted_Event_Queues.Unbounded
     (Storage => Event_Storage);

   function "<" (L, R : Timer_Queue_Entry_P) return Boolean;

   package Abstract_Timed_Event_Containers
   is new BC.Containers (Timer_Queue_Entry_P);
   package Abstract_Timed_Event_Queues
   is new Abstract_Timed_Event_Containers.Queues;
   package Abstract_Timed_Event_Ordered_Queues
   is new Abstract_Timed_Event_Queues.Ordered;
   package Timed_Event_Queues
   is new Abstract_Timed_Event_Ordered_Queues.Unbounded
     (Storage => Event_Storage);


   task type Dispatcher (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      pragma Priority (16);

   end Dispatcher;


   task type Timer_Manager (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      entry Append (The : Timer_Queue_Entry_P);

      entry Invalidate (For_The_Instance : Instance_Base_P);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  Fetched.

   end Timer_Manager;


   --  Mutual exclusion between posters and the Dispatcher.
   protected type Excluder (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      procedure Post (The : Event_P);
      --  Post an event.

      entry Fetch (The : out Event_P);
      --  Blocks until there is an event on the queue; when one is found,
      --  removes it from the queue and sets "The".

      procedure Invalidate (For_The_Instance : access Instance_Base'Class);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  Fetched.

   end Excluder;

   --  The actual Event Queue.
   type Event_Queue is new Event_Queue_Base with record
      The_Excluder : Excluder (Event_Queue'Access);
      The_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Dispatcher : Dispatcher (Event_Queue'Access);
      The_Timer_Manager : Timer_Manager (Event_Queue'Access);
   end record;

   procedure Invalidate
     (On : access Event_Queue;
      For_The_Instance : access Instance_Base'Class);

end ColdFrame.Events_G.Standard_G;
