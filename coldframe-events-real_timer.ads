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

--  $RCSfile: coldframe-events-real_timer.ads,v $
--  $Revision: fa4d5d083322 $
--  $Date: 2002/02/20 20:23:57 $
--  $Author: simon $

with BC.Containers.Queues.Unbounded;
with ColdFrame.Global_Storage_Pool;

package ColdFrame.Events.Real_Timer is

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
     (Storage => ColdFrame.Global_Storage_Pool.Pool);

   task type Dispatcher (The_Queue : access Event_Queue'Class) is
      pragma Priority (16);
   end Dispatcher;
   --  We need to constrain by 'Class so that internal calls to
   --  potentially dispatching operations (such as
   --  Log_{Pre,Post}_Dispatch) will in fact dispatch.


   type Timer_P is access all Timer;

   task type Timer_Manager (The_Queue : access Event_Queue'Class) is
      entry Append (The : Timer_P);
      entry Remove (The : Timer_P);
   end Timer_Manager;
   --  We need to constrain by 'Class so that internal calls to
   --  potentially dispatching operations (such as
   --  Log_{Pre,Post}_Dispatch) will in fact dispatch.


   --  Mutual exclusion between posters and the Dispatcher.
   protected type Excluder (The_Queue : access Event_Queue'Class) is
      procedure Post (The : Event_P);
      entry Fetch (The : out Event_P);
      procedure Retract (The : Event_P; Success : out Boolean);
   end Excluder;


   --  The actual Event Queue.
   type Event_Queue is new Event_Queue_Base with record
      The_Excluder : Excluder (Event_Queue'Access);
      The_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Dispatcher : Dispatcher (Event_Queue'Access);
      The_Timer_Manager : Timer_Manager (Event_Queue'Access);
   end record;

end ColdFrame.Events.Real_Timer;
