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

--  $RCSfile: coldframe-events-wall_timer.adb,v $
--  $Revision: f880bcb9ecd3 $
--  $Date: 2002/02/01 20:48:43 $
--  $Author: simon $

with Ada.Unchecked_Deallocation;
with BC.Containers.Queues.Ordered.Unbounded;

package body ColdFrame.States.Wall_Timer is


   procedure Post (It : Event_Base'Class;
                   On : access Event_Queue) is
   begin
      Event_Queues.Append (On.The_Events, It'Unrestricted_Access);
   end Post;


   procedure Retract (It : Event_Base'Class;
                      On : access Event_Queue;
                      Success : out Boolean)  is

      --  We make two distinct accesses to the synchronized event
      --  queue, so you might think that another task could get in and
      --  alter the queue between our working out where our event is
      --  in it (if indeed it is) and our removing it. However, this
      --  procedure is called by an action, which is in the context of
      --  our Dispatcher, so (since it's not a priority queue) the
      --  only possible alteration is the addition of another (timer)
      --  event after this one. We certainly won't have taken anything
      --  off the front.
      --
      --  Ideally, we'd add a single Synchronized_Queue operation to
      --  remove (the first occurrence of) an Item from a Queue.

      Loc : constant Natural
        := Event_Queues.Location (On.The_Events, It'Unrestricted_Access);
      Q : Event_Queue'Class renames Event_Queue'Class (On.all);
      --  we need a classwide object to force dispatching in the call
      --  to Log_Retraction

   begin

      if Loc = 0 then
         Success := False;
      else
         Log_Retraction (It => It, On => Q'Access);
         Event_Queues.Remove (On.The_Events, Loc);
         Success := True;
      end if;

   end Retract;


   task body Dispatcher is

      E : Event_P;
      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Base'Class, Event_P);

   begin

      loop

         Event_Queues.Pop_Value (The_Queue.The_Events, E);

         if not E.Invalidated then

            Log_Pre_Dispatch (It => E.all, On => The_Queue);
            Handler (E.all);
            Log_Post_Dispatch (It => E.all, On => The_Queue);

         end if;

         Delete (E);

      end loop;

   end Dispatcher;


   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : access Event_Base'Class;
                  After : Natural_Duration) is

      use type Ada.Calendar.Time;

   begin

      case The.Status is

         when Initial | Fired =>
            The.The_Event := Event_P (To_Fire);
            The.Wall_Time_To_Fire := Ada.Calendar.Clock + After;
            The.Status := Set;
            On.The_Timer_Manager.Append (The'Unrestricted_Access);

         when Set =>
            raise Use_Error;

      end case;

   end Set;


   procedure Unset (The : in out Timer;
                    On : access Event_Queue) is

      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Base'Class, Event_P);

   begin

      case The.Status is

         when Initial =>
            raise Use_Error;

         when Set =>
            On.The_Timer_Manager.Remove (The'Unrestricted_Access);
            if The.Status = Fired then
               --  On.The_Timer_Manager got in first, need to retract
               --  the event.
               --
               --  If the event isn't in the event queue, something
               --  has gone far wrong.
               declare
                  Success : Boolean;
               begin
                  Retract (The.The_Event.all, On, Success);
                  if not Success then
                     raise Program_Error;
                  end if;
               end;
            end if;
            Delete (The.The_Event);
            The.Status := Initial;

         when Fired =>
            --  If the event isn't in the event queue, it must have
            --  already been dispatched; so the user shouldn't be
            --  trying to cancel it.
            declare
               Success : Boolean;
            begin
               Retract (The.The_Event.all, On, Success);
               if not Success then
                  raise Use_Error;
               end if;
            end;
            Delete (The.The_Event);
            The.Status := Initial;

      end case;

   end Unset;


   ---------------
   --  Logging  --
   ---------------

   procedure Log_Retraction (It : Event_Base'Class;
                             On : access Event_Queue) is
   begin
      null;
   end Log_Retraction;


   procedure Log_Pre_Dispatch (It : Event_Base'Class;
                               On : access Event_Queue) is
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (It : Event_Base'Class;
                                On : access Event_Queue) is
   begin
      null;
   end Log_Post_Dispatch;


   ------------------------------
   --  Timed event management  --
   ------------------------------

   function "<" (L, R : Timer_P) return Boolean;

   package Abstract_Timed_Event_Containers
   is new BC.Containers (Timer_P);
   package Abstract_Timed_Event_Queues
   is new Abstract_Timed_Event_Containers.Queues;
   package Abstract_Timed_Event_Ordered_Queues
   is new Abstract_Timed_Event_Queues.Ordered;
   package Timed_Event_Queues
   is new Abstract_Timed_Event_Ordered_Queues.Unbounded
     (Storage => ColdFrame.Global_Storage_Pool.Pool);


   task body Timer_Manager is

      The_Events : Timed_Event_Queues.Queue;

   begin

      loop

         if Timed_Event_Queues.Is_Empty (The_Events) then

            accept Append (The : Timer_P) do
               Timed_Event_Queues.Append (The_Events, The);
            end Append;

         else

            select
               accept Append (The : Timer_P) do
                  Timed_Event_Queues.Append (The_Events, The);
               end Append;

            or
               accept Remove (The : Timer_P) do
                  declare
                     Pos : constant Natural
                       := Timed_Event_Queues.Location (The_Events, The);
                  begin
                     Timed_Event_Queues.Remove (The_Events, Pos);
                  end;
               end Remove;

            or
               delay until Timed_Event_Queues.Front
                 (The_Events).Wall_Time_To_Fire;
               declare
                  T : constant Timer_P
                    := Timed_Event_Queues.Front (The_Events);
               begin
                  Post (T.The_Event.all, The_Queue);
                  T.Status := Fired;
                  Timed_Event_Queues.Pop (The_Events);
               end;

            end select;

         end if;

      end loop;

   end Timer_Manager;


   function "<" (L, R : Timer_P) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return L.Wall_Time_To_Fire < R.Wall_Time_To_Fire;
   end "<";


end ColdFrame.States.Wall_Timer;
