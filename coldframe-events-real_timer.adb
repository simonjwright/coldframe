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

--  $RCSfile: coldframe-events-real_timer.adb,v $
--  $Revision: fa4d5d083322 $
--  $Date: 2002/02/20 20:23:57 $
--  $Author: simon $

with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with BC.Containers.Queues.Ordered.Unbounded;

package body ColdFrame.Events.Real_Timer is


   procedure Post (The : Event_P;
                   On : access Event_Queue) is
   begin
      On.The_Excluder.Post (The);
   end Post;


   task body Dispatcher is

      E : Event_P;
      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Base'Class, Event_P);

   begin

      loop

         The_Queue.The_Excluder.Fetch (E);
--           Posted_Event_Queues.Pop_Value (The_Queue.The_Events, E);

         if not E.Invalidated then

            Log_Pre_Dispatch (The => E, On => The_Queue);
            Handler (E.all);
            Log_Post_Dispatch (The => E, On => The_Queue);

         end if;

         Delete (E);

      end loop;

   end Dispatcher;


   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration) is

      use type Ada.Real_Time.Time;

   begin

      case The.Status is

         when Initial | Fired =>
            The.The_Event := Event_P (To_Fire);
            The.Real_Time_To_Fire :=
              Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (After);
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
                  On.The_Excluder.Retract (The.The_Event, Success);
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
               On.The_Excluder.Retract (The.The_Event, Success);
               if not Success then
                  raise Use_Error;
               end if;
            end;
            Delete (The.The_Event);
            The.Status := Initial;

      end case;

   end Unset;


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
                     if Pos > 0 then
                        Timed_Event_Queues.Remove (The_Events, Pos);
                     end if;
                  end;
               end Remove;

            or
               delay until Timed_Event_Queues.Front
                 (The_Events).Real_Time_To_Fire;
               declare
                  T : constant Timer_P
                    := Timed_Event_Queues.Front (The_Events);
               begin
                  Post (T.The_Event, The_Queue);
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


   protected body Excluder is

      procedure Post (The : Event_P) is
      begin
         Unbounded_Posted_Event_Queues.Append (The_Queue.The_Events, The);
      end Post;

      entry Fetch (The : out Event_P)
      when not Unbounded_Posted_Event_Queues.Is_Empty (The_Queue.The_Events) is
      begin
         The := Unbounded_Posted_Event_Queues.Front (The_Queue.The_Events);
         Unbounded_Posted_Event_Queues.Pop (The_Queue.The_Events);
      end Fetch;

      procedure Retract (The : Event_P; Success : out Boolean) is
         Loc : constant Natural :=
           Unbounded_Posted_Event_Queues.Location
            (The_Queue.The_Events, The);
      begin
         if Loc = 0 then
            Success := False;
         else
            Log_Retraction (The => The, On => The_Queue);
            Unbounded_Posted_Event_Queues.Remove (The_Queue.The_Events, Loc);
            Success := True;
         end if;
      end Retract;

   end Excluder;


end ColdFrame.Events.Real_Timer;
