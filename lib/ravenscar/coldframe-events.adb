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

package body ColdFrame.Events is


   procedure Check_Deletable (The_Instance : not null access Instance_Base) is
   begin
      if The_Instance.In_Handler then
         raise Exceptions.Use_Error
            with "deleting instance from event handler";
      end if;
   end Check_Deletable;


   procedure Finalize (The_Instance : not null access Instance_Base'Class) is
   begin
      if The_Instance.Events_Posted_On /= null then
         --  Some events have been posted on a Queue
         pragma Warnings (Off, "*No_Exception_Propagation*");
         Invalidate_Events
           (On => The_Instance.Events_Posted_On,
            For_The_Instance => The_Instance);
         pragma Warnings (On, "*No_Exception_Propagation*");
      end if;
   end Finalize;


   --  procedure Finalize (The_Lock : in out Lock) is
   --  begin
   --     if not The_Lock.Finalized then
   --        The_Lock.Finalized := True;
   --        Unlocker (The_Lock.The_Queue);
   --     end if;
   --  end Finalize;


   procedure Finalize (The_Timer : in out Timer) is
   begin
      if The_Timer.The_Entry /= null then

         declare
            pragma Warnings (Off, "*No_Exception_Propagation*");
            H : Held_Event renames Held_Event (The_Timer.The_Entry.all);
            pragma Warnings (On, "*No_Exception_Propagation*");
         begin
            --  The Timer is set. Tell the timer event that the timer has
            --  been deleted.
            H.The_Timer := null;

            --  Invalidate the held event.
            pragma Warnings (Off, "*No_Exception_Propagation*");
            H.The_Event.Invalidated := True;
            pragma Warnings (On, "*No_Exception_Propagation*");
         end;

         The_Timer.The_Entry := null;

      end if;
   end Finalize;


   procedure Handler (This : Held_Event) is
      The_Event : Event_P := This.The_Event;
   begin

      --  First, provided that the Timer (still) exists, indicate to
      --  it that it's now unset.
      if This.The_Timer /= null then
         This.The_Timer.The_Entry := null;
      end if;

      --  Handle the held event, unless it's been invalidated.
      if not The_Event.Invalidated then
         Start_Handling (The_Event);
         Handler (The_Event.all);
         Stop_Handling (The_Event);
      end if;

      --  Free the referenced memory (the outer dispatcher will free
      --  This)
      Delete (The_Event);

   end Handler;


   --  procedure Initialize (The_Lock : in out Lock) is
   --  begin
   --     Locker (The_Lock.The_Queue);
   --  end Initialize;


   procedure Instance_Is_Deleted
     (For_The_Event : not null access Instance_Event_Base'Class) is
   begin
      For_The_Event.Instance_Deleted := True;
   end Instance_Is_Deleted;


   procedure Invalidate (The_Event : not null access Event_Base;
                         If_For_Instance : not null Instance_Base_P) is
      pragma Unreferenced (The_Event);
      pragma Unreferenced (If_For_Instance);
   begin
      null;
   end Invalidate;


   procedure Invalidate (The_Event : not null access Instance_Event_Base;
                         If_For_Instance : not null Instance_Base_P) is
   begin
      if Instance_Base_P (The_Event.For_The_Instance) = If_For_Instance then
         The_Event.Invalidated := True;
      end if;
   end Invalidate;


   procedure Invalidate (The_Event : not null access Held_Event;
                         If_For_Instance : not null Instance_Base_P) is
   begin
      --  We need to invalidate the Held_Event if the event that it's
      --  holding is an instance event for For_The_Instance.
      if The_Event.The_Event.all in Instance_Event_Base'Class
        and then Instance_Base_P
          (Instance_Event_Base (The_Event.The_Event.all).For_The_Instance)
        = If_For_Instance
      then
         The_Event.Invalidated := True;
      end if;
   end Invalidate;


   procedure Invalidate_Events
     (On : not null access Event_Queue_Base;
      For_The_Instance : not null access Instance_Base'Class) is
      pragma Unreferenced (On);
      pragma Unreferenced (For_The_Instance);
   begin
      raise Program_Error;
   end Invalidate_Events;


   procedure Locker (The_Queue : not null access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Locker;


   procedure Mark_Deletable (The_Instance : not null access Instance_Base) is
   begin
      The_Instance.In_Handler := False;
   end Mark_Deletable;


   procedure Start (The_Queue : not null access Event_Queue_Base) is
   begin
      if The_Queue.Started then
         raise Exceptions.Use_Error with "queue already started";
      else
         The_Queue.Started := True;
         Start_Queue (Event_Queue_P (The_Queue)); -- need to dispatch
      end if;
   end Start;


   procedure Start_Handling (The_Event : not null access Event_Base) is
      pragma Unreferenced (The_Event);
   begin
      null;
   end Start_Handling;


   procedure Start_Handling
     (The_Event : not null access Instance_Event_Base) is
   begin
      The_Event.For_The_Instance.In_Handler := True;
   end Start_Handling;


   procedure Stop_Handling (The_Event : not null access Event_Base) is
      pragma Unreferenced (The_Event);
   begin
      null;
   end Stop_Handling;


   procedure Stop_Handling (The_Event : not null access Instance_Event_Base) is
   begin
      if not The_Event.Instance_Deleted then
         The_Event.For_The_Instance.In_Handler := False;
      end if;
   end Stop_Handling;


   procedure Start_Queue (The_Queue : not null access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Start_Queue;


   procedure Unlocker (The_Queue : not null access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Unlocker;


end ColdFrame.Events;
