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

--  $RCSfile: coldframe-events.adb,v $
--  $Revision: 8e09f5990806 $
--  $Date: 2002/03/13 20:07:28 $
--  $Author: simon $

with Ada.Unchecked_Deallocation;

package body ColdFrame.Events is


   procedure Finalize (The_Terminator : in out Instance_Terminator) is
   begin
      if The_Terminator.For_The_Instance.Events_Posted_On /= null then
         --  Some events have been posted on a Queue
         Invalidate (On => The_Terminator.For_The_Instance.Events_Posted_On,
                     For_The_Instance => The_Terminator.For_The_Instance);
      end if;
   end Finalize;


   procedure Invalidate
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class) is
      pragma Warnings (Off, On);
      pragma Warnings (Off, For_The_Instance);
   begin
      null;
   end Invalidate;


   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue_Base) is
      pragma Warnings (Off, The);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Retraction;


   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue_Base) is
      pragma Warnings (Off, The);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue_Base) is
      pragma Warnings (Off, The);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Post_Dispatch;


   procedure Handler (This : Timer_Event) is
      The_Event : Event_P := This.The_Event;
      Time_To_Fire : Time_P := This.Time_To_Fire;
   begin

      --  First, provided that the Timer hasn't already been deleted,
      --  indicate to it that it's now unset.
      if This.The_Timer /= null then
         This.The_Timer.The_Entry := null;
      end if;

      --  Handle the held event, unless it's been invalidated.
      if not The_Event.Invalidated then
         Log_Pre_Dispatch (The => The_Event, On => This.On);
         Handler (The_Event.all);
         Log_Post_Dispatch (The => The_Event, On => This.On);
      end if;

      --  Free the referenced memory (the outer dispatcher will free
      --  This)
      Delete (The_Event);
      Delete (Time_To_Fire);

   end Handler;


   procedure Finalize (The_Terminator : in out Timer_Terminator) is
   begin

      if The_Terminator.For_The_Timer.The_Entry /= null then

         --  The Timer is set. Tell the timer event that the timer has
         --  been deleted.
         The_Terminator.For_The_Timer.The_Entry.The_Timer := null;

         --  Invalidate the held event.
         The_Terminator.For_The_Timer.The_Entry.The_Event.Invalidated := True;

      end if;

   end Finalize;


end ColdFrame.Events;
