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
--  $Revision: 0d91c35b0d85 $
--  $Date: 2002/03/09 09:53:49 $
--  $Author: simon $

with GNAT.IO; use GNAT.IO;

package body ColdFrame.Events is


   procedure Finalize (The_Terminator : in out Instance_Terminator) is
   begin
      Put_Line ("an Instance_Terminator has just died.");
      --  We'll just mark the events as "don't dispatch me! I want to
      --  die", rather than deleting them from the queue now.
   end Finalize;


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


   procedure Finalize (The_Terminator : in out Timer_Terminator) is
   begin
      Put_Line ("a Timer_Terminator has just died.");
      --  XXX need to do _something_ about this timer still being on the
      --  queue!
      if The_Terminator.For_The_Timer.The_Event /= null then
         The_Terminator.For_The_Timer.The_Event.Invalidated := True;
      end if;
   end Finalize;


end ColdFrame.Events;
