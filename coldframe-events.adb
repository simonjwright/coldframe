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
--  $Revision: 8937907f915d $
--  $Date: 2002/02/06 20:50:35 $
--  $Author: simon $

--  with Ada.Calendar;
--  with Ada.Exceptions;
with GNAT.IO; use GNAT.IO;
--  with GNAT.Calendar.Time_IO;

package body ColdFrame.States is


--     function Seconds (Date : Ada.Calendar.Time) return String;
--     function Seconds (Date : Ada.Calendar.Time) return String is
--        use type Ada.Calendar.Time;
--        S : Duration
--          := Duration (GNAT.Calendar.Second (Date))
--          + GNAT.Calendar.Sub_Second (Date);
--     begin
--        return GNAT.Calendar.Time_IO.Image (Date, "%H%M") & S'Img;
--     end Seconds;


--     procedure Log (Event : String; State : String) is
--     begin
--        Put_Line ("event "
--                  & Event
--                  & " occurred in state "
--                  & State
--                  & " at "
--                  & Seconds (Ada.Calendar.Clock));
--     end Log;


--     procedure Log (Entering : String) is
--     begin
--        Put_Line ("entering state " & Entering);
--     end Log;


   procedure Finalize (The_Terminator : in out Terminator) is
   begin
      Put_Line ("a Terminator has just died.");
      --  Needs to visit the Timer queue to kill off any events there,
      --  then the Event queue. We'll just mark the events as "don't
      --  dispatch me! I want to die", rather than deleting them from
      --  the queue now.
   end Finalize;


   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue_Base) is
   begin
      null;
   end Log_Retraction;


   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue_Base) is
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue_Base) is
   begin
      null;
   end Log_Post_Dispatch;


end ColdFrame.States;
