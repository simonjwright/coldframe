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

--  $RCSfile: coldframe-logging_event_basis.adb,v $
--  $Revision: bc275143d585 $
--  $Date: 2003/11/08 17:12:59 $
--  $Author: simon $

with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Exceptions;

package body ColdFrame.Logging_Event_Basis is


   procedure Log (The_Event : access Event_Base;
                  At_Phase : Event_Basis.Event_Processing_Phase) is
      use type Event_Basis.Event_Processing_Phase;
      use type High_Resolution_Time.Time;
   begin
      case At_Phase is
         when Event_Basis.Initial =>
            raise Exceptions.Use_Error;
         when Event_Basis.Posting =>
            pragma Assert (The_Event.Last_Phase = Event_Basis.Initial,
                          "loggable event at wrong phase");
            The_Event.Last_Phase := Event_Basis.Posting;
            The_Event.Posted := High_Resolution_Time.Clock;
         when Event_Basis.Dispatching =>
            pragma Assert (The_Event.Last_Phase = Event_Basis.Posting,
                          "loggable event at wrong phase");
            The_Event.Last_Phase := Event_Basis.Dispatching;
            The_Event.Dispatched := High_Resolution_Time.Clock;
         when Event_Basis.Finishing =>
            pragma Assert (The_Event.Last_Phase /= Event_Basis.Finishing,
                           "loggable event at wrong phase");
            declare
               Now : constant High_Resolution_Time.Time
                 := High_Resolution_Time.Clock;
            begin
               Put_Line
                 (Ada.Tags.Expanded_Name (Event_Base'Class (The_Event.all)'Tag)
                    & ","
                    & Duration'Image
                        (The_Event.Dispatched - The_Event.Posted)
                    & ","
                    & Duration'Image (Now - The_Event.Dispatched));
            end;
      end case;
   end Log;


   procedure Print is
   begin
      null;
   end Print;


end ColdFrame.Logging_Event_Basis;
