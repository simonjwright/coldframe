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

--  $RCSfile: coldframe-logging_event_basis-ews_support.adb,v $
--  $Revision: 5aff14253c7c $
--  $Date: 2003/11/15 14:07:52 $
--  $Author: simon $

with Ada.Calendar;
with EWS.Types;
with GNAT.Calendar.Time_IO;

function ColdFrame.Logging_Event_Basis.EWS_Page
  (From_Request : EWS.HTTP.Request_P)
  return EWS.Dynamic.Dynamic_Response'Class is

   function Less_Than (L, R : Datum) return Boolean;

   procedure Sort
   is new ColdFrame.Logging_Event_Basis.Sort ("<" => Less_Than);

   function Less_Than (L, R : Datum) return Boolean is
   begin
      return Ada.Strings.Unbounded."<" (L.Event, R.Event);
   end Less_Than;

   Result : EWS.Dynamic.Dynamic_Response (From_Request);

   Data : Abstract_Datum_Containers.Container'Class
     := Logging_Event_Basis.Results;

   It : Abstract_Datum_Containers.Iterator'Class
     := Abstract_Datum_Containers.New_Iterator (Data);

begin

   EWS.Dynamic.Set_Content_Type (Result, To => EWS.Types.HTML);

   EWS.Dynamic.Set_Content
     (Result,
      "<html><head><title>ColdFrame event statistics</title><head>"
        & "<body bgcolor=""white"">"
        & "The time is <b>");
   EWS.Dynamic.Append_Content
     (Result,
      Adding => GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                             "%c"));

   EWS.Dynamic.Append_Content
     (Result,
      "<p>"
        & "<table border=1>"
        & "<tr>"
        & "<th>Event<th>No<th>Q mean<th>sd<th>min<th>max"
        & "<th>D mean<th>sd<th>min<th>max");

   Sort (Data);
   Abstract_Datum_Containers.Reset (It);

   while not Abstract_Datum_Containers.Is_Done (It) loop
      declare
         D : constant Datum := Abstract_Datum_Containers.Current_Item (It);
         use BC.Support;
      begin
         EWS.Dynamic.Append_Content
           (Result,
            "<tr>"
              & "<td>"
              & Ada.Strings.Unbounded.To_String (D.Event)
              & "<td>"
              & Statistics.Count (D.Queueing)'Img
              & "<td bgcolor=""#dddddd"">"
              & Duration (Statistics.Mean (D.Queueing))'Img
              & "<td bgcolor=""#dddddd"">"
              & Duration (Statistics.Sigma (D.Queueing))'Img
              & "<td bgcolor=""#dddddd"">"
              & Duration (Statistics.Min (D.Queueing))'Img
              & "<td bgcolor=""#dddddd"">"
              & Duration (Statistics.Max (D.Queueing))'Img
              & "<td>"
              & Duration (Statistics.Mean (D.Executing))'Img
              & "<td>"
              & Duration (Statistics.Sigma (D.Executing))'Img
              & "<td>"
              & Duration (Statistics.Min (D.Executing))'Img
              & "<td>"
              & Duration (Statistics.Max (D.Executing))'Img);
      end;
      Abstract_Datum_Containers.Next (It);
   end loop;

   EWS.Dynamic.Append_Content (Result, "</table>");

   EWS.Dynamic.Append_Content
     (Result,
      "</b>"
        & "</body>"
        & "</html>");

   return Result;

end ColdFrame.Logging_Event_Basis.EWS_Page;
