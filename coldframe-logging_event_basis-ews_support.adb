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
--  $Revision: 9197014cdfa2 $
--  $Date: 2003/11/17 15:16:55 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Characters.Latin_1;
with EWS.Types;
with GNAT.Calendar.Time_IO;

package body ColdFrame.Logging_Event_Basis.EWS_Support is


   use Ada.Characters.Latin_1;
   CRLF : constant String := CR & LF;


   procedure Add_Section
     (For_Request : EWS.HTTP.Request_P;
      To : in out EWS.Dynamic.Dynamic_Response'Class) is

      pragma Warnings (Off, For_Request);

      function Less_Than (L, R : Datum) return Boolean;

      procedure Sort
      is new ColdFrame.Logging_Event_Basis.Sort ("<" => Less_Than);

      function Less_Than (L, R : Datum) return Boolean is
      begin
         return Ada.Strings.Unbounded."<" (L.Event, R.Event);
      end Less_Than;

      Data : Abstract_Datum_Containers.Container'Class
        := Logging_Event_Basis.Results;

      It : Abstract_Datum_Containers.Iterator'Class
        := Abstract_Datum_Containers.New_Iterator (Data);

   begin

      EWS.Dynamic.Append
        (To,
         Adding =>
           "<p>"
           & CRLF
           & "<table border=1>"
           & CRLF
           & "<tr>"
           & "<th bgcolor=""#eeeeee"">Event"
           & "<th>No"
           & "<th bgcolor=""#eeeeee"">Q mean"
           & "<th bgcolor=""#eeeeee"">sd"
           & "<th bgcolor=""#eeeeee"">min"
           & "<th bgcolor=""#eeeeee"">max"
           & "<th>D mean"
           & "<th>sd"
           & "<th>min"
           & "<th>max"
           & CRLF);

      Sort (Data);
      Abstract_Datum_Containers.Reset (It);

      while not Abstract_Datum_Containers.Is_Done (It) loop
         declare
            D : constant Datum := Abstract_Datum_Containers.Current_Item (It);
            use BC.Support;
         begin
            EWS.Dynamic.Append
              (To,
               Adding =>
                 "<tr>"
                 & "<td bgcolor=""#eeeeee"">"
                 & Ada.Strings.Unbounded.To_String (D.Event)
                 & "<td>"
                 & Statistics.Count (D.Queueing)'Img
                 & "<td bgcolor=""#eeeeee"">"
                 & Duration (Statistics.Mean (D.Queueing))'Img
                 & "<td bgcolor=""#eeeeee"">"
                 & Duration (Statistics.Sigma (D.Queueing))'Img
                 & "<td bgcolor=""#eeeeee"">"
                 & Duration (Statistics.Min (D.Queueing))'Img
                 & "<td bgcolor=""#eeeeee"">"
                 & Duration (Statistics.Max (D.Queueing))'Img
                 & "<td>"
                 & Duration (Statistics.Mean (D.Executing))'Img
                 & "<td>"
                 & Duration (Statistics.Sigma (D.Executing))'Img
                 & "<td>"
                 & Duration (Statistics.Min (D.Executing))'Img
                 & "<td>"
                 & Duration (Statistics.Max (D.Executing))'Img
                 & CRLF);
         end;
         Abstract_Datum_Containers.Next (It);
      end loop;

      EWS.Dynamic.Append (To, Adding => "</table>" & CRLF);

   end Add_Section;


   function Whole_Page
     (From_Request : EWS.HTTP.Request_P)
     return EWS.Dynamic.Dynamic_Response'Class is

      Result : EWS.Dynamic.Dynamic_Response (From_Request);

   begin

      EWS.Dynamic.Set_Content_Type (Result, To => EWS.Types.HTML);

      EWS.Dynamic.Set_Content
        (Result,
         "<html><head><title>ColdFrame event statistics</title></head>"
           & CRLF
           & "<body bgcolor=""white"">"
           & CRLF);

      EWS.Dynamic.Append
        (Result, Adding => "The time is <b>");
      EWS.Dynamic.Append
        (Result,
         Adding => GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                                "%c"));
      EWS.Dynamic.Append
        (Result, Adding => "</b>" & CRLF);

      Add_Section (For_Request => From_Request,
                   To => Result);

      EWS.Dynamic.Append
        (Result,
         Adding =>
           "</body>"
           & CRLF
           & "</html>"
           & CRLF);

      return Result;

   end Whole_Page;


end ColdFrame.Logging_Event_Basis.EWS_Support;
