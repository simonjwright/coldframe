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
--  $Revision: dc7bcd001839 $
--  $Date: 2003/11/09 17:45:35 $
--  $Author: simon $

with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with BC.Containers.Maps.Unmanaged;
with BC.Support.Statistics;
with ColdFrame.Exceptions;
with ColdFrame.Hash.Strings.Standard;

package body ColdFrame.Logging_Event_Basis is


   function Tag_Hash (T : Ada.Tags.Tag) return Natural;

   type Info is record
      Queueing : BC.Support.Statistics.Instance;
      Executing : BC.Support.Statistics.Instance;
   end record;
   type Info_P is access Info;

   package Abstract_Containers
   is new BC.Containers (Info_P);

   package Abstract_Maps
   is new Abstract_Containers.Maps (Key => Ada.Tags.Tag,
                                   "=" => Ada.Tags."=");

   package Maps
   is new Abstract_Maps.Unmanaged (Hash => Tag_Hash,
                                   Buckets => 89);

   function Tag_Hash (T : Ada.Tags.Tag) return Natural is
   begin
      return ColdFrame.Hash.Strings.Standard (Ada.Tags.Expanded_Name (T));
   end Tag_Hash;


   Data : Maps.Map;


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
               Tag : constant Ada.Tags.Tag
                 := Event_Base'Class (The_Event.all)'Tag;
               Inf : Info_P;
            begin
               if Maps.Is_Bound (Data, Tag) then
                  Inf := Maps.Item_Of (Data, Tag);
               else
                  Inf := new Info;
                  Maps.Bind (Data, Tag, Inf);
               end if;
               BC.Support.Statistics.Add
                 (Long_Float (The_Event.Dispatched - The_Event.Posted),
                  To => Inf.Queueing);
               BC.Support.Statistics.Add
                 (Long_Float (Now - The_Event.Dispatched),
                  To => Inf.Executing);
            end;
      end case;
   end Log;


   procedure Print is
      It : Abstract_Containers.Iterator'Class
        := Maps.New_Iterator (Data);
   begin
      Put_Line ("printing event statistics:");
      while not Abstract_Containers.Is_Done (It) loop
         declare
            T : constant Ada.Tags.Tag
              := Abstract_Maps.Current_Key
                    (Abstract_Maps.Map_Iterator'Class (It));
            Inf : constant Info_P
              := Abstract_Containers.Current_Item (It);
            use BC.Support;
         begin
            Put (Ada.Tags.Expanded_Name (T));
            Put (',');
            Put (Integer'Image (Statistics.Count (Inf.Queueing)));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Mean (Inf.Queueing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Min (Inf.Queueing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Max (Inf.Queueing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Sigma (Inf.Queueing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Mean (Inf.Executing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Min (Inf.Executing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Max (Inf.Executing))));
            Put (',');
            Put (Duration'Image (Duration (Statistics.Sigma (Inf.Executing))));
            New_Line;
         end;
         Abstract_Containers.Next (It);
      end loop;
   end Print;


end ColdFrame.Logging_Event_Basis;
