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
--  $Revision: 7af0bcf0d4e0 $
--  $Date: 2003/11/11 21:25:05 $
--  $Author: simon $

with Ada.Tags;
with Ada.Unchecked_Conversion;
with BC.Containers.Collections.Unmanaged;
with BC.Containers.Maps.Unmanaged;
with BC.Support.Synchronization;
with ColdFrame.Exceptions;
with ColdFrame.Hash.Access_Hash;

package body ColdFrame.Logging_Event_Basis is


   --  The remainder of the spec's implementation of collections of
   --  Datums.

   package Abstract_Datum_Collections
   is new Abstract_Datum_Containers.Collections;

   package Datum_Collections
   is new Abstract_Datum_Collections.Unmanaged;


   --  The Map for data collection (no string key here, for
   --  performance reasons).

   type Info is record
      Queueing : BC.Support.Statistics.Instance;
      Executing : BC.Support.Statistics.Instance;
   end record;
   type Info_P is access Info;

   function Tag_Hash (T : Ada.Tags.Tag) return Natural;

   package Abstract_Info_Containers
   is new BC.Containers (Info_P);

   package Abstract_Info_Maps
   is new Abstract_Info_Containers.Maps (Key => Ada.Tags.Tag,
                                         "=" => Ada.Tags."=");

   package Info_Maps
   is new Abstract_Info_Maps.Unmanaged (Hash => Tag_Hash,
                                        Buckets => 89);

   function Tag_Hash (T : Ada.Tags.Tag) return Natural is
      type P is access all Integer;
      function To_P is new Ada.Unchecked_Conversion (Ada.Tags.Tag, P);
      function P_Hash is new ColdFrame.Hash.Access_Hash (Integer, P);
   begin
      return P_Hash (To_P (T));
   end Tag_Hash;


   Data : Info_Maps.Map;
   Access_Control : BC.Support.Synchronization.Semaphore;


   procedure Log (The_Event : access Event_Base;
                  At_Phase : Event_Basis.Event_Processing_Phase) is
      use type Event_Basis.Event_Processing_Phase;
      use type Project.High_Resolution_Time.Time;
   begin
      case At_Phase is
         when Event_Basis.Initial =>
            raise Exceptions.Use_Error;
         when Event_Basis.Posting =>
            pragma Assert (The_Event.Last_Phase = Event_Basis.Initial,
                          "loggable event at wrong phase");
            The_Event.Last_Phase := Event_Basis.Posting;
            The_Event.Posted := Project.High_Resolution_Time.Clock;
         when Event_Basis.Dispatching =>
            pragma Assert (The_Event.Last_Phase = Event_Basis.Posting,
                          "loggable event at wrong phase");
            The_Event.Last_Phase := Event_Basis.Dispatching;
            The_Event.Dispatched := Project.High_Resolution_Time.Clock;
         when Event_Basis.Finishing =>
            pragma Assert (The_Event.Last_Phase /= Event_Basis.Finishing,
                           "loggable event at wrong phase");
            declare
               Now : constant Project.High_Resolution_Time.Time
                 := Project.High_Resolution_Time.Clock;
               Tag : constant Ada.Tags.Tag
                 := Event_Base'Class (The_Event.all)'Tag;
               Inf : Info_P;
            begin
               BC.Support.Synchronization.Seize (Access_Control);
               if Info_Maps.Is_Bound (Data, Tag) then
                  Inf := Info_Maps.Item_Of (Data, Tag);
               else
                  Inf := new Info;
                  Info_Maps.Bind (Data, Tag, Inf);
               end if;
               BC.Support.Statistics.Add
                 (Long_Float (The_Event.Dispatched - The_Event.Posted),
                  To => Inf.Queueing);
               BC.Support.Statistics.Add
                 (Long_Float (Now - The_Event.Dispatched),
                  To => Inf.Executing);
               BC.Support.Synchronization.Release (Access_Control);
            end;
      end case;
   end Log;


   procedure Print
     (To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output) is
      It : Abstract_Info_Containers.Iterator'Class
        := Info_Maps.New_Iterator (Data);
      use Ada.Text_IO;
   begin
      BC.Support.Synchronization.Seize (Access_Control);
      Abstract_Info_Containers.Reset (It);
      while not Abstract_Info_Containers.Is_Done (It) loop
         declare
            T : constant Ada.Tags.Tag
              := Abstract_Info_Maps.Current_Key
              (Abstract_Info_Maps.Map_Iterator'Class (It));
            Inf : constant Info_P
              := Abstract_Info_Containers.Current_Item (It);
            use BC.Support;
         begin
            Put (To_File,
                 Ada.Tags.Expanded_Name (T));
            Put (To_File, ',');
            Put (To_File,
                 Integer'Image (Statistics.Count (Inf.Queueing)));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Mean (Inf.Queueing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Min (Inf.Queueing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Max (Inf.Queueing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Sigma (Inf.Queueing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Mean (Inf.Executing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Min (Inf.Executing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Max (Inf.Executing))));
            Put (To_File, ',');
            Put (To_File,
                 Duration'Image (Duration
                                   (Statistics.Sigma (Inf.Executing))));
            New_Line (To_File);
         end;
         Abstract_Info_Containers.Next (It);
      end loop;
      BC.Support.Synchronization.Release (Access_Control);
   end Print;


   function Results return Abstract_Datum_Containers.Container'Class is
      Result : Datum_Collections.Collection;
      It : Abstract_Info_Containers.Iterator'Class
        := Info_Maps.New_Iterator (Data);
   begin
      BC.Support.Synchronization.Seize (Access_Control);
      Abstract_Info_Containers.Reset (It);
      while not Abstract_Info_Containers.Is_Done (It) loop
         declare
            T : constant Ada.Tags.Tag
              := Abstract_Info_Maps.Current_Key
                    (Abstract_Info_Maps.Map_Iterator'Class (It));
            Inf : constant Info_P
              := Abstract_Info_Containers.Current_Item (It);
            use Ada.Strings.Unbounded;
         begin
            Datum_Collections.Append
              (Result,
               (To_Unbounded_String (Ada.Tags.Expanded_Name (T)),
                Inf.Queueing,
                Inf.Executing));
         end;
         Abstract_Info_Containers.Next (It);
      end loop;
      BC.Support.Synchronization.Release (Access_Control);
      return Result;
   end Results;


end ColdFrame.Logging_Event_Basis;
