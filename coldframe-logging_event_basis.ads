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

--  $RCSfile: coldframe-logging_event_basis.ads,v $
--  $Revision: 6469ed193604 $
--  $Date: 2003/11/11 06:28:08 $
--  $Author: simon $

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with BC.Containers;
with BC.Support.Statistics;
with ColdFrame.Event_Basis;
with High_Resolution_Time;

package ColdFrame.Logging_Event_Basis is


   --  A basis for Events that log the time used.

   type Event_Base is abstract new Event_Basis.Event_Base with private;

   procedure Log (The_Event : access Event_Base;
                  At_Phase : Event_Basis.Event_Processing_Phase);


   --  Simple printing of statistics.

   procedure Print
     (To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);


   --  A means of extracting the statistics, so that (for example)
   --  they can be transmitted over the network to a logging machine.

   type Datum is record
      Event : Ada.Strings.Unbounded.Unbounded_String;
      Queueing : BC.Support.Statistics.Instance;
      Executing : BC.Support.Statistics.Instance;
   end record;

   package Abstract_Datum_Containers
   is new BC.Containers (Datum);

   function Results return Abstract_Datum_Containers.Container'Class;


private

   type Event_Base is abstract new Event_Basis.Event_Base with record
      Last_Phase : Event_Basis.Event_Processing_Phase
        := Event_Basis.Event_Processing_Phase'First;
      Posted : High_Resolution_Time.Time;
      Dispatched : High_Resolution_Time.Time;
      --  no need for Finished, it's the time when Log is called at
      --  phase Finished.
   end record;

end ColdFrame.Logging_Event_Basis;
