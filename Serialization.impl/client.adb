--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $RCSfile: client.adb,v $
--  $Revision: 484929c83c72 $
--  $Date: 2004/05/01 15:16:00 $
--  $Author: simon $


with GNAT.IO; use GNAT.IO;

with Serialization.Initialize;
with Serialization.Interface;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Ada.Real_Time;
with Ada.Strings.Unbounded;
with ColdFrame.Project.Calendar;
with Serialization_Demo.Serializable;

procedure Client is

begin

   Serialization.Initialize;

   Serialization.Interface.Open (On_Host => "localhost",
                                 Using_Port => 40673);


   loop

      Put_Line ("outputting a Sample_A record");
      Serialization.Interface.Output
        (Serialization_Demo.Serializable.Sample_A'
           (Serialization.Serializable_Base with
              Payload =>
              (I => 42,
               F => 0.12345,
               B => False,
               D => 1.2345,
               T => ColdFrame.Project.Calendar.Clock,
               U => Ada.Strings.Unbounded.To_Unbounded_String ("unbounded"),
               N => Serialization_Demo.Name_String_Package.To_Bounded_String
                 ("bounded"),
               R => (T => Ada.Real_Time.Clock))));
      delay 0.5;
      Put_Line ("outputting a Recordable_Real_Time record");
      Serialization.Interface.Output
        (Serialization_Demo.Serializable.Recordable_Real_Time'
           (Serialization.Serializable_Base with
              Payload =>
              (T => Ada.Real_Time.Clock)));
      delay 0.5;
      Put_Line ("outputting a Real_Time record");
      Serialization.Interface.Output
        (Serialization_Demo.Serializable.Real_Time'
           (Serialization.Serializable_Base with
              Payload => Ada.Real_Time.Clock));

      delay 5.0;

   end loop;

end Client;
