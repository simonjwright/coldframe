--  $Id: client.adb,v 2cd92cbbdec1 2003/02/02 17:22:12 simon $

with GNAT.IO; use GNAT.IO;

with Serialization.Initialize;
with Serialization.Interface;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Ada.Strings.Unbounded;
with ColdFrame.Project.Calendar;
with Serialization_Demo.Serializable;

procedure Client is

begin

   Serialization.Initialize;

   Serialization.Interface.Open (On_Host => "localhost",
                                 Using_Port => 40673);


   loop

      Put_Line ("outputting a Client_Support record");
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
                 ("bounded"))));
      delay 5.0;

   end loop;

end Client;
