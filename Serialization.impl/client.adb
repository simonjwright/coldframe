--  $Id: client.adb,v 3d93d0840eae 2003/01/26 19:06:06 simon $

with GNAT.IO; use GNAT.IO;

with Serialization.Initialize;
with Serialization.Interface;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Serialization_Demo.Serializable;
with ColdFrame.Project.Calendar;

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
              Payload => (I => 42,
                          F => 0.12345,
                          B => False,
                          D => 1.2345,
                          T => ColdFrame.Project.Calendar.Clock)));
      delay 5.0;

   end loop;

end Client;
