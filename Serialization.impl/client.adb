--  $Id: client.adb,v af2592de3d70 2003/01/24 06:21:50 simon $

with GNAT.IO; use GNAT.IO;

with Serialization.Initialize;
with Serialization.Interface;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Serialization_Demo;
with ColdFrame.Project.Calendar;

procedure Client is

begin

   Serialization.Initialize;

   Serialization.Interface.Open (On_Host => "localhost",
                                 Using_Port => 40673);


   loop

      Put_Line ("outputting a Client_Support record");
      Serialization.Interface.Output
        (Serialization_Demo.Sample_A'
           (Serialization.Serializable_Base with I => 42,
            F => 0.12345,
            B => False,
            D => 1.2345,
            T => ColdFrame.Project.Calendar.Clock));
      delay 5.0;

   end loop;

end Client;
