--  $Id: client.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

with GNAT.IO; use GNAT.IO;

with Serialization.Initialize;
with Serialization.Interface;

with Client_Support;

procedure Client is

begin

   Serialization.Initialize;

   Serialization.Interface.Open (On_Host => "localhost",
                                 Using_Port => 40773);


   Serialization.Interface.Output
     (Client_Support.Rec'(Serialization.Serializable_Base with X => 42,
                          Y => 73));

end Client;
