--  $Id: serialization-interface-open.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

with Serialization.Server;

separate (Serialization.Interface)
procedure Open
  (On_Host : String;
   Using_Port : Port) is

begin

   Server.Create (Connecting_To_Host => On_Host,
                  Using_Port => Using_Port);

end Open;
