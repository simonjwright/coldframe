--  $Id: serialization-server-create.adb,v 03ad5137fc4e 2003/02/05 20:46:31 simon $

separate (Serialization.Server)
procedure Create
  (Connecting_To_Host : String;
   Using_Port : Port) is

   H : Handle;
   Address : GNAT.Sockets.Sock_Addr_Type;

begin

   GNAT.Sockets.Initialize;

   H := Create;

   GNAT.Sockets.Create_Socket (This.Sock,
                               GNAT.Sockets.Family_Inet,
                               GNAT.Sockets.Socket_Stream);
   GNAT.Sockets.Set_Socket_Option (This.Sock,
                                   GNAT.Sockets.Socket_Level,
                                   (GNAT.Sockets.Reuse_Address, True));
   Address.Addr := GNAT.Sockets.Addresses
     (GNAT.Sockets.Get_Host_By_Name (Connecting_To_Host), 1);
   Address.Port := Using_Port;
   GNAT.Sockets.Connect_Socket (This.Sock, Address);
   This.Channel := GNAT.Sockets.Stream (This.Sock, Address);

end Create;
