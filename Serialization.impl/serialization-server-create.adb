--  $Id: serialization-server-create.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

separate (Serialization.Server)
procedure Create
  (Connecting_To_Host : String;
   Using_Port : Port) is

   H : Handle;
   Address : GNAT.Sockets.Sock_Addr_Type;

begin

   H := Create;

   GNAT.Sockets.Create_Socket (This.Sock,
                               GNAT.Sockets.Family_Inet,
                               GNAT.Sockets.Socket_Datagram);
   GNAT.Sockets.Set_Socket_Option (This.Sock,
                                   GNAT.Sockets.Socket_Level,
                                   (GNAT.Sockets.Reuse_Address, True));
   Address.Addr := GNAT.Sockets.Addresses
     (GNAT.Sockets.Get_Host_By_Name (Connecting_To_Host), 1);
   Address.Port := Using_Port;
   This.Channel := GNAT.Sockets.Stream (This.Sock, Address);

end Create;
