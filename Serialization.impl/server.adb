--  $Id: server.adb,v 72f13cb6bb38 2003/01/24 06:22:20 simon $

--  This program receives and decodes TCP packets of type
--  ColdFrame.Serialization.Base'Class, using port 40673.

with Ada.Exceptions;
with ColdFrame.Serialization;
with GNAT.Sockets;
with GNAT.IO; use GNAT.IO;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Serialization_Demo;

procedure Server is

   Server_Socket : GNAT.Sockets.Socket_Type;
   Address : GNAT.Sockets.Sock_Addr_Type;
   Socket : GNAT.Sockets.Socket_Type;
   Connected_Address : GNAT.Sockets.Sock_Addr_Type;
   Channel : GNAT.Sockets.Stream_Access;

begin

   GNAT.Sockets.Create_Socket (Server_Socket,
                               GNAT.Sockets.Family_Inet,
                               GNAT.Sockets.Socket_Stream);
   GNAT.Sockets.Set_Socket_Option (Server_Socket,
                                   GNAT.Sockets.Socket_Level,
                                   (GNAT.Sockets.Reuse_Address, True));
   Address.Addr := GNAT.Sockets.Addresses
     (GNAT.Sockets.Get_Host_By_Name ("localhost"), 1);
   Address.Port := 40673;
   GNAT.Sockets.Bind_Socket (Server_Socket, Address);
   GNAT.Sockets.Listen_Socket (Server_Socket, 5);
   GNAT.Sockets.Accept_Socket (Server_Socket, Socket, Connected_Address);
   Channel := GNAT.Sockets.Stream (Socket, Connected_Address);

   loop

      begin

         Put_Line ("about to read ..");
         declare
            Rec : constant ColdFrame.Serialization.Base'Class :=
              ColdFrame.Serialization.Base'Class'Input (Channel);
         begin
            Put_Line (ColdFrame.Serialization.Image (Rec));
         end;

      exception
         when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
            delay 1.0;  -- to give us time to C-c out
      end;

   end loop;

end Server;
