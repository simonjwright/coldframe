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

--  $RCSfile: server.adb,v $
--  $Revision: 141dc30870c4 $
--  $Date: 2003/02/20 20:58:51 $
--  $Author: simon $

--  This program receives and decodes TCP packets of type
--  ColdFrame.Serialization.Base'Class, using port 40673.

with Ada.Exceptions;
with ColdFrame.Serialization;
with GNAT.Sockets;
with GNAT.IO; use GNAT.IO;

with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with Serialization_Demo.Serializable;

procedure Server is

   Server_Socket : GNAT.Sockets.Socket_Type;
   Address : GNAT.Sockets.Sock_Addr_Type;
   Socket : GNAT.Sockets.Socket_Type;
   Connected_Address : GNAT.Sockets.Sock_Addr_Type;
   Channel : GNAT.Sockets.Stream_Access;

begin

   GNAT.Sockets.Initialize;

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
   GNAT.Sockets.Listen_Socket (Server_Socket, 1);

   --  wait for a connection ..
   GNAT.Sockets.Accept_Socket (Server_Socket, Socket, Connected_Address);

   --  .. got it, get the stream.
   Channel := GNAT.Sockets.Stream (Socket, Connected_Address);

   loop
      --  forever

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
