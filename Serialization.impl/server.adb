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
--  $Revision: 48357f4482f1 $
--  $Date: 2003/09/10 05:44:52 $
--  $Author: simon $

--  This program receives and decodes TCP packets of type
--  ColdFrame.Serialization.Base'Class, using port 40673.

with Ada.Exceptions;
with Ada.Text_IO;             --  need Flush
with ColdFrame.Project.Serialization;
with GNAT.IO; use GNAT.IO;
with GNAT.Sockets;

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

   GNAT.IO.Set_Output (GNAT.IO.Standard_Error);

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

   loop

      --  wait for a connection ..
      GNAT.Sockets.Accept_Socket (Server_Socket, Socket, Connected_Address);

      --  .. got it, get the stream.
      Channel := GNAT.Sockets.Stream (Socket, Connected_Address);

      loop
         --  until an error

         begin

            Put_Line ("about to read ..");
            declare
               Rec : constant ColdFrame.Project.Serialization.Base'Class :=
                 ColdFrame.Project.Serialization.Base'Class'Input (Channel);
            begin
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Output,
                  ColdFrame.Project.Serialization.Image (Rec));
               Ada.Text_IO.Flush;
            end;

         exception
            when E : others =>
               Put_Line (Ada.Exceptions.Exception_Information (E));
               exit;
         end;

      end loop;

      --  something went wrong, close and restart.
      GNAT.Sockets.Close_Socket (Socket);
      delay 1.0;  -- to give us time to C-c out

   end loop;

end Server;
