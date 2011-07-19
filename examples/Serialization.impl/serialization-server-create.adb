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

--  $RCSfile: serialization-server-create.adb,v $
--  $Revision: 0521d114696b $
--  $Date: 2011/07/19 17:48:15 $
--  $Author: simonjwright $

--  Create an instance of Server, transmitting data to the remote
--  server on host "Connecting To Host" using port "Using Port". On
--  successful return, the TCP/IP connection has been made.

separate (Serialization.Server)
procedure Create
  (Connecting_To_Host : String;
   Using_Port : Port) is

   H : Handle;
   Address : GNAT.Sockets.Sock_Addr_Type;

begin

   pragma Warnings (Off, "explicit initialization is no longer required");
   GNAT.Sockets.Initialize;
   pragma Warnings (On, "explicit initialization is no longer required");

   H := Create;

   GNAT.Sockets.Create_Socket (H.Sock,
                               GNAT.Sockets.Family_Inet,
                               GNAT.Sockets.Socket_Stream);
   GNAT.Sockets.Set_Socket_Option (H.Sock,
                                   GNAT.Sockets.Socket_Level,
                                   (GNAT.Sockets.Reuse_Address, True));
   Address.Addr := GNAT.Sockets.Addresses
     (GNAT.Sockets.Get_Host_By_Name (Connecting_To_Host), 1);
   Address.Port := Using_Port;
   GNAT.Sockets.Connect_Socket (H.Sock, Address);
   H.Channel := GNAT.Sockets.Stream (H.Sock, Address);

end Create;
