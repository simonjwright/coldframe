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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with ColdFrame.Project.Events;
with Serialization.Events;
with Serialization.Server;

separate (Serialization.Interface)
procedure Output
  (V : Serializable) is

   SH : constant Server.Handle := Server.Find;

   Ev : ColdFrame.Project.Events.Event_P;

   use type Server.Handle;

begin

   if SH = null then
      raise Use_Error;
   end if;

   Ev := new Server.Posted_Value (SH);
   declare
      --  This suppression avoids a bug in GCC 4.0.0 & GNAT 5.03a.
      pragma Suppress (All_Checks);
   begin
      Serializable'Output
        (Server.Posted_Value (Ev.all).Payload'Unrestricted_Access, V);
   end;
   ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);

end Output;
