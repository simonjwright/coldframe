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

--  Handles Button Events, which occur when a button has been pushed.

--  Tell the associated Lamps.

with House_Management.Lamp.Collections;
with House_Management.Lamp.Iterate;
with House_Management.A1;

separate (House_Management.Button)
procedure Pushed
  (E : Button_Event) is

   procedure Button_Pushed (L : Lamp.Handle);
   pragma Inline (Button_Pushed);
   procedure Process is new Lamp.Iterate (Button_Pushed);
   procedure Button_Pushed (L : Lamp.Handle) is
      Ev : Lamp.Button_Push (For_The_Instance => L);
   begin
      Lamp.Handler (Ev);
   end Button_Pushed;

   BH : constant Handle := Find ((Name => E.Payload));
   LHS : constant Lamp.Collections.Collection
     := A1.Is_Controlled_By (BH);

begin
   Process (LHS);
end Pushed;
