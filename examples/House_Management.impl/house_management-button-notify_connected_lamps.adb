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

--  Called to indicate to connected Lamps that the Button's state has
--  changed.

with House_Management.A1;
with House_Management.Lamp;

separate (House_Management.Button)
procedure Notify_Connected_Lamps
  (This : not null Handle) is
   Lamps : constant Lamp.Vectors.Vector := A1.Is_Controlled_By (This);
begin
   for L of Lamps loop
      Lamp.Changed (L);
   end loop;
end Notify_Connected_Lamps;
