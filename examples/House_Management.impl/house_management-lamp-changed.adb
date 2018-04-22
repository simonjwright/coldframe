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

--  This operation is used by a linked Button to tell the Lamp
--  to evaluate all its linked Buttons to see if any are Set (in which
--  case the Lamp should be lit) or all are Reset (in which case the
--  Lamp should be off).

with Digital_IO;
with House_Management.A1;
with House_Management.Button;

separate (House_Management.Lamp)
procedure Changed
  (This : not null Handle) is
   Buttons : constant Button.Vectors.Vector := A1.Controls (This);
begin
   Digital_IO.Set
     (Output_For_Lamp (This),
      To_State => (for some B of Buttons => Button.Get_State (B)));
end Changed;
