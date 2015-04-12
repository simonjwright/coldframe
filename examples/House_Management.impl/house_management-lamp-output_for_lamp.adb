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

--  Maps the Lamp to the corresponding Digital_IO output pin.

separate (House_Management.Lamp)
function Output_For_Lamp
  (This : not null Handle)
  return Output_Signal is

   --  The Output_Signals used are in reverse order, because of a
   --  foible of the electrician.
   Lamp_Output_Signal : constant array (Lamp_Name) of Output_Signal
     := (Basement     => 3,
         Ground_Floor => 2,
         First_Floor  => 1,
         Second_Floor => 0);

begin
   return Lamp_Output_Signal (This.Name);
end Output_For_Lamp;
