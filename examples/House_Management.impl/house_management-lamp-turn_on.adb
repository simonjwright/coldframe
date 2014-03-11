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

--  $RCSfile: house_management-lamp-turn_on.adb,v $
--  $Revision: e08cb16c3dfb $
--  $Date: 2014/03/11 18:27:45 $
--  $Author: simonjwright $

--  This state entry action turns on the associated signal via Digital
--  IO.

with Digital_IO.Application;

separate (House_Management.Lamp)
procedure Turn_On
  (This : not null Handle) is

   Signals : constant array (Lamp_Name) of Digital_IO.Signal_Name
     := (Second_Floor => Digital_IO.Lamp_A,
         First_Floor => Digital_IO.Lamp_B,
         Ground_Floor => Digital_IO.Lamp_C,
         Basement => Digital_IO.Lamp_D);

begin

   Digital_IO.Application.Set_Output (Signals (This.Name), To_State => True);

end Turn_On;
