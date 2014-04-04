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
--  $Revision: 4ee79f54b785 $
--  $Date: 2014/04/04 12:46:49 $
--  $Author: simonjwright $

--  This state entry action turns on the associated signal via Digital
--  IO.

with Digital_IO;

separate (House_Management.Lamp)
procedure Turn_On
  (This : not null Handle) is
begin
   Digital_IO.Set (This.Output, To_State => True);
end Turn_On;
