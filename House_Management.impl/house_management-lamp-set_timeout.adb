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

--  $RCSfile: house_management-lamp-set_timeout.adb,v $
--  $Revision: 0f7eda971a8e $
--  $Date: 2003/02/07 05:55:57 $
--  $Author: simon $

--  This state entry action sets the instance Timeout to the required
--  activation period.

with ColdFrame.Project.Events;
with House_Management.Events;

separate (House_Management.Lamp)
procedure Set_Timeout
  (This : Handle) is
begin

   ColdFrame.Project.Events.Set (The_Timer => This.Timeout,
                                 On => Events.Dispatcher,
                                 To_Fire => new Timeout (This),
                                 After => 5.0);

end Set_Timeout;
