--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with States.Events;

separate (States.Monitor)
procedure Set_2_Second_Warmup_Timeout (This : Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Warmup_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => new Warmup_Timeout (This),
                                 After => 2.0);
end Set_2_Second_Warmup_Timeout;
