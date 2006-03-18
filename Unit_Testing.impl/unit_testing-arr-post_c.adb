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

pragma Style_Checks (Off);
--  $Id: unit_testing-arr-post_c.adb,v 94977717b3f9 2006/03/18 17:49:18 simonjwright $
pragma Style_Checks (On);

with ColdFrame.Project.Events;
with Unit_Testing.Events;

separate (Unit_Testing.Arr)
procedure Post_C is
begin
   ColdFrame.Project.Events.Set (S,
                                 On => Events.Dispatcher,
                                 To_Fire => new C,
                                 After => 1.0);
end Post_C;
