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

--  Delete the current instance and create a new one; which should be
--  in the Initial state.

separate (Regressions.Phoenix)
procedure Reincarnate
  (This : Handle) is
   Old_One : Handle := This;
   New_One : Handle;
   pragma Unreferenced (New_One);
begin
   Delete (Old_One);
   New_One := Create;
end Reincarnate;
