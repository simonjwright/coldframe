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

--  Check that we can take 'Access of aliased attributes.

separate (Compilation_Regressions.Aliased_Components)
procedure Check_Access
  (This : Handle) is
   type Integer_P is access all Integer;
   R : Record_With_Aliased_Component := (I => 42);
   B : Aliasable_Boolean_P;
   I : Integer_P;
   pragma Unreferenced (B, I);
begin
   B := This.Ins_B'Access;
   I := This.Ins_I'Access;
   B := Cls_B'Access;
   I := Cls_I'Access;
   I := R.I'Access;
end Check_Access;
