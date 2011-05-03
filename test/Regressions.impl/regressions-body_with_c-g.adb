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

separate (Regressions.Body_With_C)
procedure G
  (This : Handle;
   P : ColdFrame.Instances.Handle) is
   pragma Warnings (Off, This);
   pragma Warnings (Off, P);
   A : Body_With_A.Handle;
   B : Body_With_B.Handle;
   M : Body_With_P.Handle;
   pragma Warnings (Off, A);
   pragma Warnings (Off, B);
   pragma Warnings (Off, M);
begin
   null;
end G;
