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

--  $Id: library_test_harness.adb,v f4fe3a74c81e 2001/09/04 05:20:33 simon $

with AUnit.Test_Runner;
with Library.Test;

procedure Library_Test_Harness is

   procedure Run is new AUnit.Test_Runner (Library.Test);

begin

   Run (Timed => False);

end Library_Test_Harness;
