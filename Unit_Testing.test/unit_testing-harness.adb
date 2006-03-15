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

--  $RCSfile: unit_testing-harness.adb,v $
--  $Revision: 40949c6d745f $
--  $Date: 2006/03/15 20:17:38 $
--  $Author: simonjwright $

with AUnit.Test_Runner;

with Unit_Testing.Suite;

procedure Unit_Testing.Harness is

   procedure Run is new AUnit.Test_Runner (Unit_Testing.Suite.Suite);

begin

   Run;

end Unit_Testing.Harness;
