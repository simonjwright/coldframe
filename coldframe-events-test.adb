--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $RCSfile: coldframe-events-test.adb,v $
--  $Revision: 3daa7f1e04bd $
--  $Date: 2002/02/03 10:26:21 $
--  $Author: simon $

with ColdFrame.States.Test_Support;

procedure ColdFrame.States.Test is

   Ins : aliased Test_Support.Instance;

   E : Event_P;

begin

   E := new Test_Support.Ev (Ins'Unchecked_Access);
   States.Set (The => Test_Support.T1,
               On => Test_Support.Dispatcher,
               To_Fire => E,
               After => 1.0);

   E := new Test_Support.Ev (Ins'Unchecked_Access);
   States.Set (The => Test_Support.T2,
               On => Test_Support.Dispatcher,
               To_Fire => E,
               After => 2.0);

   delay 4.0;

end ColdFrame.States.Test;
