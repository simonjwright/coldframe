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

--  $RCSfile: coldframe-events-test_support.adb,v $
--  $Revision: fa4d5d083322 $
--  $Date: 2002/02/20 20:23:57 $
--  $Author: simon $

package body ColdFrame.Events.Test_Support is


   function State_Image (This : Instance) return String is
   begin
      return "<none>";
   end State_Image;


   procedure Handler (This : Quiet_Ev) is
   begin
      null;
   end Handler;

   procedure Handler (This : Noisy_Ev) is
   begin
      delay 0.5;
      Events.Unset (The => T2,
                    On => Noisy_Dispatcher);
   end Handler;

end ColdFrame.Events.Test_Support;
