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

--  $RCSfile: digital_io-signal-initialize.adb,v $
--  $Revision: e52e2300d52d $
--  $Date: 2003/08/30 20:32:01 $
--  $Author: simon $

--  Creates all the Signals.

with Digital_IO.Input;
with Digital_IO.Output;

separate (Digital_IO.Signal)
procedure Initialize is

   SH : Signal.Handle;
   IH : Input.Handle;
   pragma Warnings (Off, IH);
   OH : Output.Handle;
   pragma Warnings (Off, OH);

   subtype CIH is ColdFrame.Instances.Handle;

begin

   SH := Signal.Create ((S => Floor_0));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_1));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_2));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_3));
   IH := Input.Create ((G1_Parent => CIH (SH)));

   SH := Signal.Create ((S => Lamp_A));
   OH := Output.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Lamp_B));
   OH := Output.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Lamp_C));
   OH := Output.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Lamp_D));
   OH := Output.Create ((G1_Parent => CIH (SH)));

end Initialize;
