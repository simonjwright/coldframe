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

--  $RCSfile: house_management-lamp-initialize.adb,v $
--  $Revision: 0f7eda971a8e $
--  $Date: 2003/02/07 05:55:57 $
--  $Author: simon $

--  This operation initializes the Lamps and Buttons, and connects to
--  the Digital IO domaion.

with House_Management.Button;
with House_Management.Button_To_Lamp;
with House_Management.A1;
with Digital_IO.Signal_State_Callback;

separate (House_Management.Lamp)
procedure Initialize is

   LBH, LGH, L1H, L2H : Lamp.Handle;
   BBH, BGH, B1H, B2H : Button.Handle;
   BTLH : Button_To_Lamp.Handle;

begin

   --  The second floor lamp is controlled by the buttons on the first
   --  and second floors.

   --  The first floor lamp is controlled by the buttons on the
   --  ground, first and second floors.

   --  The ground floor lamp is controlled by the buttons in the
   --  basement and on the ground and first floors.

   --  The basement lamp is controlled by the basement button only.

   L2H := Lamp.Create ((Name => Second_Floor));
   L1H := Lamp.Create ((Name => First_Floor));
   LGH := Lamp.Create ((Name => Ground_Floor));
   LBH := Lamp.Create ((Name => Basement));

   B2H := Button.Create ((Name => Second_Floor));
   B1H := Button.Create ((Name => First_Floor));
   BGH := Button.Create ((Name => Ground_Floor));
   BBH := Button.Create ((Name => Basement));

   BTLH := A1.Link (Controls => L2H, Is_Controlled_By => B2H);
   BTLH := A1.Link (Controls => L2H, Is_Controlled_By => B1H);

   BTLH := A1.Link (Controls => L1H, Is_Controlled_By => B2H);
   BTLH := A1.Link (Controls => L1H, Is_Controlled_By => B1H);
   BTLH := A1.Link (Controls => L1H, Is_Controlled_By => BGH);

   BTLH := A1.Link (Controls => LGH, Is_Controlled_By => B1H);
   BTLH := A1.Link (Controls => LGH, Is_Controlled_By => BGH);
   BTLH := A1.Link (Controls => LGH, Is_Controlled_By => BBH);

   BTLH := A1.Link (Controls => LBH, Is_Controlled_By => BBH);

   --  Register for button state changes.
   Digital_IO.Signal_State_Callback.Register (Button.Changed'Access);

end Initialize;
