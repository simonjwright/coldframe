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

--  $RCSfile: digital_io-raspi_support.adb,v $
--  $Revision: 06443f39bb05 $
--  $Date: 2014/04/06 17:20:21 $
--  $Author: simonjwright $

with Ada.Text_IO; use Ada.Text_IO;
with Digital_IO.Input_Signal_State_Callback;
with I2C.MCP23017.Polling;

package body Digital_IO.RasPi_Support is

   Bus : aliased I2C.Bus (Address => 0);
   Chip : I2C.MCP23017.Polling.Chip (Address => 16#20#,
                                     On_Bus => Bus'Access);

   --  GPB pins are used as inputs
   subtype Input_Pin is I2C.MCP23017.GPIO_Pin
     range I2C.MCP23017.B0 .. I2C.MCP23017.B7;

   --  GPA pins are used as outputs
   subtype Output_Pin is I2C.MCP23017.GPIO_Pin
     range I2C.MCP23017.A0 .. I2C.MCP23017.A7;

   procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean);

   procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean)
   is
      Pin_Map : constant array (Input_Pin) of Input_Signal
        := (I2C.MCP23017.B0 => 0,
            I2C.MCP23017.B1 => 1,
            I2C.MCP23017.B2 => 2,
            I2C.MCP23017.B3 => 3,
            I2C.MCP23017.B4 => 4,
            I2C.MCP23017.B5 => 5,
            I2C.MCP23017.B6 => 6,
            I2C.MCP23017.B7 => 7);
   begin
      Put_Line ("Pin " & Pin_Changed'Img & " changed: " & To'Img);
      if Pin_Changed not in Input_Pin then
         raise Program_Error with "pin changed isn't an input pin";
      end if;
      if Pin_Map (Pin_Changed) in 0 .. 3 then
         Input_Signal_State_Callback.Call_Callbacks
           ((S => Pin_Map (Pin_Changed),
             State => To));
      end if;
   end Callback;

   procedure Initialize
   is
   begin
      Chip.Reset;

      Chip.Configure (Connect_Interrupt_Pins => True,
                   Interrupt_Pins_Open_Drain => False,
                   Interrupt_Pins_Active_High => True);

      for J in Input_Pin loop
         Chip.Configure (Pin => J,
                         As_Input => True,
                         Normal_Input_Polarity => False,
                         Pullup_Enabled => True);
      end loop;

      for J in Output_Pin loop
         Chip.Configure (Pin => J, As_Input => False);
      end loop;

      Register (new Implementation);

      Chip.Register_Callback (Callback'Unrestricted_Access);

   end Initialize;

   procedure Set (This : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      subtype Usable_Output_Signal
         is Digital_IO_Support.Output_Signal range 0 .. 7;
      Pin_Map : constant array (Usable_Output_Signal) of Output_Pin
        := (0 => I2C.MCP23017.A0,
            1 => I2C.MCP23017.A1,
            2 => I2C.MCP23017.A2,
            3 => I2C.MCP23017.A3,
            4 => I2C.MCP23017.A4,
            5 => I2C.MCP23017.A5,
            6 => I2C.MCP23017.A6,
            7 => I2C.MCP23017.A7);
   begin
      Put_Line ("Output " & For_Output'Img & " changed: " & To'Img);
      if For_Output not in Usable_Output_Signal then
         raise Program_Error with "output signal not a usable output pin";
      end if;
      Chip.Write (Pin_Map (For_Output), To);
   end Set;

end Digital_IO.RasPi_Support;
