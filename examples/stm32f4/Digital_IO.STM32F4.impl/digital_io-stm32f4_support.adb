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

--  The debouncing code here (task Debounce_Inputs) was inspired by
--  Jack Ganssle's paper "A Guide to Debouncing" at
--  https://pubweb.eng.utah.edu/~cs5780/debouncing.pdf.

pragma Warnings (Off, "referenced");

with Ada.Real_Time;
with Digital_IO.Input_Signal_State_Callback;
with STM32.Device;
with STM32.GPIO;

package body Digital_IO.STM32F4_Support is

   type LED is (Green, Orange, Red, Blue);
   for LED use (Green  => 12,
                Orange => 13,
                Red    => 14,
                Blue   => 15);

   LEDs : STM32.GPIO.GPIO_Points
     := (Green'Enum_Rep  => (Periph => STM32.Device.GPIO_D'Access,
                             Pin => STM32.GPIO.Pin_12),
         Orange'Enum_Rep => (Periph => STM32.Device.GPIO_D'Access,
                             Pin => STM32.GPIO.Pin_13),
         Red'Enum_Rep    => (Periph => STM32.Device.GPIO_D'Access,
                             Pin => STM32.GPIO.Pin_14),
         Blue'Enum_Rep   => (Periph => STM32.Device.GPIO_D'Access,
                             Pin => STM32.GPIO.Pin_15));

   --  There's only one Lamp and one Button
   Lamp : STM32.GPIO.GPIO_Point := LEDs (Red'Enum_Rep);
   Button : constant STM32.GPIO.GPIO_Point :=
     (Periph => STM32.Device.GPIO_A'Access,
      Pin    => STM32.GPIO.Pin_0);
   Input_Signal_For_Button : constant := 0;

   task Heartbeat;
   --  Flash the green LED

   --  How often to check for input change
   Debounce_Interval : constant Ada.Real_Time.Time_Span
     := Ada.Real_Time.Milliseconds (5);

   --  Number of successive inputs that must be the same to
   --  recognise a change
   Debounce_Count : constant := 8;

   task Debounce_Inputs;

   procedure Configure_Inputs;
   procedure Configure_Outputs;

   function Get_Input_State
     (For_Input : Digital_IO_Support.Input_Signal)
     return Boolean;
   procedure Set_Output_State
     (For_Output : Digital_IO_Support.Output_Signal;
      To         : Boolean);

   task body Debounce_Inputs is
      type Debounce_Index is mod Debounce_Count;
      type Successive_Inputs is array (Debounce_Index) of Boolean
      with Default_Component_Value => False;
      type Input_Debouncing is record
         Inputs : Successive_Inputs;
         Index : Debounce_Index := Debounce_Index'First;
         Debounced : Boolean := False;
      end record;
      Button_Input : Input_Debouncing; -- one button
      Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      use type Ada.Real_Time.Time;
   begin
      loop
         Next := Next + Debounce_Interval;
         delay until Next;

         Button_Input.Index := Button_Input.Index + 1;
         Button_Input.Inputs (Button_Input.Index)
           := Button.Set;
         if Button_Input.Inputs = Successive_Inputs'(others => True)
           and Button_Input.Debounced = False
         then
            Button_Input.Debounced := True;
            Input_Signal_State_Callback.Call_Callbacks
              ((S     => Input_Signal_For_Button,
                State => True));
         elsif Button_Input.Inputs = Successive_Inputs'(others => False)
           and Button_Input.Debounced = True
         then
            Button_Input.Debounced := False;
            Input_Signal_State_Callback.Call_Callbacks
              ((S     => Input_Signal_For_Button,
                State => False));
         end if;
      end loop;
   end Debounce_Inputs;

   task body Heartbeat is
      use type Ada.Real_Time.Time;
   begin

      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         STM32.GPIO.Clear (LEDs (Green'Enum_Rep));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         STM32.GPIO.Set (LEDs (Green'Enum_Rep));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         STM32.GPIO.Clear (LEDs (Green'Enum_Rep));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
         STM32.GPIO.Set (LEDs (Green'Enum_Rep));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;
   end Heartbeat;

   procedure Initialize is
   begin
      Configure_Inputs;
      Configure_Outputs;
      Register (new Implementation);
   end Initialize;

   procedure Configure_Inputs is
      use STM32.Device;
      use STM32.GPIO;
   begin
      Enable_Clock (Button.Periph.all);
      Configure_IO (Button,
                    Config => (Mode      => Mode_In,
                               Resistors => Pull_Down));
   end Configure_Inputs;

   procedure Configure_Outputs is
      use STM32.Device;
      use STM32.GPIO;
   begin
      for L of LEDs loop
         Enable_Clock (L.Periph.all);  -- overkill!
      end loop;
      Configure_IO (LEDs,
                    Config => (Mode      => Mode_Out,
                               Resistors => Pull_Up,
                               others    => <>));
      Clear (LEDs);
   end Configure_Outputs;

   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
   begin
      return Get_Input_State (For_Input);
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
   begin
      Set_Output_State (For_Output, To);
   end Set;

   function Get_Input_State
     (For_Input : Digital_IO_Support.Input_Signal)
     return Boolean
   is
      use type Digital_IO_Support.Input_Signal;
      pragma Assert (For_Input = 0, "only input 0 supported");
   begin
      return Button.Set;
   end Get_Input_State;

   procedure Set_Output_State
     (For_Output : Digital_IO_Support.Output_Signal;
      To         : Boolean)
   is
      use type Digital_IO_Support.Output_Signal;
   begin
      --  We're only controlling the one Lamp, but initialization goes
      --  wild and clears all 0 .. 15 possibilities.
      if For_Output /= 0 then
         return;
      end if;

      if To then
         LEDs (Red'Enum_Rep).Set;
      else
         LEDs (Red'Enum_Rep).Clear;
      end if;
   end Set_Output_State;


end Digital_IO.STM32F4_Support;
