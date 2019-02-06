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

with Ada.Real_Time;
with Digital_IO.Input_Signal_State_Callback;
with LEDs;
with nrf51.GPIO;

package body Digital_IO.Microbit_Support is

   --  Buttons.
   --
   --  Button A is input signal 0.

   type Button_Rep is (Button_A, Button_B);
   for Button_Rep use (Button_A => 17, Button_B => 26);

   type Input_Pin_Mapping
     is array (Digital_IO_Support.Input_Signal range <>) of Button_Rep;

   Input_Pin_Map : constant Input_Pin_Mapping
     := (0 => Button_A);

   subtype Used_Input_Signal
     is Digital_IO_Support.Input_Signal range Input_Pin_Map'Range;

   --  Output signals:
   --
   --  Signal 0 is LED (3, 2).
   --
   --  Heartbeat is LED (3, 4) (need to be careful of microbit LED
   --    control here, because of the clumsy way the r/c controls are
   --    organised; without careful choice, other LEDs get
   --    accidentally set when both of these two LEDs are set).

   type Output_Pin_Data is record
      Row : LEDs.Coord;
      Col : LEDs.Coord;
   end record;

   type Output_Pin_Mapping
     is array (Digital_IO_Support.Output_Signal range <>)
     of Output_Pin_Data;

   Output_Pin_Map : constant Output_Pin_Mapping
     := (0 => (3, 2));

   task Heartbeat with Secondary_Stack_Size => 0;
   --  Flash the LED

   --  How often to check for input change. NB, on Microbit, tick is
   --  100 Hz, not the more usual 1000 Hz.
   Debounce_Interval : constant Ada.Real_Time.Time_Span
     := Ada.Real_Time.Milliseconds (10);

   --  Number of successive inputs that must be the same to recognise
   --  a change
   Debounce_Count : constant := 4;

   task Debounce_Inputs
   with Secondary_Stack_Size => 0;

   procedure Configure_Inputs;
   procedure Configure_Outputs;

   task body Heartbeat is
      use type Ada.Real_Time.Time;
      Row : constant LEDs.Coord := 3;
      Col : constant LEDs.Coord := 4;
   begin
      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         LEDs.Set (Row, Col, To => True);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         LEDs.Set (Row, Col, To => False);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         LEDs.Set (Row, Col, To => True);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         LEDs.Set (Row, Col, To => False);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
      end loop;
   end Heartbeat;

   task body Debounce_Inputs is
      type Debounce_Index is mod Debounce_Count;
      type Successive_Inputs is array (Debounce_Index) of Boolean
      with Default_Component_Value => False;
      type Input_Debouncing is record
         Inputs : Successive_Inputs;
         Index : Debounce_Index := Debounce_Index'First;
         Debounced : Boolean := False;
      end record;
      Inputs : array (Used_Input_Signal) of Input_Debouncing;
      Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      use type Ada.Real_Time.Time;
   begin
      loop
         Next := Next + Debounce_Interval;
         delay until Next;
         for L in Inputs'Range loop
            declare
               use nrf51.GPIO;
               State : constant Boolean
                 :=  GPIO_Periph.IN_k.Arr (Input_Pin_Map (L)'Enum_Rep) = Low;
            begin
               Inputs (L).Index := Inputs (L).Index + 1;
               Inputs (L).Inputs (Inputs (L).Index) := State;
               if Inputs (L).Inputs = Successive_Inputs'(others => True)
                 and Inputs (L).Debounced = False
               then
                  Inputs (L).Debounced := True;
                  Input_Signal_State_Callback.Call_Callbacks
                    ((S     => Integer (L),
                      State => True));
               elsif Inputs (L).Inputs = Successive_Inputs'(others => False)
                 and Inputs (L).Debounced = True
               then
                  Inputs (L).Debounced := False;
                  Input_Signal_State_Callback.Call_Callbacks
                    ((S     => Integer (L),
                      State => False));
               end if;
            end;
         end loop;
      end loop;
   end Debounce_Inputs;

   procedure Initialize is
   begin
      Configure_Inputs;
      Configure_Outputs;
      Register (new Implementation);
   end Initialize;

   procedure Configure_Inputs is
      use nrf51.GPIO;
   begin
      --  Buttons
      GPIO_Periph.PIN_CNF (Button_A'Enum_Rep) := (DIR    => Input,
                                                  INPUT  => Connect,
                                                  PULL   => Pullup,
                                                  SENSE  => Low,
                                                  others => <>);
      GPIO_Periph.PIN_CNF (Button_B'Enum_Rep) := (DIR    => Input,
                                                  INPUT  => Connect,
                                                  PULL   => Pullup,
                                                  SENSE  => Low,
                                                  others => <>);
   end Configure_Inputs;

   procedure Configure_Outputs is
   begin
      LEDs.Initialize;
   end Configure_Outputs;


   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This); -- only used for dispatching
      use nrf51.GPIO;
   begin
      if For_Input in Input_Pin_Map'Range then
         return
           GPIO_Periph.IN_k.Arr (Input_Pin_Map (For_Input)'Enum_Rep) = Low;
      else
         return False;
      end if;
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
   begin
      if For_Output in Output_Pin_Map'Range then
         LEDs.Set (At_Row => Output_Pin_Map (For_Output).Row,
                   At_Col => Output_Pin_Map (For_Output).Col,
                   To     => To);
      end if;
   end Set;

end Digital_IO.Microbit_Support;
