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
with nRF.GPIO;

package body Digital_IO.Microbit_Support is

   --  Output signals:
   --
   --  Signal 0 is the central LED (3, 3).

   --  The LEDs are driven as a 3-row, 9-column matrix, addressed by
   --  row/column (i.e., to set LED 1.1, set row 1, column 1 each to
   --  High).

   --  See https://www.iot-programmer.com/index.php/books/\
   --  27-micro-bit-iot-in-c/chapters-micro-bit-iot-in-c/\
   --  54-micro-bit-iot-in-c-the-led-display

   type Row_Pin is (R1, R2, R3);
   for Row_Pin use (R1 => 13, R2 => 14, R3 => 15);

   type Col_Pin is (C1, C2, C3, C4, C5, C6, C7, C8, C9);
   for Col_Pin use (C1 => 4, C2 => 5, C3 => 6, C4 => 7, C5 => 8,
                    C6 => 9, C7 => 10, C8 => 11, C9 => 12);

   type LED_Pins is record
      R : Row_Pin;
      C : Col_Pin;
   end record;

   --  The LEDs are displayed in a (5, 5) array.
   type Coord is range 1 .. 5;
   type LED_Array is array (Coord, Coord) of LED_Pins;

   LEDs : constant LED_Array :=
     (((R1, C1), (R2, C4), (R1, C2), (R2, C5), (R1, C3)),
      ((R3, C4), (R3, C5), (R3, C6), (R3, C7), (R3, C8)),
      ((R2, C2), (R1, C9), (R2, C3), (R3, C9), (R2, C1)),
      ((R1, C8), (R1, C7), (R1, C6), (R1, C5), (R1, C4)),
      ((R3, C3), (R2, C7), (R3, C1), (R2, C6), (R3, C2)));

   type LED_Point is record
      R : nRF.GPIO.GPIO_Point;
      C : nRF.GPIO.GPIO_Point;
   end record;
   type LED_Point_Array is array (Coord, Coord) of LED_Point;
   --  Easier to initialize in code!
   --  The GPIO_Points need to be writable, because Set/Clear take them
   --  as in out parameters.
   LED_Points : LED_Point_Array;

   type Output_Pin_Data is record
      Row : Coord;
      Col : Coord;
   end record;

   type Output_Pin_Mapping
     is array (Digital_IO_Support.Output_Signal range <>)
     of Output_Pin_Data;

   Output_Pin_Map : constant Output_Pin_Mapping
     := (0 => (3, 3));

   procedure Configure_Outputs;

   --  Input signals:
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

   procedure Configure_Inputs;

   task Heartbeat with Secondary_Stack_Size => 0;

   --  Debouncing

   --  How often to check for input change.
   Debounce_Interval : constant Ada.Real_Time.Time_Span
     := Ada.Real_Time.Milliseconds (5);

   --  Number of successive inputs that must be the same to recognise
   --  a change
   Debounce_Count : constant := 4;

   task Debounce_Inputs with Secondary_Stack_Size => 0;

   task body Heartbeat is
      use nRF.GPIO;
      use type Ada.Real_Time.Time;
   begin
      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         Set (LED_Points (5, 1).R);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         Clear (LED_Points (5, 1).R);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         Set (LED_Points (5, 1).R);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         Clear (LED_Points (5, 1).R);
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
               use nRF.GPIO;
               State : constant Boolean
                 :=  not Set (GPIO_Point'(Pin => Input_Pin_Map (L)'Enum_Rep));
               --  the pin will be low, i.e. False, when pushed.
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

   procedure Configure_Inputs is
      use nRF.GPIO;
      Config : constant GPIO_Configuration :=
        (Mode         => Mode_In,
         Resistors    => Pull_Up,
         Input_Buffer => Input_Buffer_Connect,
         Sense        => Sense_For_Low_Level,
         others       => <>);
   begin
      --  Buttons
      Configure_IO (GPIO_Point'(Pin => Button_A'Enum_Rep), Config);
      Configure_IO (GPIO_Point'(Pin => Button_B'Enum_Rep), Config);
   end Configure_Inputs;

   procedure Configure_Outputs is
      use nRF.GPIO;
      Config : constant GPIO_Configuration :=
        (Mode         => Mode_Out,
         Resistors    => Pull_Up,
         others       => <>);
      Point : GPIO_Point;
   begin
      --  Set all LEDs to off (r => 0, c => 1)
      for Pin in Row_Pin loop
         Point := GPIO_Point'(Pin => Pin'Enum_Rep);
         Clear (Point);
      end loop;
      for Pin in Col_Pin loop
         Point := GPIO_Point'(Pin => Pin'Enum_Rep);
         Set (Point);
      end loop;

      --  Set C3 to 0, so that we can control the LEDs using just R
      Point := GPIO_Point'(Pin => C3'Enum_Rep);
      Clear (Point);

      for Row in LEDs'Range (1) loop
         for Col in LEDs'Range (2) loop
            LED_Points (Row, Col).R :=
              GPIO_Point'(Pin => LEDs (Row, Col).R'Enum_Rep);
            LED_Points (Row, Col).C :=
              GPIO_Point'(Pin => LEDs (Row, Col).C'Enum_Rep);
         end loop;
      end loop;

      for Row in LED_Points'Range (1) loop
         for Col in LED_Points'Range (2) loop
            Configure_IO (LED_Points (Row, Col).R, Config);
            Configure_IO (LED_Points (Row, Col).C, Config);
         end loop;
      end loop;
   end Configure_Outputs;

   --  Implementations of externally-promised subprograms

   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This); -- only used for dispatching
      use nRF.GPIO;
   begin
      if For_Input in Input_Pin_Map'Range then
         return
           not Set (GPIO_Point'(Pin => Input_Pin_Map (For_Input)'Enum_Rep));
         --  Pushed button pulls the signal low, i.e. False
      else
         return False;
      end if;
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      use nRF.GPIO;
   begin
      if For_Output in Output_Pin_Map'Range then
         if To then
            Set (LED_Points (3, 3).R);
         else
            Clear (LED_Points (3, 3).R);
         end if;
      end if;
   end Set;

   procedure Initialize is
   begin
      Configure_Inputs;
      Configure_Outputs;
      Register (new Implementation);
   end Initialize;

end Digital_IO.Microbit_Support;
