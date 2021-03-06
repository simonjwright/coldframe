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
with STM32F40x.GPIO;   use STM32F40x.GPIO;
with STM32F40x.RCC;    use STM32F40x.RCC;

package body Digital_IO.STM32F4_Support is

   package GPIO_Configuration is

      type EXTI_Line is mod 16;
      --  so named in case we need to use interrupts; not practical if
      --  we're looking for both falling and rising edges, because of
      --  too-rapid changes.

      type Used_Input_Signal (Valid : Boolean := False) is record
         case Valid is
            when False => null;
            when True  => Signal : Input_Signal;
         end case;
      end record;
      Line_To_Input_Signal :
        constant array (EXTI_Line) of Used_Input_Signal :=
        (0      => (True, 0),
         others => <>);

      type GPIO_Access is access all GPIO_Peripheral;

      type GPIO_Index is (A, B, C, D, E, F, G, H, I)
      with
        Warnings => Off;
      --  Note, 'Pos is the bit number in AHB1ENR, and the number to
      --  set in EXTICR to associate the Line with the GPIO.

      GPIOs : constant array (GPIO_Index) of GPIO_Access :=
        (GPIOA_Periph'Access,
         GPIOB_Periph'Access,
         GPIOC_Periph'Access,
         GPIOD_Periph'Access,
         GPIOE_Periph'Access,
         GPIOF_Periph'Access,
         GPIOG_Periph'Access,
         GPIOH_Periph'Access,
         GPIOI_Periph'Access);

      type Used_Line (Valid : Boolean := False) is record
         case Valid is
            when False =>
               null;
            when True  =>
               GPIO : GPIO_Index;
               Line : EXTI_Line;
         end case;
      end record;

      Input_Signal_To_Line : constant array (Input_Signal) of Used_Line :=
        (0      => (True, A, 0),  -- user button
         others => <>);

      Output_Signal_To_Line : constant array (Output_Signal) of Used_Line :=
        (0      => (True, D, 14),  -- red
         1      => (True, D, 12),  -- green
         2      => (True, D, 15),  -- blue
         3      => (True, D, 13),  -- orange
         others => <>);

      --  How often to check for input change
      Debounce_Interval : constant Ada.Real_Time.Time_Span
        := Ada.Real_Time.Milliseconds (5);

      --  Number of successive inputs that must be the same to
      --  recognise a change
      Debounce_Count : constant := 8;

   end GPIO_Configuration;
   use GPIO_Configuration;

   task Heartbeat;
   --  Flash the green LED

   task Debounce_Inputs;

   procedure Configure_Inputs;
   procedure Configure_Outputs;
   procedure Enable_GPIO (Which : GPIO_Index);
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
      Inputs : array (Input_Signal_To_Line'Range) of Input_Debouncing;
      Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      use type Ada.Real_Time.Time;
   begin
      loop
         Next := Next + Debounce_Interval;
         delay until Next;
         for L in Inputs'Range loop
            if Input_Signal_To_Line (L).Valid then
               declare
                  Signal : constant Input_Signal
                    := Line_To_Input_Signal (EXTI_Line (L)).Signal;
               begin
                  Inputs (L).Index := Inputs (L).Index + 1;
                  Inputs (L).Inputs (Inputs (L).Index)
                    := Get_Input_State
                      (Digital_IO_Support.Input_Signal (Signal));
                  if Inputs (L).Inputs = Successive_Inputs'(others => True)
                    and Inputs (L).Debounced = False
                  then
                     Inputs (L).Debounced := True;
                     Input_Signal_State_Callback.Call_Callbacks
                       ((S     => Signal,
                         State => True));
                  elsif Inputs (L).Inputs = Successive_Inputs'(others => False)
                    and Inputs (L).Debounced = True
                  then
                     Inputs (L).Debounced := False;
                     Input_Signal_State_Callback.Call_Callbacks
                       ((S     => Signal,
                         State => False));
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Debounce_Inputs;

   task body Heartbeat is
      use type Ada.Real_Time.Time;
   begin

      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         Set_Output_State (1, False);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         Set_Output_State (1, True);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         Set_Output_State (1, False);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
         Set_Output_State (1, True);
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
   begin
      for J in Input_Signal'Range loop
         if Input_Signal_To_Line (J).Valid then
            Enable_GPIO (Input_Signal_To_Line (J).GPIO);

            declare
               Which_GPIO : constant GPIO_Index
                 := Input_Signal_To_Line (J).GPIO;
               GPIO : GPIO_Peripheral renames
                 GPIOs (Which_GPIO).all;
               Line : constant Natural :=
                 Natural (Input_Signal_To_Line (J).Line);
            begin
               --  Don't need to configure the line as an input, because
               --  that's the default.

               --  Don't need to configure OTYPER, because the pin is an
               --  input.

               --  Don't need to configure OSPEEDR, because the pin is an
               --  input.

               --  Configure PUPDR for neither pull-up nor -down (the
               --  user pushbutton has an internal pull-down
               --  resistor). 0 => released, 1 => pushed.
               GPIO.PUPDR.Arr (Line) := 0;
            end;
         end if;
      end loop;
   end Configure_Inputs;

   procedure Configure_Outputs is
   begin
      for J in Output_Signal'Range loop
         if Output_Signal_To_Line (J).Valid then
            Enable_GPIO (Output_Signal_To_Line (J).GPIO);

            --  Configure the line as an output.
            declare
               GPIO : GPIO_Peripheral renames
                 GPIOs (Output_Signal_To_Line (J).GPIO).all;
               Line : constant Natural :=
                 Natural (Output_Signal_To_Line (J).Line);
            begin
               GPIO.MODER.Arr (Line) := 1;
            end;

            --  Don't need to configure OTYPER, because the default is
            --  push-pull.

            --  Don't need to configure OSPEEDR, because the default
            --  is slow, which is (I expect) more than adequate for an
            --  LED!

            --  Don't need to configure PUPDR, because the default is
            --  no pull-up, no pull-down.
         end if;
      end loop;
   end Configure_Outputs;

   procedure Enable_GPIO (Which : GPIO_Index) is
   begin
      case Which is
         when A => RCC_Periph.AHB1ENR.GPIOAEN := 1;
         when B => RCC_Periph.AHB1ENR.GPIOBEN := 1;
         when C => RCC_Periph.AHB1ENR.GPIOCEN := 1;
         when D => RCC_Periph.AHB1ENR.GPIODEN := 1;
         when E => RCC_Periph.AHB1ENR.GPIOEEN := 1;
         when F => RCC_Periph.AHB1ENR.GPIOFEN := 1;
         when G => RCC_Periph.AHB1ENR.GPIOGEN := 1;
         when H => RCC_Periph.AHB1ENR.GPIOHEN := 1;
         when I => RCC_Periph.AHB1ENR.GPIOIEN := 1;
      end case;
   end Enable_GPIO;


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
   begin
      if Input_Signal_To_Line (Input_Signal (For_Input)).Valid then
         declare
            Line : constant Natural :=
              Natural (Input_Signal_To_Line (Input_Signal (For_Input)).Line);
            GPIO : GPIO_Peripheral renames
              GPIOs (Input_Signal_To_Line (Input_Signal (For_Input)).GPIO).all;
            use type STM32F40x.Bit;
         begin
            --  The 'pushed' state is logic 1, because we're pulled down.
            --  XXX this should be parameterised!
            return GPIO.IDR.IDR.Arr (Line) /= 0;
         end;
      else
         return False;
      end if;
   end Get_Input_State;

   procedure Set_Output_State
     (For_Output : Digital_IO_Support.Output_Signal;
      To         : Boolean)
   is
   begin
      if Output_Signal_To_Line (Output_Signal (For_Output)).Valid then
         declare
            Line : constant Natural :=
              Natural (Output_Signal_To_Line
                         (Output_Signal (For_Output)).Line);
            GPIO : GPIO_Peripheral renames
              GPIOs (Output_Signal_To_Line
                       (Output_Signal (For_Output)).GPIO).all;
         begin
            if To then
               GPIO.BSRR.BS.Arr (Line) := 1;
            else
               GPIO.BSRR.BR.Arr (Line) := 1;
            end if;
         end;
      end if;
   end Set_Output_State;

end Digital_IO.STM32F4_Support;
