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

with Ada.Interrupts.Names;
with Digital_IO.Input_Signal_State_Callback;
with Interfaces;
with System;
with stm32f429xx_h; use stm32f429xx_h;

package body Digital_IO.STM32F4_Support is

   --  procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean);

   --  procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean)
   --  is
   --     Pin_Map : constant array (Input_Pin) of Input_Signal
   --       := (I2C.MCP23017.B0 => 0,
   --           I2C.MCP23017.B1 => 1,
   --           I2C.MCP23017.B2 => 2,
   --           I2C.MCP23017.B3 => 3,
   --           I2C.MCP23017.B4 => 4,
   --           I2C.MCP23017.B5 => 5,
   --           I2C.MCP23017.B6 => 6,
   --           I2C.MCP23017.B7 => 7);
   --  begin
   --     Put_Line ("Pin " & Pin_Changed'Img & " changed: " & To'Img);
   --     if Pin_Changed not in Input_Pin then
   --        raise Program_Error with "pin changed isn't an input pin";
   --     end if;
   --     if Pin_Map (Pin_Changed) in 0 .. 3 then
   --        Input_Signal_State_Callback.Call_Callbacks
   --          ((S => Pin_Map (Pin_Changed),
   --            State => To));
   --     end if;
   --  end Callback;

   protected type EXTI15_10_IRQ_T
   with
     Interrupt_Priority => System.Interrupt_Priority'First
   is new IRQ_Handler with
      overriding
      entry Button_Pressed
        (B : out Digital_IO_Support.Input_Signal);
   private
      Triggered : Boolean := False;
      Last_Button_Pressed : Digital_IO_Support.Input_Signal;
      procedure Handler;
      pragma Attach_Handler (Handler, Ada.Interrupts.Names.EXTI15_10_IRQ);
   end EXTI15_10_IRQ_T;

   EXTI15_10_IRQ : aliased EXTI15_10_IRQ_T;

   task type Receive_Button_Interrupt
     (IRQ : access IRQ_Handler'Class)
   is
   end Receive_Button_Interrupt;

   Receive_EXTI15_10_IRQ :
     Receive_Button_Interrupt (IRQ => EXTI15_10_IRQ'Access);

   protected body EXTI15_10_IRQ_T is
      entry Button_Pressed
        (B : out Digital_IO_Support.Input_Signal)
        when Triggered is
      begin
         B := Last_Button_Pressed;
         Triggered := False;
      end Button_Pressed;

      procedure Handler is
         use type Interfaces.Unsigned_32;
         PR : Interfaces.Unsigned_32;
      begin
         PR := EXTI.PR;
         if (PR and EXTI_PR_PR11) /= 0 then
            Last_Button_Pressed :=  Digital_IO_Support.Input_Signal'First;
            Triggered := True;
            EXTI.PR := EXTI_PR_PR11;  -- clear the interrupt by *writing* 1
         else
            raise Program_Error with "unexpected interrupt";
         end if;
      end Handler;
   end EXTI15_10_IRQ_T;

   task body Receive_Button_Interrupt is
      Last_Button_Pressed : Digital_IO_Support.Input_Signal;
   begin
      loop
         IRQ.Button_Pressed (Last_Button_Pressed);
         Input_Signal_State_Callback.Call_Callbacks
           ((S => Integer (Last_Button_Pressed),
             State => True));
      end loop;
   end Receive_Button_Interrupt;

   procedure Configure_Inputs;
   procedure Configure_Outputs;

   procedure Initialize is
   begin
      Configure_Inputs;
      Configure_Outputs;
      Register (new Implementation);
   end Initialize;

   procedure Configure_Inputs is
      use type Interfaces.Unsigned_32;
   begin
      --  First, we need to enable GPIOC's clock.
      RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOCEN;

      --  Don't need to configure bit 11 as an input, because that's
      --  the default.

      --  Don't need to configure OTYPER, because the pin is an input.

      --  Don't need to configure OSPEEDR, because the pin is an input.

      --  Configure PUPDR for pull-up (AdaCore have no-pull?)
      declare
         Pull_Register : Interfaces.Unsigned_32;
      begin
         Pull_Register := GPIOC.PUPDR;
         Pull_Register := Pull_Register and not GPIO_PUPDR_PUPDR11;
         Pull_Register := Pull_Register or Interfaces.Shift_Left (1, 11 * 2);
         GPIOC.PUPDR := Pull_Register;
      end;

      --  Enable SYSCFG's clock.
      RCC.APB2ENR := RCC.APB2ENR or RCC_APB2ENR_SYSCFGEN;

      --  Then, configure SYSCFG_EXTICR3[12:15] (pin 11) for GPIOC.
      declare
         Exti_Register : Interfaces.Unsigned_32;
      begin
         Exti_Register := SYSCFG.EXTICR (2);  -- C indexing!
         Exti_Register := Exti_Register and not SYSCFG_EXTICR3_EXTI11;
         Exti_Register := Exti_Register or SYSCFG_EXTICR3_EXTI11_PC;
         SYSCFG.EXTICR (2) := Exti_Register;
      end;

      --  Then, configure EXTI on pin 11 for rising-edge (button-release).
      EXTI.RTSR := EXTI.RTSR or EXTI_RTSR_TR11;
      --  and enable interrupts on pin 11.
      EXTI.IMR := EXTI.IMR or EXTI_IMR_MR11;
   end Configure_Inputs;

   procedure Configure_Outputs is
      use type Interfaces.Unsigned_32;
   begin
      --  We're going to use a pin on the right-hand side of the card;
      --  P2/21 is PA5, GPIOA pin 5.

      --  First, we need to enable GPIOA's clock.
      RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOAEN;

      --  Then, we need to configure bit 5 as an output.
      declare
         Mode_Register : Interfaces.Unsigned_32;
      begin
         Mode_Register := GPIOA.MODER;
         Mode_Register := Mode_Register and not GPIO_MODER_MODER5;
         Mode_Register := Mode_Register or Interfaces.Shift_Left (1, 5 * 2);
         GPIOA.MODER := Mode_Register;
      end;

      --  Don't need to configure OTYPER, because the default is push-pull.

      --  Don't need to configure OSPEEDR, because the default is
      --  slow, which is (I expect) more than adequate for an LED!

      --  Don't need to configure PUPDR, because the default is no
      --  pull-up, no pull-down.
   end Configure_Outputs;


   function Get (This : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
      pragma Unreferenced (For_Input);
   begin
      return True;
   end Get;

   procedure Set (This : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      use type Interfaces.Unsigned_32;
   begin
      case To is
         when False =>
            GPIOA.BSRRH := Interfaces.Shift_Left (1, 5); -- off
         when True =>
            GPIOA.BSRRL := Interfaces.Shift_Left (1, 5); -- on
      end case;
   end Set;

end Digital_IO.STM32F4_Support;
