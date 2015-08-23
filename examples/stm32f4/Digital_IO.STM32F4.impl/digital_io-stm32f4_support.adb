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
with System;
with stm32f429xx_h; use stm32f429xx_h;

package body Digital_IO.STM32F4_Support is

   package EXTI_Configuration is

      type EXTI_Line is mod 16;

      type Used_Input_Signal (Valid : Boolean := False) is record
         case Valid is
            when False => null;
            when True  => Signal : Input_Signal;
         end case;
      end record;
      Line_To_Input_Signal :
        constant array (EXTI_Line) of Used_Input_Signal :=
        (8      => (True, 0),
         11     => (True, 1),
         12     => (True, 2),
         2      => (True, 3),
         --  4      => (True, 4),
         others => <>);

      type GPIO_Access is access all GPIO_TypeDef;

      type GPIO_Index is (A, B, C, D, E, F, G, H, I, J, K)
      with
        Warnings => Off;
      --  Note, 'Pos is the bit number in AHB1ENR, and the number to
      --  set in EXTICR to associate the Line with the GPIO.

      GPIOs : constant array (GPIO_Index) of GPIO_Access :=
        (GPIOA'Access,
         GPIOB'Access,
         GPIOC'Access,
         GPIOD'Access,
         GPIOE'Access,
         GPIOF'Access,
         GPIOG'Access,
         GPIOH'Access,
         GPIOI'Access,
         GPIOJ'Access,
         GPIOK'Access);

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
        (0      => (True, C, 8),
         1      => (True, C, 11),
         2      => (True, C, 12),
         3      => (True, D, 2),
         --  4      => (True, D, 4),
         others => <>);

      Output_Signal_To_Line : constant array (Output_Signal) of Used_Line :=
        (0      => (True, A, 5),
         1      => (True, C, 3),
         2      => (True, F, 6),
         3      => (True, G, 2),
         others => <>);

   end EXTI_Configuration;
   use EXTI_Configuration;

   protected EXTI_Handler
   with
     Interrupt_Priority => System.Interrupt_Priority'First
   is
      entry Button_Pressed (B : out Interfaces.Unsigned_32);
   private
      Triggered        : Boolean := False;

      Last_Read_Values : Interfaces.Unsigned_32 := 0;
      --  We accumulate interrupt indications as they arrive, and
      --  clear when retreived by the caller of Button_Pressed.

      procedure EXTI0_IRQ_Handler;
      pragma Attach_Handler (EXTI0_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI0_IRQ);
      procedure EXTI1_IRQ_Handler;
      pragma Attach_Handler (EXTI1_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI1_IRQ);
      procedure EXTI2_IRQ_Handler;
      pragma Attach_Handler (EXTI2_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI2_IRQ);
      procedure EXTI3_IRQ_Handler;
      pragma Attach_Handler (EXTI3_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI3_IRQ);
      procedure EXTI4_IRQ_Handler;
      pragma Attach_Handler (EXTI4_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI4_IRQ);
      procedure EXTI9_5_IRQ_Handler;
      pragma Attach_Handler (EXTI9_5_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI9_5_IRQ);
      procedure EXTI15_10_IRQ_Handler;
      pragma Attach_Handler (EXTI15_10_IRQ_Handler,
                               Ada.Interrupts.Names.EXTI15_10_IRQ);
   end EXTI_Handler;

   task Receive_Button_Interrupt
   is
   end Receive_Button_Interrupt;

   protected body EXTI_Handler is
      entry Button_Pressed (B : out Interfaces.Unsigned_32)
        when Triggered is
      begin
         B := Last_Read_Values;
         Last_Read_Values := 0;
         Triggered := False;
      end Button_Pressed;

      procedure EXTI0_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI0_IRQ_Handler;

      procedure EXTI1_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI1_IRQ_Handler;

      procedure EXTI2_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI2_IRQ_Handler;

      procedure EXTI3_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI3_IRQ_Handler;

      procedure EXTI4_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI4_IRQ_Handler;

      procedure EXTI9_5_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI9_5_IRQ_Handler;

      procedure EXTI15_10_IRQ_Handler is
         Read_Value : constant Interfaces.Unsigned_32 := EXTI.PR;
         use type Interfaces.Unsigned_32;
      begin
         EXTI.PR := Read_Value;
         Triggered := True;
         Last_Read_Values := Last_Read_Values or Read_Value;
      end EXTI15_10_IRQ_Handler;
   end EXTI_Handler;

   task body Receive_Button_Interrupt is
      Last_Read_Values : Interfaces.Unsigned_32;
      Bits : Bits_32x1;
   begin
      loop
         EXTI_Handler.Button_Pressed (Last_Read_Values);
         Bits := U32_To_B1 (Last_Read_Values);
         for Line in EXTI_Line loop
            if Bits (Integer (Line)) /= 0 then
               Input_Signal_State_Callback.Call_Callbacks
                 ((S     => Line_To_Input_Signal (Line).Signal,
                   State => True));
            end if;
         end loop;
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
      for J in Input_Signal'Range loop
         if Input_Signal_To_Line (J).Valid then
            --  Enable the GPIO's clock.
            declare
               AHB1ENR : Bits_32x1 := U32_To_B1 (RCC.AHB1ENR);
            begin
               AHB1ENR (GPIO_Index'Pos (Input_Signal_To_Line (J).GPIO)) := 1;
               RCC.AHB1ENR := B1_To_U32 (AHB1ENR);
            end;

            declare
               GPIO : GPIO_TypeDef renames
                 GPIOs (Input_Signal_To_Line (J).GPIO).all;
               Line : constant Natural :=
                 Natural (Input_Signal_To_Line (J).Line);
            begin
               --  Don't need to configure the line as an input, because
               --  that's the default.

               --  Don't need to configure OTYPER, because the pin is an
               --  input.

               --  Don't need to configure OSPEEDR, because the pin is an
               --  input.

               --  Configure PUPDR for pull-up.
               --  This means that the 'on' state is logic 0.
               declare
                  PUPDR : Bits_32x2 := U32_To_B2 (GPIO.PUPDR);
               begin
                  PUPDR (Line) := 1;
                  GPIO.PUPDR := B2_To_U32 (PUPDR);
               end;

               --  Enable SYSCFG's clock.
               RCC.APB2ENR := RCC.APB2ENR or RCC_APB2ENR_SYSCFGEN;

               --  Connect the GPIO to the interrupt Line.
               declare
                  Offset : constant Natural := Line / 4;
                  Index : constant Natural := Line mod 4;
                  EXTICR : Bits_32x4 := U32_To_B4 (SYSCFG.EXTICR (Offset))
                    with Volatile;
               begin
                  EXTICR (Index) :=
                    GPIO_Index'Pos (Input_Signal_To_Line (J).GPIO);
                  SYSCFG.EXTICR (Offset) := B4_To_U32 (EXTICR);
               end;

               --  Then, configure EXTI on this line for rising-edge
               --  (button-release).
               declare
                  RTSR : Bits_32x1 := U32_To_B1 (EXTI.RTSR)
                    with Volatile;
               begin
                  RTSR (Line) := 1;
                  EXTI.RTSR := B1_To_U32 (RTSR);
               end;

               --  and enable interrupts on the line.
               declare
                  IMR : Bits_32x1 := U32_To_B1 (EXTI.IMR)
                    with Volatile;
               begin
                  IMR (Line) := 1;
                  EXTI.IMR := B1_To_U32 (IMR);
               end;
            end;
         end if;
      end loop;
   end Configure_Inputs;

   procedure Configure_Outputs is
   begin
      for J in Output_Signal'Range loop
         if Output_Signal_To_Line (J).Valid then
            --  Enable the GPIO's clock.
            declare
               AHB1ENR : Bits_32x1 := U32_To_B1 (RCC.AHB1ENR);
            begin
               AHB1ENR (GPIO_Index'Pos (Output_Signal_To_Line (J).GPIO)) := 1;
               RCC.AHB1ENR := B1_To_U32 (AHB1ENR);
            end;

            --  Configure the line as an output.
            declare
               GPIO : GPIO_TypeDef renames
                 GPIOs (Output_Signal_To_Line (J).GPIO).all;
               Line : constant Natural :=
                 Natural (Output_Signal_To_Line (J).Line);
               MODER : Bits_32x2 := U32_To_B2 (GPIO.MODER);
            begin
               MODER (Line) := 1;
               GPIO.MODER := B2_To_U32 (MODER);
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


   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
      use type Interfaces.Unsigned_32;
      Line : constant Natural :=
        Natural (Input_Signal_To_Line (Input_Signal (For_Input)).Line);
      GPIO : GPIO_TypeDef renames
        GPIOs (Input_Signal_To_Line (Input_Signal (For_Input)).GPIO).all;
   begin
      --  The 'on' state is logic 0, because we're pulling up.
      return U32_To_B1 (GPIO.IDR)(Line) = 0;
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      Line : constant Natural :=
        Natural (Output_Signal_To_Line (Output_Signal (For_Output)).Line);
      Bits : Bits_32x1 := (others => 0);
      GPIO : GPIO_TypeDef renames
        GPIOs (Output_Signal_To_Line (Output_Signal (For_Output)).GPIO).all;
   begin
      --  To set, use the low 16 bits; to clear, use the high 16 bits
      Bits (Line + (if To then 0 else 16)) := 1;
      GPIO.BSRR := B1_To_U32 (Bits);
   end Set;

end Digital_IO.STM32F4_Support;
