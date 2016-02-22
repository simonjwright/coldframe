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
with Ada.Real_Time;
with Digital_IO.Input_Signal_State_Callback;
with Interfaces;
with System;
with Registers.ATSAM3X8.Peripheral_Identifiers;
with Registers.ATSAM3X8.PMC;
with Registers.ATSAM3X8.PIO;

package body Digital_IO.Arduino_Support is

   --  For pin mapping see https://www.arduino.cc/en/Hacking/PinMappingSAM3X.

   type Pin_Data is record
      The_Peripheral_ID : Natural;
      The_PIO           : access Registers.ATSAM3X8.PIO.PIO;
      The_Pin           : Natural;
   end record;

   --  Input signals:
   --
   --  0 is Digital Pin 53 (PB14)
   --  1 is Digital Pin 52 (PB21)
   --  2 is Digital Pin 51 (PC12)
   --  3 is Digital Pin 50 (PC13)
   --  4 is Digital Pin 49 (PC14)

   type Input_Pin_Mapping
     is array (Digital_IO_Support.Input_Signal range <>) of Pin_Data;

   Input_Pin_Map : constant Input_Pin_Mapping
     := (0 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOB_IRQ,
               Registers.ATSAM3X8.PIO.PIOB'Access,
               14),
         1 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOB_IRQ,
               Registers.ATSAM3X8.PIO.PIOB'Access,
               21),
         2 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOC_IRQ,
               Registers.ATSAM3X8.PIO.PIOC'Access,
               12),
         3 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOC_IRQ,
               Registers.ATSAM3X8.PIO.PIOC'Access,
               13),
         4 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOC_IRQ,
               Registers.ATSAM3X8.PIO.PIOC'Access,
               14));

   subtype Used_Input_Signal
     is Digital_IO_Support.Input_Signal range Input_Pin_Map'Range;

   --  Output signals:
   --
   --  0 is Digital Pin 22 (PB26)
   --  1 is Digital Pin 23 (PA14)
   --  2 is Digital Pin 24 (PA15)
   --  3 is Digital Pin 25 (PD0)

   type Output_Pin_Mapping
     is array (Digital_IO_Support.Output_Signal range <>) of Pin_Data;

   Output_Pin_Map : constant Output_Pin_Mapping
     := (0 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOB_IRQ,
               Registers.ATSAM3X8.PIO.PIOB'Access,
               26),
         1 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOA_IRQ,
               Registers.ATSAM3X8.PIO.PIOA'Access,
               14),
         2 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOA_IRQ,
               Registers.ATSAM3X8.PIO.PIOA'Access,
               15),
         3 => (Registers.ATSAM3X8.Peripheral_Identifiers.PIOD_IRQ,
               Registers.ATSAM3X8.PIO.PIOD'Access,
               0));


   type Inputs_Detected is array (Used_Input_Signal) of Boolean;


   protected Input_Handler
   with
     Interrupt_Priority => System.Interrupt_Priority'First
   is
      entry Button_Pressed (Inputs : out Inputs_Detected);
   private
      Triggered        : Boolean := False;

      Inputs : Inputs_Detected := (others => False);
      --  We accumulate interrupt indications as they arrive, and
      --  clear when retreived by the caller of Button_Pressed.

      procedure PIOA_IRQ_Handler
      with
        Attach_Handler => Ada.Interrupts.Names.PIOA_IRQ,
        Unreferenced;
      procedure PIOB_IRQ_Handler
      with
        Attach_Handler => Ada.Interrupts.Names.PIOB_IRQ,
        Unreferenced;
      procedure PIOC_IRQ_Handler
      with
        Attach_Handler => Ada.Interrupts.Names.PIOC_IRQ,
        Unreferenced;
      procedure PIOD_IRQ_Handler
      with
        Attach_Handler => Ada.Interrupts.Names.PIOD_IRQ,
        Unreferenced;
   end Input_Handler;

   task Receive_Button_Interrupt is
   end Receive_Button_Interrupt;

   task Heartbeat is
      --  Flash the LED
   end Heartbeat;

   protected body Input_Handler is
      entry Button_Pressed (Inputs : out Inputs_Detected)
        when Triggered is
      begin
         Inputs := Input_Handler.Inputs;
         Input_Handler.Inputs := (others => False);
         Triggered := False;
      end Button_Pressed;

      procedure PIOA_IRQ_Handler is
         Read_Value : constant Registers.Bits_32x1
           := Registers.ATSAM3X8.PIO.PIOA.ISR;
         use Registers.ATSAM3X8.Peripheral_Identifiers;
         use type Registers.Bits_1;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOA_IRQ
              and then Read_Value (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOA_IRQ_Handler;

      procedure PIOB_IRQ_Handler is
         Read_Value : constant Registers.Bits_32x1
           := Registers.ATSAM3X8.PIO.PIOB.ISR;
         use Registers.ATSAM3X8.Peripheral_Identifiers;
         use type Registers.Bits_1;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOB_IRQ
              and then Read_Value (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOB_IRQ_Handler;

      procedure PIOC_IRQ_Handler is
         Read_Value : constant Registers.Bits_32x1
           := Registers.ATSAM3X8.PIO.PIOC.ISR;
         use Registers.ATSAM3X8.Peripheral_Identifiers;
         use type Registers.Bits_1;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOC_IRQ
              and then Read_Value (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOC_IRQ_Handler;

      procedure PIOD_IRQ_Handler is
         Read_Value : constant Registers.Bits_32x1
           := Registers.ATSAM3X8.PIO.PIOD.ISR;
         use Registers.ATSAM3X8.Peripheral_Identifiers;
         use type Registers.Bits_1;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOD_IRQ
              and then Read_Value (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOD_IRQ_Handler;
   end Input_Handler;

   task body Receive_Button_Interrupt is
      Inputs : Inputs_Detected;
   begin
      loop
         Input_Handler.Button_Pressed (Inputs);
         for J in Inputs'Range loop
            if Inputs (J) then
               Input_Signal_State_Callback.Call_Callbacks
                 ((S     => Integer (J),
                   State => True));
            end if;
         end loop;
      end loop;
   end Receive_Button_Interrupt;

   task body Heartbeat is
      --  The on-board LED is pin PB27.
      use Registers.ATSAM3X8.Peripheral_Identifiers;
      use Registers.ATSAM3X8.PMC;
      use Registers.ATSAM3X8.PIO;
      use type Ada.Real_Time.Time;
   begin
      --  Enable PIOB
      PMC.PCER0 := (PIOB_IRQ => 1, others => 0);
      --  Enable PB27 ..
      PIOB.PER := (27 => 1, others => 0);
      --  .. as output.
      PIOB.OER := (27 => 1, others => 0);

      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         PIOB.CODR := (27 => 1, others => 0);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         PIOB.SODR := (27 => 1, others => 0);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         PIOB.CODR := (27 => 1, others => 0);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
         PIOB.SODR := (27 => 1, others => 0);
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;
   end Heartbeat;

   procedure Configure_Inputs;
   procedure Configure_Outputs;

   procedure Initialize is
   begin
      Configure_Inputs;
      Configure_Outputs;
      Register (new Implementation);
   end Initialize;

   procedure Configure_Inputs is
      Bits : Registers.Bits_32x1;
      use Registers.ATSAM3X8.PMC;
      use type Interfaces.Unsigned_32;
   begin
      for Data of Input_Pin_Map loop
         --  enable the PIO
         Bits := (others => 0);
         if Data.The_Peripheral_ID < 32 then
            Bits (Data.The_Peripheral_ID) := 1;
            PMC.PCER0 := Bits;
         else
            Bits (Data.The_Peripheral_ID - 32) := 1;
            PMC.PCER1 := Bits;
         end if;

         Bits := (others => 0);
         Bits (Data.The_Pin) := 1;

         Data.The_PIO.SCDR := 32768 / 200;
         --  debounce slow clock multiplier; slow clock is 32768 Hz,
         --  we want 5 ms => 200 Hz.

         Data.The_PIO.PER    := Bits; -- enable pin
         Data.The_PIO.ODR    := Bits; -- pin is input
         --  pull-up is enabled at reset
         Data.The_PIO.AIMER  := Bits; -- rising/falling interrupt modes
         Data.The_PIO.REHLSR := Bits; -- interrupts on button-up
         Data.The_PIO.DIFSR  := Bits; -- debounce

         --  clear previously-set interrupts (best done before general
         --  processing starts!)
         declare
            Dummy : Registers.Bits_32x1
              with Unreferenced;
         begin
            Dummy := Data.The_PIO.ISR;
         end;
         Data.The_PIO.IER    := Bits; -- enable interrupts
      end loop;
   end Configure_Inputs;

   procedure Configure_Outputs is
      Bits : Registers.Bits_32x1;
      use Registers.ATSAM3X8.PMC;
   begin
      for Data of Output_Pin_Map loop
         --  enable the PIO
         Bits := (others => 0);
         if Data.The_Peripheral_ID < 32 then
            Bits (Data.The_Peripheral_ID) := 1;
            PMC.PCER0 := Bits;
         else
            Bits (Data.The_Peripheral_ID - 32) := 1;
            PMC.PCER1 := Bits;
         end if;

         Bits := (others => 0);
         Bits (Data.The_Pin) := 1;

         Data.The_PIO.PER  := Bits; -- enable pin
         Data.The_PIO.OER  := Bits; -- pin is output
         Data.The_PIO.CODR := Bits; -- clear
      end loop;
   end Configure_Outputs;


   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
      The_Data : constant Pin_Data := Input_Pin_Map (For_Input);
      use type Registers.Bits_1;
   begin
      --  The 'on' state is logic 0, because we're pulling up.
      return The_Data.The_PIO.PDSR (The_Data.The_Pin) = 0;
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      The_Data : constant Pin_Data := Output_Pin_Map (For_Output);
      Bits : Registers.Bits_32x1 := (others => 0);
   begin
      Bits (The_Data.The_Pin) := 1;
      if To then
         The_Data.The_PIO.SODR := Bits;
      else
         The_Data.The_PIO.CODR := Bits;
      end if;
   end Set;

end Digital_IO.Arduino_Support;
