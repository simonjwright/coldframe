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
with System;

with ATSAM3X8E.PIO; use ATSAM3X8E.PIO;
with ATSAM3X8E.PMC; use ATSAM3X8E.PMC;

package body Digital_IO.Arduino_Support is

   --  For pin mapping see https://www.arduino.cc/en/Hacking/PinMappingSAM3X.

   type Pin_Data is record
      The_Peripheral_ID : Natural;
      The_PIO           : access PIO_Peripheral;
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
     := (0 => (Ada.Interrupts.Names.PIOB_IRQ,
               PIOB_Periph'Access,
               14),
         1 => (Ada.Interrupts.Names.PIOB_IRQ,
               PIOB_Periph'Access,
               21),
         2 => (Ada.Interrupts.Names.PIOC_IRQ,
               PIOC_Periph'Access,
               12),
         3 => (Ada.Interrupts.Names.PIOC_IRQ,
               PIOC_Periph'Access,
               13),
         4 => (Ada.Interrupts.Names.PIOC_IRQ,
               PIOC_Periph'Access,
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
     := (0 => (Ada.Interrupts.Names.PIOB_IRQ,
               PIOB_Periph'Access,
               26),
         1 => (Ada.Interrupts.Names.PIOA_IRQ,
               PIOA_Periph'Access,
               14),
         2 => (Ada.Interrupts.Names.PIOA_IRQ,
               PIOA_Periph'Access,
               15),
         3 => (Ada.Interrupts.Names.PIOD_IRQ,
               PIOD_Periph'Access,
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
         Read_Value : constant ISR_Register := PIOA_Periph.ISR;
         use Ada.Interrupts.Names;
         use type ATSAM3X8E.Bit;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOA_IRQ
              and then Read_Value.Arr (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOA_IRQ_Handler;

      procedure PIOB_IRQ_Handler is
         Read_Value : constant ISR_Register := PIOB_Periph.ISR;
         use Ada.Interrupts.Names;
         use type ATSAM3X8E.Bit;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOB_IRQ
              and then Read_Value.Arr (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOB_IRQ_Handler;

      procedure PIOC_IRQ_Handler is
         Read_Value : constant ISR_Register := PIOC_Periph.ISR;
         use Ada.Interrupts.Names;
         use type ATSAM3X8E.Bit;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOC_IRQ
              and then Read_Value.Arr (Input_Pin_Map (J).The_Pin) /= 0
            then
               Input_Handler.Inputs (J) := True;
            end if;
         end loop;
      end PIOC_IRQ_Handler;

      procedure PIOD_IRQ_Handler is
         Read_Value : constant ISR_Register := PIOD_Periph.ISR;
         use Ada.Interrupts.Names;
         use type ATSAM3X8E.Bit;
      begin
         Triggered := True;
         for J in Input_Pin_Map'Range loop
            if Input_Pin_Map (J).The_Peripheral_ID = PIOD_IRQ
              and then Read_Value.Arr (Input_Pin_Map (J).The_Pin) /= 0
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
      use Ada.Interrupts.Names;
      use type Ada.Real_Time.Time;
   begin
      --  Enable PIOB
      declare
         PIDs : PMC_PCER0_PID_Field_Array := (others => 0);
      begin
         PIDs (PIOB_IRQ) := 1;
         PMC_Periph.PMC_PCER0 := (PID    => (As_Array => True,
                                             Arr      => PIDs),
                                  others => <>);
      end;
      --  Enable PB27 ..
      PIOB_Periph.PER := (As_Array => True,
                          Arr => (27 => 1, others => 0));
      --  .. as output.
      PIOB_Periph.OER := (As_Array => True,
                          Arr => (27 => 1, others => 0));

      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         for Data of Input_Pin_Map loop
            declare
               use type ATSAM3X8E.Bit;
            begin
               null;
               pragma Assert (Data.The_PIO.PSR.Arr (Data.The_Pin) /= 0,
                                "PIO inactive");
               pragma Assert (Data.The_PIO.OSR.Arr (Data.The_Pin) = 0,
                                "output enabled");
               pragma Assert (Data.The_PIO.PUSR.Arr (Data.The_Pin) = 0,
                                "pullup disabled");
               pragma Assert (Data.The_PIO.IMR.Arr (Data.The_Pin) /= 0,
                                "interrupt disabled");
            end;
         end loop;
         PIOB_Periph.CODR := (As_Array => True,
                              Arr => (27 => 1, others => 0));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         PIOB_Periph.SODR := (As_Array => True,
                              Arr => (27 => 1, others => 0));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         PIOB_Periph.CODR := (As_Array => True,
                              Arr => (27 => 1, others => 0));
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
         PIOB_Periph.SODR := (As_Array => True,
                              Arr => (27 => 1, others => 0));
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
      use type ATSAM3X8E.Bit;  -- for assertions
   begin
      for Data of Input_Pin_Map loop
         --  enable the PIO
         if Data.The_Peripheral_ID < 32 then
            declare
               PIDs : PMC_PCER0_PID_Field_Array := (others => 0);
            begin
               PIDs (Data.The_Peripheral_ID) := 1;
               PMC_Periph.PMC_PCER0 := (PID    => (As_Array => True,
                                                   Arr      => PIDs),
                                        others => <>);
            end;
         else
            declare
               PIDs : PMC_PCER1_PID_Field_Array := (others => 0);
            begin
               PIDs (Data.The_Peripheral_ID - 32) := 1;
               PMC_Periph.PMC_PCER1 := (PID    => (As_Array => True,
                                                   Arr      => PIDs),
                                        others => <>);
            end;
         end if;

         --  enable pin
         Data.The_PIO.PER := (As_Array => False,
                              Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.PSR.Arr (Data.The_Pin) /= 0,
                          "PIO inactive");

         --  pin is input
         Data.The_PIO.ODR := (As_Array => False,
                              Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.OSR.Arr (Data.The_Pin) = 0,
                          "output enabled");

         --  pull-up should be enabled at reset, but set it anyway
         Data.The_PIO.PUER := (As_Array => False,
                               Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.PUSR.Arr (Data.The_Pin) = 0,
                          "pullup not enabled");

         --  rising/falling interrupt modes
         Data.The_PIO.AIMER := (As_Array => False,
                                Val => 2 ** Data.The_Pin);

         --  interrupts on button-up
         Data.The_PIO.REHLSR := (As_Array => False,
                                 Val => 2 ** Data.The_Pin);

         --  Debounce: slow clock multiplier (32 kHz; we want 5 ms, 200 Hz)
         --  From 11057 datasheet, 31.5.9 & 31.7.2:
         --     Tdiv_slclk = 2*(DIV+1)*Tslow_clock
         --  so
         --     DIV = Tdiv_slclk/Tslow_clock/2 - 1
         --         = Fslow_clock/Fslclk/2 - 1
         Data.The_PIO.SCDR  := (DIV => ATSAM3X8E.UInt14 (32768 / 200 / 2 - 1),
                                others => <>);

         --  debounce vs glitch
         Data.The_PIO.DIFSR := (As_Array => False,
                                Val => 2 ** Data.The_Pin);

         --  enable debounce
         Data.The_PIO.IFER := (As_Array => False,
                               Val => 2 ** Data.The_Pin);

         --  clear previously-set interrupts (best done before general
         --  processing starts!)
         declare
            ISR : ISR_Register;
         begin
            ISR := Data.The_PIO.ISR;
            ISR := Data.The_PIO.ISR;
            pragma Assert (ISR.Arr (Data.The_Pin) = 0,
                          "interrupt outstanding");
         end;

         --  enable interrupts
         Data.The_PIO.IER := (As_Array => False,
                              Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.IMR.Arr (Data.The_Pin) /= 0,
                          "interrupt not enabled");
      end loop;
   end Configure_Inputs;

   procedure Configure_Outputs is
      use type ATSAM3X8E.Bit;  -- for assertions
   begin
      for Data of Output_Pin_Map loop
         --  enable the PIO
         if Data.The_Peripheral_ID < 32 then
            declare
               PIDs : PMC_PCER0_PID_Field_Array := (others => 0);
            begin
               PIDs (Data.The_Peripheral_ID) := 1;
               PMC_Periph.PMC_PCER0 := (PID    => (As_Array => True,
                                                   Arr      => PIDs),
                                        others => <>);
            end;
         else
            declare
               PIDs : PMC_PCER1_PID_Field_Array := (others => 0);
            begin
               PIDs (Data.The_Peripheral_ID - 32) := 1;
               PMC_Periph.PMC_PCER1 := (PID    => (As_Array => True,
                                                   Arr      => PIDs),
                                        others => <>);
            end;
         end if;

         --  enable pin
         Data.The_PIO.PER := (As_Array => False,
                              Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.PSR.Arr (Data.The_Pin) /= 0,
                          "PIO inactive");

         --  pin is output
         Data.The_PIO.OER := (As_Array => False,
                              Val => 2 ** Data.The_Pin);
         pragma Assert (Data.The_PIO.OSR.Arr (Data.The_Pin) /= 0,
                          "output disabled");

         --  clear pin
         Data.The_PIO.CODR := (As_Array => False,
                               Val => 2 ** Data.The_Pin);
      end loop;
   end Configure_Outputs;


   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
      The_Data : constant Pin_Data := Input_Pin_Map (For_Input);
      use type ATSAM3X8E.Bit;
   begin
      --  The 'on' state is logic 0, because we're pulling up.
      return The_Data.The_PIO.PDSR.Arr (The_Data.The_Pin) = 0;
   end Get;

   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      The_Data : constant Pin_Data := Output_Pin_Map (For_Output);
   begin
      if To then
         The_Data.The_PIO.SODR := (As_Array => False,
                                   Val => 2 ** The_Data.The_Pin);
      else
         The_Data.The_PIO.CODR := (As_Array => False,
                                   Val => 2 ** The_Data.The_Pin);
      end if;
   end Set;

end Digital_IO.Arduino_Support;
