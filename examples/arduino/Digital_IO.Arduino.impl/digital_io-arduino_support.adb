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

with Ada.Interrupts.Names;
--  Note, on this CPU the peripheral number is the same as the
--  Interrupt_ID (or vice versa!)

with Ada.Real_Time;
with Digital_IO.Input_Signal_State_Callback;
with ATSAM3X8E.MATRIX; use ATSAM3X8E.MATRIX;
with ATSAM3X8E.PIO;    use ATSAM3X8E.PIO;
with ATSAM3X8E.PMC;    use ATSAM3X8E.PMC;

package body Digital_IO.Arduino_Support is

   --  For pin mapping see https://www.arduino.cc/en/Hacking/PinMappingSAM3X.
   --
   --  However, here we'd like to use the on-board ERASE pin (PC0) as
   --  input, and the on-board LED (PB27). This is Input_Signal'(0).

   type Pin_Data is record
      The_Peripheral_ID : Natural;
      The_PIO           : access PIO_Peripheral;
      The_Pin           : Natural;
   end record;

   type Input_Pin_Mapping
     is array (Digital_IO_Support.Input_Signal range <>) of Pin_Data;

   --  **** NB ****
   --
   --  PC0 is the ERASE pin, which is the only pushbutton on the
   --  board. Can be configured as PIO; we'll configure properly
   --  later.

   Input_Pin_Map : constant Input_Pin_Mapping
     := (0 => (Ada.Interrupts.Names.PIOC_IRQ,
               PIOC_Periph'Access,
               1));  -- pb26 is pin 22, pc1 is pin 33

   subtype Used_Input_Signal
     is Digital_IO_Support.Input_Signal range Input_Pin_Map'Range;

   --  Output signals:
   --
   --  Onboard LED is PB27.

   type Output_Pin_Mapping
     is array (Digital_IO_Support.Output_Signal range <>) of Pin_Data;

   Output_Pin_Map : constant Output_Pin_Mapping
     := (0 => (Ada.Interrupts.Names.PIOB_IRQ,
               PIOB_Periph'Access,
               27));

   --  How often to check for input change
   Debounce_Interval : constant Ada.Real_Time.Time_Span
     := Ada.Real_Time.Milliseconds (5);

   --  Number of successive inputs that must be the same to recognise
   --  a change
   Debounce_Count : constant := 8;

   task Debounce_Inputs;

   procedure Configure_Inputs;
   procedure Configure_Outputs;

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
               The_PIO : PIO_Peripheral
                 renames Input_Pin_Map (L).The_PIO.all;
               The_Pin : Natural
                 renames Input_Pin_Map (L).The_Pin;
               use type ATSAM3X8E.Bit;
               State : constant Boolean
                 := The_PIO.PDSR.Arr (The_Pin) = 0;
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
      use type ATSAM3X8E.Bit;  -- for assertions
   begin
      for Data of Input_Pin_Map loop

         --  Check for PC0: have to make it a normal PIO, not ERASE!
         if Data.The_Peripheral_ID = Ada.Interrupts.Names.PIOC_IRQ
           and then Data.The_Pin = 0
         then
            MATRIX_Periph.CCFG_SYSIO.SYSIO12 := 0;
            pragma Assert (MATRIX_Periph.CCFG_SYSIO.SYSIO12 = 0,
                           "ERASE on PC0 still active");
         end if;

         --  enable the PIO
         if Data.The_Peripheral_ID < 32 then
            PMC_Periph.PMC_PCER0.PID.Arr (Data.The_Peripheral_ID) := 1;
            pragma Assert
              (PMC_Periph.PMC_PCSR0.PID.Arr (Data.The_Peripheral_ID) /= 0,
               "peripheral" & Data.The_Peripheral_ID'Image & " not enabled");
         else
            PMC_Periph.PMC_PCER1.PID.Arr (Data.The_Peripheral_ID - 32)  := 1;
            pragma Assert
              (PMC_Periph.PMC_PCSR1.PID.Arr (Data.The_Peripheral_ID - 32) /= 0,
               "peripheral" & Data.The_Peripheral_ID'Image & " not enabled");
         end if;

         --  enable pin
         Data.The_PIO.PER.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.PSR.Arr (Data.The_Pin) /= 0,
                          "PIO inactive");

         --  pin is input
         Data.The_PIO.ODR.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.OSR.Arr (Data.The_Pin) = 0,
                          "output enabled");

         --  pull-up should be enabled at reset, but set it anyway
         Data.The_PIO.PUDR.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.PUSR.Arr (Data.The_Pin) = 1,
                        "pullup not disabled");
         Data.The_PIO.PUER.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.PUSR.Arr (Data.The_Pin) = 0,
                        "pullup not enabled");
         pragma Assert (Data.The_PIO.PDSR.Arr (Data.The_Pin) = 1,
                        "not pulled up");

      end loop;
   end Configure_Inputs;

   procedure Configure_Outputs is
      use type ATSAM3X8E.Bit;  -- for assertions
   begin
      for Data of Output_Pin_Map loop
         --  enable the PIO
         if Data.The_Peripheral_ID < 32 then
            PMC_Periph.PMC_PCER0.PID.Arr (Data.The_Peripheral_ID) := 1;
            pragma Assert
              (PMC_Periph.PMC_PCSR0.PID.Arr (Data.The_Peripheral_ID) /= 0,
               "peripheral" & Data.The_Peripheral_ID'Image & " not enabled");
         else
            PMC_Periph.PMC_PCER1.PID.Arr (Data.The_Peripheral_ID - 32)  := 1;
            pragma Assert
              (PMC_Periph.PMC_PCSR1.PID.Arr (Data.The_Peripheral_ID - 32) /= 0,
               "peripheral" & Data.The_Peripheral_ID'Image & " not enabled");
         end if;

         --  enable pin
         Data.The_PIO.PER.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.PSR.Arr (Data.The_Pin) /= 0,
                          "PIO inactive");

         --  pin is output
         Data.The_PIO.OER.Arr (Data.The_Pin) := 1;
         pragma Assert (Data.The_PIO.OSR.Arr (Data.The_Pin) /= 0,
                          "output disabled");

         --  clear pin
         Data.The_PIO.CODR.Arr (Data.The_Pin) := 1;
      end loop;
   end Configure_Outputs;


   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal)
                return Boolean
   is
      pragma Unreferenced (This);  -- only used for dispatching
   begin
      if For_Input in Input_Pin_Map'Range then
         declare
            The_Data : constant Pin_Data := Input_Pin_Map (For_Input);
            use type ATSAM3X8E.Bit;
         begin
            --  The 'on' state is logic 0, because we're pulling up.
            return The_Data.The_PIO.PDSR.Arr (The_Data.The_Pin) = 0;
         end;
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
         declare
            The_Data : constant Pin_Data := Output_Pin_Map (For_Output);
         begin
            if To then
               The_Data.The_PIO.SODR.Arr (The_Data.The_Pin) := 1;
            else
               The_Data.The_PIO.CODR.Arr (The_Data.The_Pin) := 1;
            end if;
         end;
      end if;
   end Set;

end Digital_IO.Arduino_Support;
