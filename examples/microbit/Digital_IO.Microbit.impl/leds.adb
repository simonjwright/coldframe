with nrf51.GPIO;

package body LEDs is

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
   type LED_Array is array (Coord, Coord) of LED_Pins;
   LEDs : constant LED_Array :=
     (((R1, C1), (R2, C4), (R1, C2), (R2, C5), (R1, C3)),
      ((R3, C4), (R3, C5), (R3, C6), (R3, C7), (R3, C8)),
      ((R2, C2), (R1, C9), (R2, C3), (R3, C9), (R2, C1)),
      ((R1, C8), (R1, C7), (R1, C6), (R1, C5), (R1, C4)),
      ((R3, C3), (R2, C7), (R3, C1), (R2, C6), (R3, C2)));

   Current_State : array (Coord, Coord) of Boolean :=
     (others => (others => False));

   procedure Redisplay;

   procedure Clear
   is
   begin
      Current_State := (others => (others => False));
      Redisplay;
   end Clear;

   procedure Set (At_Row : Coord; At_Col : Coord; To : Boolean)
   is
   begin
      Current_State (At_Row, At_Col) := To;
      Redisplay;
   end Set;

   procedure Initialize
   is
      use nrf51.GPIO;
   begin
      --  LED matrix setup
      for R in Row_Pin loop
         GPIO_Periph.PIN_CNF (R'Enum_Rep) := (DIR    => Output,
                                              PULL   => Pullup,
                                              others => <>);
      end loop;
      for C in Col_Pin loop
         GPIO_Periph.PIN_CNF (C'Enum_Rep) := (DIR    => Output,
                                              PULL   => Pullup,
                                              others => <>);
      end loop;

      Current_State := (others => (others => False));
      Redisplay;
   end Initialize;

   procedure Redisplay
   is
      use nrf51.GPIO;
   begin
      for Row in Current_State'Range (1) loop
         for Col in Current_State'Range (2) loop
            GPIO_Periph.OUT_k.Arr (LEDs (Row, Col).R'Enum_Rep) := Low;
            GPIO_Periph.OUT_k.Arr (LEDs (Row, Col).C'Enum_Rep) := High;
         end loop;
      end loop;
      for Row in Current_State'Range (1) loop
         for Col in Current_State'Range (2) loop
            if Current_State (Row, Col) then
               GPIO_Periph.OUT_k.Arr (LEDs (Row, Col).R'Enum_Rep) := High;
               GPIO_Periph.OUT_k.Arr (LEDs (Row, Col).C'Enum_Rep) := Low;
            end if;
         end loop;
      end loop;
   end Redisplay;

end LEDs;
