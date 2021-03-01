package body Interrupt_Handling.Control_C is

   pragma Unreserve_All_Interrupts;

   protected body Handler is
      entry Wait when Released is
      begin
         Released := False;
      end Wait;
      procedure Handler is
      begin
         Released := True;
      end Handler;
   end Handler;

end Interrupt_Handling.Control_C;
