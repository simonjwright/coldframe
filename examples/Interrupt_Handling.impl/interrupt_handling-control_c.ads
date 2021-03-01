with Ada.Interrupts.Names;
package Interrupt_Handling.Control_C is
   protected Handler is
      entry Wait;
   private
      procedure Handler with Attach_Handler => Ada.Interrupts.Names.SIGINT;
      Released : Boolean := False;
   end Handler;
end Interrupt_Handling.Control_C;
