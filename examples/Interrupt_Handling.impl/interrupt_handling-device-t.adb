with Interrupt_Handling.Control_C;
with Interrupt_Handling.Events;

with ColdFrame.Exceptions.Message;
with GNAT.IO; use GNAT.IO;

separate (Interrupt_Handling.Device)
task body T is
begin

   delay 0.1;  -- bodge to avoid mingled output.
   Put_Line ("Interrupt_Handling.Device.T running");

   accept Start;

   Put_Line ("Interrupt_Handling.Device.T started");

   loop

      begin

         Control_C.Handler.Wait;

         Put_Line ("Interrupt_Handling.Device.T released");

         declare
            Ev : constant ColdFrame.Project.Events.Event_P
              := new Interrupt (This);
         begin
            ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
         end;

      exception

         when E : others =>
            ColdFrame.Exceptions.Message
              ("Interrupt_Handling.Device.T(loop)", E);

      end;

   end loop;

exception

   when E : others =>
      ColdFrame.Exceptions.Message ("Interrupt_Handling.Device.T", E);

end T;
