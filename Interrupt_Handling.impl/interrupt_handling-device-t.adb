with Ada.Interrupts.Names;

with Interrupt_Handling.Events;

with ColdFrame.Interrupts;
with ColdFrame.Exceptions.Message;
with GNAT.IO; use GNAT.IO;

separate (Interrupt_Handling.Device)
task body T is

   H : ColdFrame.Interrupts.Handler;

begin

   Put_Line ("Interrupt_Handling.Device.T running");

   accept Start;

   ColdFrame.Interrupts.Attach (H, Ada.Interrupts.Names.SIGHUP);

   Put_Line ("Interrupt_Handling.Device.T started");

   loop

      begin

         ColdFrame.Interrupts.Wait (On => H);

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
