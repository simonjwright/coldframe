with Ada.Interrupts.Names;

with Interrupt_Handling.Events;

with ColdFrame.Interrupts;
with ColdFrame.Exceptions.Message;
with GNAT.IO; use GNAT.IO;

pragma Unreserve_All_Interrupts;
--  so we can use SIGINT on Linux, Mac OS X (Windows doesn't care).

separate (Interrupt_Handling.Device)
task body T is

   H : ColdFrame.Interrupts.Handler;

begin

   while not Domain_Initialized loop
      --  wait until ready
      delay 0.1;
   end loop;

   ColdFrame.Interrupts.Attach (H, Ada.Interrupts.Names.SIGINT);

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
