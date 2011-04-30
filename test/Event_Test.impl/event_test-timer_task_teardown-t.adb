with Ada.Calendar;
with Event_Test.Events;

separate (Event_Test.Timer_Task_Teardown)
task body T is
   Next : Ada.Calendar.Time;
   use type Ada.Calendar.Time;
begin
   accept Start;
   Next := Ada.Calendar.Clock;
   loop
      Next := Next + 0.01;
      delay until Next;
      ColdFrame.Project.Events.Post (new E1,
                                     On => Events.Dispatcher);
      ColdFrame.Project.Events.Post (new E2,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 0.01);
   end loop;
end T;
