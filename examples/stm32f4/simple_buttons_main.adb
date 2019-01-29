with Ada.Real_Time;
with ColdFrame.Events;
with Digital_IO.Initialize;
with Simple_Buttons.Initialize;
with Digital_IO.STM32F4_Support;
with Simple_Buttons_Dispatcher;

procedure Simple_Buttons_Main is

   Dispatcher : constant ColdFrame.Events.Event_Queue_P
     := Simple_Buttons_Dispatcher.Dispatcher'Unchecked_Access;

begin

   Digital_IO.Initialize (Dispatcher);
   Digital_IO.STM32F4_Support.Initialize;
   Simple_Buttons.Initialize (Dispatcher);

   delay until Ada.Real_Time.Time_Last;

end Simple_Buttons_Main;
