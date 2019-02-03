with ColdFrame.Events.Standard;
with System;

package Simple_Buttons_Dispatcher is

   --  The ColdFrame event queue.
   Dispatcher : aliased ColdFrame.Events.Standard.Event_Queue_Base
     (Priority             => System.Default_Priority,
      Storage_Size         => 2048,
      Secondary_Stack_Size => 256,
      Capacity             => 16);

end Simple_Buttons_Dispatcher;
