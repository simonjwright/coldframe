with ColdFrame.Events.Standard;
with System;

package Simple_Buttons_Dispatcher is

   --  The ColdFrame event queue.
   Dispatcher : aliased ColdFrame.Events.Standard.Event_Queue_Base
     (Priority             => System.Default_Priority,
      Storage_Size         => 2560,
      --  Increased from 2048 for PR108801, see
      --  ColdFrame.Events.Standard; Secondary_Stack_Size here is
      --  ignored, left to default to Storage_Size / 10.
      Secondary_Stack_Size => 256,
      Capacity             => 16);

end Simple_Buttons_Dispatcher;
