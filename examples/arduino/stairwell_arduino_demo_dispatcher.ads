with ColdFrame.Events.Standard;
with System;

package Stairwell_Arduino_Demo_Dispatcher is

   --  The ColdFrame event queue.
   Dispatcher : aliased ColdFrame.Events.Standard.Event_Queue_Base
     (Priority => System.Default_Priority,
      Storage_Size => 4096,
      Capacity => 16);

end Stairwell_Arduino_Demo_Dispatcher;
