with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Events_G.Debug_G;
package Debugging is

   package Debugging_Queues is new ColdFrame.Project.Events.Debug_G
     (Standard_Queue =>
        ColdFrame.Project.Events.Standard.Test.Event_Queue_Base);

   subtype Event_Queue
      is Debugging_Queues.Event_Queue (Start_Started => False);

end Debugging;
