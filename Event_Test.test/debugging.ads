with ColdFrame.Project.Events.Standard.Debug;
with ColdFrame.Events_G.Test_G;
package Debugging is

   package Debugging_Queues is new ColdFrame.Project.Events.Test_G
     (Standard_Queue =>
        ColdFrame.Project.Events.Standard.Debug.Event_Queue_Base);

   subtype Event_Queue
      is Debugging_Queues.Event_Queue;

end Debugging;
