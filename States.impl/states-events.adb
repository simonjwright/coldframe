with ColdFrame.States.Wall_Timer.Debug;
package body States.Events is
--     The_Dispatcher :
--       aliased ColdFrame.States.Wall_Timer.Event_Queue;
begin
   --     Dispatcher := The_Dispatcher'Access;
   Dispatcher := new ColdFrame.States.Wall_Timer.Debug.Event_Queue;
end States.Events;
