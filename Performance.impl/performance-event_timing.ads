with ColdFrame.Project.Events;

package Performance.Event_Timing is

   type No_Action is new ColdFrame.Project.Events.Event_Base with null record;

   procedure Handler (Ev : No_Action);

   type Repost is new ColdFrame.Project.Events.Event_Base with record
      Count : Natural := 0;
   end record;

   procedure Handler (Ev : Repost);

   Dispatcher : ColdFrame.Project.Events.Event_Queue_P;

end Performance.Event_Timing;
