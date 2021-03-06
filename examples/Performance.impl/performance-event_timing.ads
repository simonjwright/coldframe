with ColdFrame.Project.Events;
with ColdFrame.Project.High_Resolution_Time;

package Performance.Event_Timing is

   Loops : constant := 100;

   Done_At : ColdFrame.Project.High_Resolution_Time.Time;

   type Repost is new ColdFrame.Project.Events.Event_Base with record
      Count : Natural := Loops;
   end record;
   procedure Handler (Ev : Repost);

   type Ping is new ColdFrame.Project.Events.Event_Base with record
      Count : Natural := Loops;
   end record;
   procedure Handler (Ev : Ping);

   type Pong is new ColdFrame.Project.Events.Event_Base with record
      Count : Natural := Loops;
   end record;
   procedure Handler (Ev : Pong);

   type Timing is new ColdFrame.Project.Events.Event_Base with record
      Count : Natural := Loops;
   end record;
   procedure Handler (Ev : Timing);

   Timer : ColdFrame.Project.Events.Timer;

   Dispatcher_A : ColdFrame.Project.Events.Event_Queue_P;
   Dispatcher_B : ColdFrame.Project.Events.Event_Queue_P;

end Performance.Event_Timing;
