with ColdFrame.Project.Events.Standard.Debug;

package body Performance.Event_Timing is


   procedure Handler (Ev : Repost) is
   begin
      if Ev.Count > 0 then
         declare
            Next : ColdFrame.Project.Events.Event_P := new Repost;
         begin
            Repost (Next.all).Count := Ev.Count - 1;
            ColdFrame.Project.Events.Post (Next,
                                           On => Dispatcher_A);
         end;
      else
         Done_At := High_Resolution_Time.Clock;
      end if;
   end Handler;


   procedure Handler (Ev : Ping) is
   begin
      if Ev.Count > 0 then
         declare
            Next : ColdFrame.Project.Events.Event_P := new Pong;
         begin
            Pong (Next.all).Count := Ev.Count - 1;
            ColdFrame.Project.Events.Post (Next,
                                           On => Dispatcher_B);
         end;
      else
         Done_At := High_Resolution_Time.Clock;
      end if;
   end Handler;


   procedure Handler (Ev : Pong) is
   begin
      if Ev.Count > 0 then
         declare
            Next : ColdFrame.Project.Events.Event_P := new Ping;
         begin
            Ping (Next.all).Count := Ev.Count - 1;
            ColdFrame.Project.Events.Post (Next,
                                           On => Dispatcher_A);
         end;
      else
         Done_At := High_Resolution_Time.Clock;
      end if;
   end Handler;


   procedure Handler (Ev : Timing) is
   begin
      if Ev.Count > 0 then
         declare
            Next : ColdFrame.Project.Events.Event_P := new Timing;
         begin
            Timing (Next.all).Count := Ev.Count - 1;
            ColdFrame.Project.Events.Post (Next,
                                           On => Dispatcher_A);
         end;
      else
         Done_At := High_Resolution_Time.Clock;
      end if;
   end Handler;


begin

   Dispatcher_A := new ColdFrame.Project.Events.Standard.Event_Queue;
   Dispatcher_B := new ColdFrame.Project.Events.Standard.Event_Queue;

end Performance.Event_Timing;
