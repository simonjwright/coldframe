with ColdFrame.Project.Events.Standard;

package body Performance.Event_Timing is


   procedure Handler (Ev : No_Action) is
   begin
      null;
   end Handler;


   procedure Handler (Ev : Repost) is
   begin
      if Ev.Count > 0 then
         declare
            Next : ColdFrame.Project.Events.Event_P := new Repost;
         begin
            Repost (Next.all).Count := Ev.Count - 1;
            ColdFrame.Project.Events.Post (Next,
                                           On => Dispatcher);
         end;
      end if;
   end Handler;


begin

   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;

end Performance.Event_Timing;
