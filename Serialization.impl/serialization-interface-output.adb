--  $Id: serialization-interface-output.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

with ColdFrame.Project.Events;
with Serialization.Events;
with Serialization.Server;

separate (Serialization.Interface)
procedure Output
  (V : Serializable) is

   SH : constant Server.Handle := Server.Find;

   Ev : ColdFrame.Project.Events.Event_P;

   use type Server.Handle;

begin

   if SH = null then
      raise Use_Error;
   end if;

   Ev := new Server.Posted_Value (SH);
   Server.Posted_Value (Ev.all).Payload := new Serializable'(V);
   ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);

end Output;
