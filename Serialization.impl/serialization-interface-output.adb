--  $Id: serialization-interface-output.adb,v fbe698e22b63 2003/01/24 06:25:15 simon $

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
   Serializable'Output
     (Server.Posted_Value (Ev.all).Payload'Unrestricted_Access, V);
   ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);

end Output;
