--  $Id: serialization-events-initialize.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

with ColdFrame.Project.Events.Standard;

separate (Serialization.Events)
procedure Initialize is
begin

   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;

end Initialize;
