--  $Id: house_management-events-initialize.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Set up a standard Event Queue.

with ColdFrame.Project.Events.Standard.Debug;

separate (House_Management.Events)
procedure Initialize is
begin

   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;

end Initialize;
