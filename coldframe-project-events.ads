with ColdFrame.Events_G;
with ColdFrame.Global_Storage_Pool;
with ColdFrame.Project.Event_Support;

package ColdFrame.Project.Events is new ColdFrame.Events_G
  (Time => ColdFrame.Project.Event_Support.Signature,
   Event_Storage => ColdFrame.Global_Storage_Pool.Pool);
