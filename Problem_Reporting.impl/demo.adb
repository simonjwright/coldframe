with Ada.Text_Io;
With Problem_Reporting.Interface;
with Problem_Reporting.Interface.Add_Component;
with Problem_Reporting.Interface.Add_Problem;
with Problem_Reporting.Interface.Delete_Component;
with Problem_Reporting.Interface.Note_Defect;
with Problem_Reporting.Interface.Reject_Problem;
with Problem_Reporting.Interface.Report_Problems;

procedure Demo is

  use Problem_Reporting.Interface;

begin

  Add_Component ("foo");
  Add_Component ("bar");
  Add_Component ("baz");
  Add_Component ("quux");

  Add_Problem (1, "alice", "complex");
  Add_Problem (2, "bob", "boring");
  Add_Problem (3, "carol", "silly");
  Add_Problem (4, "dave", "over-bold");

  Note_Defect (1, "foo", "hairy");
  Note_Defect (1, "bar", "very hairy");
  Note_Defect (2, "quux", "tedious");
  Note_Defect (2, "foo", "very tedious");

  Reject_Problem (4, "just right");

  Report_Problems;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line
     ("deleting a simple Component, and then adding a new Component:");
  Delete_Component ("baz");
  Add_Component ("squeeg");

  Report_Problems;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line
     ("deleting a Component with defects:");
  Delete_Component ("foo");

  Report_Problems;

end Demo;
