with Ada.Text_IO;
with Problem_Reporting.Initialize;
with Problem_Reporting.Interface;

procedure Demo is

   use Problem_Reporting.Interface;

begin

   Problem_Reporting.Initialize;

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

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("deleting a simple Component, and then adding a new Component:");
   Delete_Component ("baz");
   Add_Component ("squeeg");

   Report_Problems;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("deleting a Component with defects:");
   Delete_Component ("foo");

   Report_Problems;

end Demo;
