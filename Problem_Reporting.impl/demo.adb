with Problem_Reporting.Component.Add_Component;
with Problem_Reporting.Problem_Report.Add_Problem;
with Problem_Reporting.Problem_Report.Note_Defect;
with Problem_Reporting.Problem_Report.Reject_Problem;
with Problem_Reporting.Component.Report_Problems;
procedure Demo is

  use Problem_Reporting.Component;
  use Problem_Reporting.Problem_Report;

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

end Demo;
