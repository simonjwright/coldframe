with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Problem_Reporting.Component;
with Problem_Reporting.Problem_Report;

separate (Problem_Reporting.Public)
procedure Report_Problems is

begin

   --  Process all the Components.
   Component.Report_All;

end Report_Problems;
