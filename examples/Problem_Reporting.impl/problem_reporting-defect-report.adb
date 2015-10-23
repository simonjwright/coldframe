with GNAT.IO; use GNAT.IO;

with Problem_Reporting.Diagnosed_Problem_Report;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Defect;

separate (Problem_Reporting.Defect)
procedure Report
  (This : not null Handle) is
   DPR : constant Diagnosed_Problem_Report.Handle := Defect.Affects (This);
   PR : constant Problem_Report.Handle :=
     Problem_Report.Handle (Diagnosed_Problem_Report.Get_R1_Parent (DPR));
begin
   Put ("  problem");
   Put (Problem_Report.Get_ID (PR)'Img);
   Put (": ");
   Put (To_String (Problem_Report.Get_Details (PR)));
   Put (": ");
   Put (To_String (Get_Problem (This)));
   New_Line;
end Report;
