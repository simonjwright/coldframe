with GNAT.IO; use GNAT.IO;
with Problem_Reporting.Defect.Iterate;
with Problem_Reporting.Defect.Vectors;
with Problem_Reporting.R100;

separate (Problem_Reporting.Component)
procedure Report
  (This : not null Handle) is
   procedure Report_Defects is new Defect.Iterate (Defect.Report);
   Defects : constant Defect.Vectors.Vector := R100.Affects (This);
begin
   Put_Line ("Component:" & To_String (This.ID));
   if Defects.Is_Empty then
      Put_Line (".. no defects.");
   else
      Report_Defects (Defects);
   end if;
end Report;
