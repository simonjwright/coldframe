with GNAT.IO; use GNAT.IO;

with Problem_Reporting.Defect.Collections;
with Problem_Reporting.Defect.Iterate;
with Problem_Reporting.R100;

separate (Problem_Reporting.Component)
procedure Report
  (This : Handle) is
   procedure Report_Defects is new Defect.Iterate (Defect.Report);
   Defects : constant Defect.Collections.Collection :=
     R100.Affects (This);
begin
   Put_Line ("Component:" & To_String (This.Id));
   if Defect.Collections.Length (Defects) = 0 then
      Put_Line (".. no defects.");
   else
      Report_Defects (Defects);
   end if;
end Report;
