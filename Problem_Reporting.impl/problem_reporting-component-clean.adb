with Problem_Reporting.Defect.Collections;
with Problem_Reporting.Defect.Iterate;
with Problem_Reporting.R100;

separate (Problem_Reporting.Component)
procedure Clean
  (This : Handle) is
   procedure Delete (D : Defect.Handle);
   procedure Delete_Defects is new Defect.Iterate (Delete);
   procedure Delete (D : Defect.Handle) is
      That : Defect.Handle := D;
   begin
      R100.Unlink (That);
      Defect.Delete (That);
   end Delete;
   Defects : constant Defect.Collections.Collection :=
     R100.Affects (This);
begin
   Delete_Defects (Defects);
end Clean;
