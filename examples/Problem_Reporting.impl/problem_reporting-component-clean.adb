with ColdFrame.Instances;
with GNAT.IO;
with Problem_Reporting.Defect.Iterate;
with Problem_Reporting.Defect.Vectors;
with Problem_Reporting.Diagnosed_Problem_Report.Iterate;
with Problem_Reporting.Diagnosed_Problem_Report.Vectors;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.R100.From_Vectors;
with Problem_Reporting.Unallocated_Problem_Report;

--  Maintain referential integrity when the Component is deleted, by
--  removing any associated Defects.

--  Note that this will require deleting any of the associated
--  Diagnosed Problem Reports that are left referring to no Defects
--  (we'll migrate them to Unallocated Problem Reports).

separate (Problem_Reporting.Component)
procedure Clean
  (This : not null Handle) is

   --  Delete all the Defects that are associated with this Component
   procedure Delete (D : Defect.Handle);
   procedure Delete is new Defect.Iterate (Delete);

   --  Check if any of the Diagnosed_Problem_Reports that were
   --  associated with this Component are now associated with no
   --  Components, and of so revert them to
   --  Unallocated_Problem_Reports.
   procedure Check (DPR : Diagnosed_Problem_Report.Handle);
   procedure Check is new Diagnosed_Problem_Report.Iterate (Check);

   Defects : constant Defect.Vectors.Vector := R100.Affects (This);
   DPRs : constant Diagnosed_Problem_Report.Vectors.Vector
     := R100.From_Vectors.Affects (Defects);

   procedure Delete (D : Defect.Handle) is
      That : Defect.Handle := D;
   begin
      R100.Unlink (That);
      Defect.Delete (That);
   end Delete;

   procedure Check (DPR : Diagnosed_Problem_Report.Handle) is
      This_DPRs_Defects : constant Defect.Vectors.Vector
        := R100.Is_Affected_By (DPR);
   begin
      if This_DPRs_Defects.Is_Empty then
         declare
            --  We must have a variable to pass to Delete()
            That : Diagnosed_Problem_Report.Handle := DPR;
            --  Remember the PR so we can migrate the child to Unallocated
            PR : constant Problem_Report.Handle :=
              Problem_Report.Handle
                (Diagnosed_Problem_Report.Get_R1_Parent (DPR));
            UPR : Unallocated_Problem_Report.Handle;
            pragma Unreferenced (UPR);
         begin
            GNAT.IO.Put_Line (To_String (This.ID)
                                & ": deleting orphaned DPR:"
                                & Diagnosed_Problem_Report.Get_ID (DPR)'Img);
            Diagnosed_Problem_Report.Delete (That);
            UPR := Unallocated_Problem_Report.Create
              ((R1_Parent => ColdFrame.Instances.Handle (PR)));
         end;
      end if;
   end Check;

begin
   Delete (Defects);
   Check (DPRs);
end Clean;
