with ColdFrame.Instances;

with Problem_Reporting.Rejected_Problem_Report;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Unallocated_Problem_Report;

separate (Problem_Reporting.Interface)
procedure Reject_Problem
  (Number : Integer;
   Reason : String) is

   H : Problem_Report.Handle;
   R : Rejected_Problem_Report.Handle;

begin

   --  Find the indicated Problem Report.
   H := Problem_Report.Find ((Id => Number));

   --  Subtype migration to Rejected Problem Report.

   --  Delete the existing Unallocated Problem Report.
   Unallocated_Problem_Report.Delete
     ((R1_Parent => ColdFrame.Instances.Handle (H)));

   --  Create the new Rejected Problem Report.
   R := Rejected_Problem_Report.Create
     ((R1_Parent => ColdFrame.Instances.Handle (H)));

   --  Store the additional details.
   Rejected_Problem_Report.Set_Rejection_Reason
     (R, Summary_String_Package.To_Bounded_String (Reason));

end Reject_Problem;
