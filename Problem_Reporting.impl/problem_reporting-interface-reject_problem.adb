with Problem_Reporting.Rejected_Problem_Report;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Unallocated_Problem_Report;
procedure Problem_Reporting.Interface.Reject_Problem
   (Number : Integer;
    Reason : String) is

  H : Problem_Report.Handle;
  R : Rejected_Problem_Report.Handle;

begin

  -- Find the indicated Problem Report.
  H := Problem_Report.Find ((Id => Number));

  -- Subtype migration to Rejected Problem Report.

  -- Delete the existing Unallocated Problem Report.
  Unallocated_Problem_Report.Delete ((R1_Child_Of_PR => H));

  -- Indicate that the new subtype is Rejected.
  Problem_Report.Set_R1_Child_Class
     (H, Problem_Report.Rejected_Problem_Report_T);

  -- Create the new Rejected Problem Report.
  R := Rejected_Problem_Report.Create ((R1_Child_Of_PR => H));

  -- Store the additional details.
  Rejected_Problem_Report.Set_Rejection_Reason
     (R, Summary_String_Package.To_Bounded_String (Reason));

end Problem_Reporting.Interface.Reject_Problem;
