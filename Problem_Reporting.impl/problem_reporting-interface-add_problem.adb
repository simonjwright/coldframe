with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ColdFrame.Instances;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Unallocated_Problem_Report;

separate (Problem_Reporting.Interface)
procedure Add_Problem
  (Number : Integer;
   Reporter : String;
   Details : String) is

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   H : Problem_Report.Handle;
   U : Unallocated_Problem_Report.Handle;

begin

   --  Create a new Problem Report
   H := Problem_Report.Create ((Id => Number));

   --  Store the additional information
   Problem_Report.Set_Details (H, +Details);
   Problem_Report.Set_Reporter (H, +Reporter);

   --  Create the corresponding Unallocated Problem Report
   U := Unallocated_Problem_Report.Create
     ((R1_Parent => ColdFrame.Instances.Handle (H)));

end Add_Problem;
