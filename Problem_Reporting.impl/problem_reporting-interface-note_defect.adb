with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io; use Ada.Strings.Unbounded.Text_Io;
with Ada.Text_Io; use Ada.Text_Io;
with Problem_Reporting.Component;
with Problem_Reporting.Defect;
with Problem_Reporting.Diagnosed_Problem_Report;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Unallocated_Problem_Report;
procedure Problem_Reporting.Interface.Note_Defect
  (Problem_Number : Integer;
   Component_Name : String;
   Description : String) is

  function "+" (Source : String) return Unbounded_String
    renames To_Unbounded_String;

  H : Problem_Report.Handle;
  D : Diagnosed_Problem_Report.Handle;
  C : Problem_Reporting.Component.Handle;
  Def : Defect.Handle;

begin

  -- Find the indicated Component.
  H := Problem_Report.Find ((Id => Problem_Number));

  case Problem_Report.Get_Child_Class (H) is

    when Problem_Report.Unallocated_Problem_Report_T =>

      -- Subtype migration: the Problem Report becomes Diagnosed.

      -- Delete the old Unallocated Problem Report.
      Unallocated_Problem_Report.Delete ((R1_Child_Of_PR => H));

      -- Indicate that this Problem Report's subtype is Diagnosed.
      Problem_Report.Set_Child_Class
         (H, Problem_Report.Diagnosed_Problem_Report_T);

      -- Create the new Diagnosed Problem Report.
      D := Diagnosed_Problem_Report.Create ((R1_Child_Of_PR => H));

    when Problem_Report.Rejected_Problem_Report_T =>

      -- If the Problem Report was Rejected, we can't accept any
      -- further diagnosis.
      Put ("problem" & Problem_Report.Get_Id (H)'Img & "(");
      Put (Problem_Report.Get_Details (H));
      Put_Line (") already rejected.");
      return;

    when Problem_Report.Diagnosed_Problem_Report_T =>

      -- Find the existing Diagnosed Problem Report.
      D := Diagnosed_Problem_Report.Find ((R1_Child_Of_PR => H));

  end case;

  -- Find the indicated Component.
  C := Problem_Reporting.Component.Find ((Id => +Component_Name));

  -- Create a new Defect.
  Def := Defect.Create ((R100_Affects_C => C,
                         R100_Is_Affected_By_DPR => D));

  -- Store the diagnosed Problem in the new Defect.
  Defect.Set_Problem (Def, +Description);

end Problem_Reporting.Interface.Note_Defect;
