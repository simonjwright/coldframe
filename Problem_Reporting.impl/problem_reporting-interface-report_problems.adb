with Ada.Strings.Unbounded.Text_Io; use Ada.Strings.Unbounded.Text_Io;
with Ada.Text_Io; use Ada.Text_Io;
with Problem_Reporting.Component;
with Problem_Reporting.Component.Abstract_Containers;
with Problem_Reporting.Component.Collections;
with Problem_Reporting.Component.All_Instances;
with Problem_Reporting.Defect;
with Problem_Reporting.Defect.Abstract_Containers;
with Problem_Reporting.Defect.Collections;
with Problem_Reporting.Defect.Selection_Function;
with Problem_Reporting.Diagnosed_Problem_Report;
with Problem_Reporting.Problem_Report;

separate (Problem_Reporting.Interface)
procedure Report_Problems is

  -- Process a single Component.
  procedure Process_Component (C : Component.Handle; OK : out Boolean);

  -- Iterate over collections of Components.
  procedure Visit_Components
  is new Component.Abstract_Containers.Visit (Process_Component);

  -- Process a single Component.
  procedure Process_Component (C : Component.Handle; OK : out Boolean) is

    -- Is a Defect interesting?
    function Select_Defect (D : Defect.Handle) return Boolean;

    -- Select interesting Defects.
    function Select_Defects
    is new Defect.Selection_Function (Select_Defect);

    -- Process a single Defect.
    procedure Process_Defect (D : Defect.Handle; OK : out Boolean);

    -- Iterate over collections of Defects.
    procedure Visit_Defects
    is new Defect.Abstract_Containers.Visit (Process_Defect);

    -- Is a Defect interesting?
    function Select_Defect (D : Defect.Handle) return Boolean is
    begin
      -- yes, if its Component is the one we're presently processing.
      return Component."=" (Defect.Get_R100_Affects_C (D), C);
    end Select_Defect;

    -- Process a single Defect
    procedure Process_Defect (D : Defect.Handle; OK : out Boolean) is
      Diag : constant Diagnosed_Problem_Report.Handle
         := Defect.Get_R100_Is_Affected_By_DPR (D);
      Pr : constant Problem_Report.Handle
         := Diagnosed_Problem_Report.Get_R1_Child_Of_PR (Diag);
    begin

      -- OK to continue with iteration
      OK := True;

      -- Print the Problem_Report's number and details, and the
      -- Defect's problem.
      Put ("  problem");
      Put (Problem_Report.Get_Id (Pr)'Img);
      Put (": ");
      Put (Problem_Report.Get_Details (Pr));
      Put (": ");
      Put_Line (Defect.Get_Problem (D));

    end Process_Defect;

    -- Select the defects related to this Component.
    Defs : constant Defect.Collections.Collection := Select_Defects;
    Def_Iter : Defect.Abstract_Containers.Iterator'Class
       := Defect.Collections.New_Iterator (Defs);

  begin

    -- OK to continue with iteration.
    OK := True;

    -- Print details of the Component.
    Put ("Component: ");
    Put_Line (Component.Get_Id (C));

    -- Print details of associated Defects (if any)
    if Defect.Collections.Length (Defs) = 0 then
      Put_Line (".. no defects.");
    else
      Visit_Defects (Def_Iter);
    end if;

  end Process_Component;

  -- Collect all the Components.
  Comps : constant Component.Collections.Collection := Component.All_Instances;
  Comp_Iter : Component.Abstract_Containers.Iterator'Class
     := Component.Collections.New_Iterator (Comps);

begin

  -- Process all the Components.
  Visit_Components (Comp_Iter);

end Report_Problems;
