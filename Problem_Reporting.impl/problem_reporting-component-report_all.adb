with Problem_Reporting.Component.All_Instances;
with Problem_Reporting.Component.Iterate;

separate (Problem_Reporting.Component)
procedure Report_All is

   procedure Report_All is new Iterate (Report);

begin

   Report_All (All_Instances);

end Report_All;
