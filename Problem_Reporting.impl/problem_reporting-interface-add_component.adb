with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Problem_Reporting.Component;
procedure Problem_Reporting.Interface.Add_Component
   (Named : String) is

  function "+" (Source : String) return Unbounded_String
    renames To_Unbounded_String;

  H : Component.Handle;

begin

  -- Create the new Component
  H := Component.Create ((Id => +Named));

end Problem_Reporting.Interface.Add_Component;
