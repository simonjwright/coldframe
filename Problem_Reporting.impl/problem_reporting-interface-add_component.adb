with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Problem_Reporting.Component;

separate (Problem_Reporting.Interface)
procedure Add_Component
  (Named : String) is

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   H : Component.Handle;

begin

   --  Create the new Component
   H := Component.Create ((Id => +Named));

end Add_Component;
