with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Problem_Reporting.Component;

separate (Problem_Reporting.Interface)
procedure Delete_Component
  (Component_Name : String) is

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

begin

   Component.Delete ((Id => +Component_Name));

end Delete_Component;
