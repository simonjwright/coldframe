with Hierarchies.F_2.Inheritance;

separate (Hierarchies.F_2)
function Create_New
  return Handle is
begin
   return Inheritance.Create_Tree (null, null, null);
end Create_New;
