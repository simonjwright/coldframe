with Architecture.String_Hash;

separate (Problem_Reporting.Component)
function Hash (Id : Identifier) return Natural is
begin
  return Architecture.String_Hash.Hash (Id.Id);
end Hash;
