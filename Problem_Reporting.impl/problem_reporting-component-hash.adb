with ColdFrame.String_Hash;

separate (Problem_Reporting.Component)
function Hash (Id : Identifier) return Natural is
begin
  return ColdFrame.String_Hash.Hash (Id.Id);
end Hash;
