separate (Problem_Reporting.Problem_Report)
function Hash (Id : Identifier) return Natural is
begin
  return Id.Id * 10009;
end Hash;
