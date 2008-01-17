with Top_Half.Abstract_Bottom_Half;

separate (Top_Half.Public)
procedure Perform
  (X : Integer) is
   pragma Assert
     (Domain_Initialized,
      "Top_Half not initialized");
begin
   Abstract_Bottom_Half.Perform (Abstract_Bottom_Half.Implementation.all, X);
end Perform;
