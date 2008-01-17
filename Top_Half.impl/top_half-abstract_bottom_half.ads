package Top_Half.Abstract_Bottom_Half is

   type Instance is abstract tagged null record;
   type Handle is access all Instance'Class;

   procedure Perform (This : Instance; X : Integer) is abstract;

   Implementation : Handle;

end Top_Half.Abstract_Bottom_Half;
