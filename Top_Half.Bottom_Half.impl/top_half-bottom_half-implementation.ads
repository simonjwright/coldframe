with Top_Half.Abstract_Bottom_Half;

private package Top_Half.Bottom_Half.Implementation is

   type Instance
   is new Top_Half.Abstract_Bottom_Half.Instance with null record;
   type Handle is access Instance;

   procedure Perform (This : Instance; X : Integer);

   procedure Initialize;

end Top_Half.Bottom_Half.Implementation;



