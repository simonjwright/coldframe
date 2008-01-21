package Top_Half.Abstract_Bottom_Half is

   type Instance is abstract tagged null record;
   type Handle is access all Instance'Class;

   --  A primitive operation, delegated to the mplementation. Must be
   --  visible so that extensions of Instance can see it.
   procedure Perform (This : Instance; X : Integer) is abstract;

   --  The corresponding user call.
   procedure Perform (X : Integer);

   --  Called by the actual implementation to register itself.
   procedure Initialize (To_Use : Handle);

end Top_Half.Abstract_Bottom_Half;
