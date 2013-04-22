with Top_Half.Bottom_Half.Bottom;

package body Top_Half.Bottom_Half.Implementation is


   procedure Perform (This : Instance; X : Integer) is
      pragma Unreferenced (This);
   begin
      Bottom.Perform (X);
   end Perform;


   procedure Initialize is
   begin
      Top_Half.Abstract_Bottom_Half.Initialize (To_Use => new Instance);
   end Initialize;


end Top_Half.Bottom_Half.Implementation;
