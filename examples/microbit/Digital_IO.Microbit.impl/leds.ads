package LEDs is

   --  The LEDs are displayed in a (5, 5) array.
   type Coord is range 1 .. 5;

   procedure Clear;

   procedure Set (At_Row : Coord; At_Col : Coord; To : Boolean);

   procedure Initialize;

end LEDs;
