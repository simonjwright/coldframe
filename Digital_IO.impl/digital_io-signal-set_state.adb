separate (Digital_IO.Signal)
procedure Set_State
  (This : Handle;
   To : Boolean) is
begin

   This.State := To;

   Changed (This);

end Set_State;
