--  $Id: digital_io-signal-set_state.adb,v 3f55fc632b38 2003/01/07 20:08:34 simon $

--  Set the signal's state as indicated; call Changed if it has
--  changed.

separate (Digital_IO.Signal)
procedure Set_State
  (This : Handle;
   To : Boolean) is
begin

   if To /= This.State then

      This.State := To;

      Changed (This);

   end if;

end Set_State;
