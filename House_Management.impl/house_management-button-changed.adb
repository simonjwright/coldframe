--  Acts as receiver of state changes from Digital IO.

separate (House_Management.Button)
procedure Changed
  (S : Signal_State) is

   subtype Floors is Digital_IO.Signal_Name
     range Digital_IO.Floor_0 .. Digital_IO.Floor_3;

   Buttons : constant array (Floors) of Button_Name
     := (Digital_IO.Floor_0 => Second_Floor,
         Digital_IO.Floor_1 => First_Floor,
         Digital_IO.Floor_2 => Ground_Floor,
         Digital_IO.Floor_3 => Basement);

begin

   if S.S in Floors then

      if S.State then

         Pushed (Find ((Name =>Buttons (S.S))));

      end if;

   else
      raise Constraint_Error;
   end if;

end Changed;
