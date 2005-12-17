separate (Initialization.Checking_Domain_Initialization)
task body T is
begin

   while not Domain_Initialized loop
      delay 0.1;
   end loop;

   case Initialized is
      when False =>
         Initialized_As_Seen := False;
      when True =>
         Initialized_As_Seen := True;
   end case;

end T;
