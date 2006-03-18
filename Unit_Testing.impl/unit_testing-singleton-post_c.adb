with ColdFrame.Project.Events;
with Unit_Testing.Events;

separate (Unit_Testing.Singleton)
procedure Post_C is
begin
   ColdFrame.Project.Events.Set (S,
                                 On => Events.Dispatcher,
                                 To_Fire => new C,
                                 After => 1.0);
end Post_C;
