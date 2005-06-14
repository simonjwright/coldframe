with Regressions.Test_Preemption;

separate (Regressions.Rule)
procedure Create
  (Preempter : Test_Name;
   Preemptee : Test_Name;
   Preempts : Boolean := False) is
   MP : Preemptable_Test.Handle;
   MBP : Preemptable_Test.Handle;
   H : Handle;
   use type Preemptable_Test.Handle;
begin
   MP := Preemptable_Test.Find ((Name => Preempter));
   if MP = null then
      MP := Preemptable_Test.Create ((Name => Preempter));
   end if;
   MBP := Preemptable_Test.Find ((Name => Preemptee));
   if MBP = null then
      MBP := Preemptable_Test.Create ((Name => Preemptee));
   end if;
   H := Test_Preemption.Link
     (Conflicts_With_And_May_Preempt => MBP,
      Conflicts_With_And_May_Be_Preempted_By => MP);
   H.Preempts := Preempts;
end Create;
