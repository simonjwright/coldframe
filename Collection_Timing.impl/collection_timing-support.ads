with ColdFrame.Instances.Collections;
with Collection_Timing.B_B.Collections;
with Collection_Timing.B_U.Collections;

private package Collection_Timing.Support is

   package CICB renames ColdFrame.Instances.Collections.Bounded_Collections;
   Bounded : CICB.Unconstrained_Collection (33);

   package CICU renames ColdFrame.Instances.Collections.Unbounded_Collections;
   Unbounded : CICU.Collection;

end Collection_Timing.Support;
