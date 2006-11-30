--  Check that we can take 'Access of aliased attributes.

separate (Compilation_Regressions.Aliased_Components)
procedure Check_Access
  (This : Handle) is
   type Integer_P is access all Integer;
   R : Record_With_Aliased_Component := (I => 42);
   B : Aliasable_Boolean_P;
   I : Integer_P;
   pragma Unreferenced (B, I);
begin
   B := This.Ins_B'Access;
   I := This.Ins_I'Access;
   B := Cls_B'Access;
   I := Cls_I'Access;
   I := R.I'Access;
end Check_Access;