separate (Regressions.Body_With_C)
procedure G
  (This : Handle;
   P : ColdFrame.Instances.Handle) is
   pragma Warnings (Off, This);
   pragma Warnings (Off, P);
   A : Body_With_A.Handle;
   B : Body_With_B.Handle;
   M : Body_With_P.Handle;
   pragma Warnings (Off, A);
   pragma Warnings (Off, B);
   pragma Warnings (Off, M);
begin
   null;
end G;
