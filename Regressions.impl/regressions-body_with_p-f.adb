separate (Regressions.Body_With_P)
function F
  (This : Handle)
  return ColdFrame.Instances.Handle is
   pragma Warnings (Off, This);
   A : Body_With_A.Handle;
--     B : Body_With_B.Handle;
   C : Body_With_C.Handle;
   pragma Warnings (Off, A);
--     pragma Warnings (Off, B);
   pragma Warnings (Off, C);
begin
   return null;
end F;
