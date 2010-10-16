--  $RCSfile: compilation_regressions-has_event_named_next-caller.adb,v $
--  $Revision: 493ac18505f7 $
--  $Date: 2010/10/16 20:08:08 $
--  $Author: simonjwright $
--
--  Regression tests for ColdFrame.
--
--  The units with'ed by the body only have to compile.

with Compilation_Regressions.Has_Event_Named_Next.All_Instances;
with Compilation_Regressions.Has_Event_Named_Next.Selection_Function;
with Compilation_Regressions.Has_Event_Named_Next.CF_Tear_Down;
separate (Compilation_Regressions.Has_Event_Named_Next)
procedure Caller
  (This : Handle) is
   Unimplemented : exception;
begin
   raise Unimplemented;
end Caller;
