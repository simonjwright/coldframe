with ColdFrame.Project.Events.Standard;
with GNAT.Exception_Traces;

with Top_Half.Initialize;
with Top_Half.Public;
with Top_Half.Bottom_Half.Initialize;

procedure Top_Half.Harness is

   Q : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Event_Queue;

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Top_Half.Initialize (Q);
   Top_Half.Bottom_Half.Initialize (Q);

   Top_Half.Public.Perform (42);

end Top_Half.Harness;
