package body ColdFrame.Project.Event_Support is


   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time is
      use type Ada.Calendar.Time;
      use type Ada.Real_Time.Time;
   begin
      case Of_Time.Kind is
         when Calendar =>
            declare
               Offset : constant Duration
                 := Of_Time.T - Ada.Calendar.Clock;
            begin
               return Ada.Real_Time.Clock
                 + Ada.Real_Time.To_Time_Span (Offset);
            end;
      end case;
   end Equivalent;


   function From_Now (Period : Duration) return Time is
      use type Ada.Calendar.Time;
   begin
      return Time'(Kind => Calendar, T => Ada.Calendar.Clock + Period);
   end From_Now;


end ColdFrame.Project.Event_Support;
