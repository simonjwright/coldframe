with Ada.Calendar;
with Ada.Real_Time;
with ColdFrame.Time_Signature;

package ColdFrame.Project.Event_Support is

   type Time_Kind is (Calendar);

   type Time (Kind : Time_Kind := Calendar) is record
      T : Ada.Calendar.Time;
   end record;

   --  type Time (Kind : Time_Kind := Calendar) is private;

   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time;

   function From_Now (Period : Duration) return Time;

   package Signature is new ColdFrame.Time_Signature
     (Time => Time,
      Equivalent => Equivalent,
      From_Now => From_Now);

end ColdFrame.Project.Event_Support;
