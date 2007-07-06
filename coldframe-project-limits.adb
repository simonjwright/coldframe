--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-project-limits.adb,v $
--  $Revision: 4a01bc2157ee $
--  $Date: 2007/07/06 05:19:43 $
--  $Author: simonjwright $

package body ColdFrame.Project.Limits is

   subtype Milliseconds is Integer;


   Monitor_Report_Trigger_Ms : Milliseconds := 1_000;
   pragma Export (C, Monitor_Report_Trigger_Ms, "monitor_report_trigger");

   function Monitor_Report_Trigger return Duration is
   begin
      return Duration (Monitor_Report_Trigger_Ms) / 1_000.0;
   end Monitor_Report_Trigger;


end ColdFrame.Project.Limits;
