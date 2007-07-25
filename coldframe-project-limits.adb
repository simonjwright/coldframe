--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-project-limits.adb,v $
--  $Revision: cfe01ded7537 $
--  $Date: 2007/07/25 06:13:30 $
--  $Author: simonjwright $

package body ColdFrame.Project.Limits is

   subtype Milliseconds is Integer;
   subtype Kilobytes is Integer;


   Monitor_Report_Trigger_Ms : Milliseconds := 1_000;
   pragma Export (C, Monitor_Report_Trigger_Ms, "monitor_report_trigger");

   function Monitor_Report_Trigger return Duration is
   begin
      return Duration (Monitor_Report_Trigger_Ms) / 1_000.0;
   end Monitor_Report_Trigger;


   Storage_Limit_Kb : Kilobytes := 1_024;
   pragma Export (C, Storage_Limit_Kb, "storage_limit");

   function Storage_Limit return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Count;
   begin
      return System.Storage_Elements.Storage_Count (Storage_Limit_Kb) * 1_024;
   end Storage_Limit;


end ColdFrame.Project.Limits;
