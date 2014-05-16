--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package specifies trigger limits used by the implementation.

with System.Storage_Elements;

package ColdFrame.Project.Limits is

   function Monitor_Report_Trigger return Duration;
   --  If a Monitoring event queue fails to return to idle after this
   --  length of continous event processing, it will log a trace of
   --  the events involved.

   function Storage_Limit return System.Storage_Elements.Storage_Count;
   --  This is the limit on the storage allocated and not yet freed
   --  via an Unbounded_Debug_Storage_Pool which triggers it to dump
   --  the allocations made at each allocation site.

end ColdFrame.Project.Limits;
