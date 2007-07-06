--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-project-limits.ads,v $
--  $Revision: 483875fae093 $
--  $Date: 2007/07/06 05:12:52 $
--  $Author: simonjwright $

--  This package specifies trigger limits used by the implementation.

package ColdFrame.Project.Limits is

   function Monitor_Report_Trigger return Duration;
   --  If a Monitoring event queue fails to return to idle after this
   --  length of continous event processing, it will log a trace of
   --  the events involved.

end ColdFrame.Project.Limits;
