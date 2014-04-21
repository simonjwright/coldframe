--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this procedure to exist to allow informational
--  messages to be reported (possibly for tracing).
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-log_info.adb,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

with ColdFrame.Project.Logging_Support;

procedure ColdFrame.Project.Log_Info (Message : String) is
begin
   ColdFrame.Project.Logging_Support.Log
     (Severity => Logging_Support.Info,
      Message => Message);
end ColdFrame.Project.Log_Info;
