--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project logging.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-logging_support.ads,v $
--  $Revision: e91d25f64219 $
--  $Date: 2002/10/01 17:43:10 $
--  $Author: simon $

with ColdFrame.Logging_Signature;

package ColdFrame.Project.Logging_Support is

   type Severity_Code is (Info, Error);

   procedure Log (Severity : Severity_Code; Message : String);

   package Implementation is new ColdFrame.Logging_Signature
     (Severity_Code => Severity_Code,
      Error => Error,
      Informational => Info,
      Log => Log);

end ColdFrame.Project.Logging_Support;
