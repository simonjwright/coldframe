--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support serialization.
--
--  This is ColdFrame's default implementation, set to provide
--  deserialization (ie, generated Image operations produce the
--  XML-represented data, rather than an empty string).
--
--  To make target builds contain empty Image operations, with smaller
--  executables and no unused code, set Deserialization_Required to a
--  compile-time False; for example, if your target is a PowerPC and
--  your host is an x86, make it 'System.Default_Bit_Order =
--  System.Low_Order_First'.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with ColdFrame.Serialization;
with ColdFrame.Serialization_Signature;

package ColdFrame.Project.Serialization
is new ColdFrame.Serialization_Signature
  (Actual_Base => ColdFrame.Serialization.Base,
   Deserialization_Required => True);
