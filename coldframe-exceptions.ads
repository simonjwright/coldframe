--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-exceptions.ads,v $
--  $Revision: 3bb76df2b5fa $
--  $Date: 2002/07/05 05:53:03 $
--  $Author: simon $

package ColdFrame.Exceptions is

   Duplicate : exception;
   --  Attempt to Create an object with an identifier that belongs to an
   --  object that already exists

   Not_Found : exception;
   --  Attempt to access an object by identifier when no such object exists

   Existing_Child : exception;
   --  Attempt to replace a non-null child in an inheritance relationship

   Cant_Happen : exception;
   --  An unexpected Event has occured.

   Use_Error : exception;
   --  Misuse of facilities (eg, attempting to post Events for the
   --  same Instance to more than one Queue; attempting to set a Timer
   --  that's already set).

   No_Default_Create : exception;
   --  Raised in Inheritance.Create_Tree if a root instance needs to
   --  be created but the Create operation requires an identifier (ie,
   --  the identifier isn't Autonumber).

   Mismatched_Handles : exception;
   --  Raised in Inheritance.Create_Tree if a non-root class with
   --  multiple parents finds that all of the supplied handles match
   --  it (ie, the programmer is trying to designate an instance of
   --  this class as the required parent) and the handles are not all
   --  the same.

end ColdFrame.Exceptions;
