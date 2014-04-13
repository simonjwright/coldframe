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

--  $RCSfile: coldframe-stubs.ads,v $
--  $Revision: ae12a49c4b3a $
--  $Date: 2014/04/13 13:27:09 $
--  $Author: simonjwright $

with Ada.Exceptions;
with Ada.Streams;
with ColdFrame.Synchronization;

package ColdFrame.Stubs is


   --  Values are stored using Streams. For most uses, the space
   --  required for the streamed representation will be less than that
   --  for the in-store representation; however, for indefinite types,
   --  there is an additional overhead required to specify the actual
   --  object's constraints. This value (a number of bytes) should be
   --  enough for all but extreme cases.
   Storage_Overhead : constant := 128;


   --  Raised if a referenced subprogram isn't known.
   No_Subprogram : exception;


   --  Raised if a referenced parameter isn't known, or if an attempt
   --  is made to set the return value of a procedure.
   No_Parameter : exception;


   --  Raised if a required return or (in)out value is not found.
   No_Value : exception;


   --  Raised by Set_Output_Value or Set_Exception if a return or
   --  (in)out value or an exception has already been set and the
   --  'Override' parameter is False.
   Already_Set : exception;


   --  Raised by Set_Output_Value or Set_Exception if a return or
   --  (in)out value or an exception has not already been set and the
   --  'Override' parameter is True.
   Not_Already_Set : exception;


   -------------------------------
   --  T e s t   c o n t r o l  --
   -------------------------------

   --  Initialize storage.
   procedure Set_Up;


   --  Free all storage.
   procedure Tear_Down;


   -------------------------------------------------------------
   --  O p e r a t i o n s   f o r   u s e r   s u p p o r t  --
   -------------------------------------------------------------

   --  Specify an output from a call to a stubbed operation for the
   --  type T.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  Normally the named parameter will be an "out" (perhaps "in
   --  out") parameter. The specified "To" value will be returned on
   --  the "For_Call"th call, and all subsequent calls until another
   --  "Set_Output_Value" call for the same parameter; if you want to
   --  have the first 4 calls to Domain.Class.Operation to set Output
   --  to 4, and any later ones to set it to 42, you'd say
   --
   --     Set_Integer ("Domain.Class.Operation", "Output", 4, 1);
   --     Set_Integer ("Domain.Class.Operation", "Output", 42, 5);
   --
   --  A special parameter name is "return". For "return", the To
   --  value will be the function result.
   --
   --  A previously stored value for a particular call can only be
   --  overridden if Override is True (when it must be).
   --
   --  Overhead_Bytes is the additional space reserved for the
   --  streamed representation.
   generic
      type T (<>) is private;
   procedure Set_Output_Value (For_Subprogram_Named : String;
                               For_Parameter_Named : String;
                               To : T;
                               For_Call : Positive := 1;
                               Override : Boolean := False;
                               Overhead_Bytes : Natural := Storage_Overhead);


   --  Specify that a call to a stubbed operation is to raise an
   --  exception.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  Normally the exception will be raised for the specified call
   --  and all later calls; to stop this, use Ada.Exceptions.Null_Id.
   --
   --  A previously stored exception for a particular call can only be
   --  overridden if Override is True (when it must be).
   procedure Set_Exception (For_Subprogram_Named : String;
                            E : Ada.Exceptions.Exception_Id;
                            For_Call : Positive := 1;
                            Override : Boolean := False);


   --  Retrieve the number of calls made to the named subprogram.
   function Number_Of_Calls (For_Subprogram_Named : String) return Natural;


   --  Retrieve values passed to stubbed operations for the type T.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  The named parameter will be an "in" (perhaps "in out")
   --  parameter. To retrieve the value passed at the second call,
   --  you'd say
   --
   --     Result := Get_Integer
   --       ("Domain.Class.Operation", "Input", 2);
   --
   --  To retrieve the result of the last call, say
   --
   --     Result := Get_Integer
   --       ("Domain.Class.Operation", "Input", 0);
   --
   --  To retrieve the value passed at the last call but one, say
   --
   --     Result := Get_Integer
   --       ("Domain.Class.Operation", "Input", -1);
   generic
      type T (<>) is private;
   function Get_Input_Value (For_Subprogram_Named : String;
                             For_Parameter_Named : String;
                             For_Call : Integer := 0) return T;

   --  A retained facility for retrieving the value passed at the last
   --  call:
   --
   --     Result := Get_Integer
   --       ("Domain.Class.Operation", "Input", Last);
   Last : constant Positive := Positive'Last;


   -----------------------------------------------------------------
   --  O p e r a t i o n s   f o r   g e n e r a t e d   c o d e  --
   -----------------------------------------------------------------

   --  Called during elaboration (of generated code) to register the
   --  existence of a subprogram.
   procedure Register_Subprogram (Named : String);


   --  Called during elaboration (of generated code) to register the
   --  existence of a subprogram input parameter.
   procedure Register_Input_Parameter (Subprogram_Named : String;
                                       Parameter_Named : String);


   --  Called during elaboration (of generated code) to register the
   --  existence of a subprogram output parameter (or function return).
   procedure Register_Output_Parameter (Subprogram_Named : String;
                                        Parameter_Named : String);


   --  Local type for access to internal streams.
   type Stream_Access is access Ada.Streams.Root_Stream_Type'Class;


   --  Must be called at entry to a subprogram to prepare for
   --  recording and retrieving data for a call.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram.
   --
   --  Returns the call number (the number of times the subprogram has
   --  been called since the last Tear_Down, including this call).
   function Note_Entry (For_Subprogram_Named : String) return Positive;


   --  Called to get the stream into which the For_Call'th input value
   --  of For_Parameter_Named for the subprogram For_Subprogram_Named
   --  is to be stored, using type-name'Output.
   --
   --  Size_In_Bits is the size of the object to be streamed; 'Size
   --  will do.
   --
   --  Overhead_Bytes is the additional space reserved for the
   --  streamed representation.
   function Get_Input_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named : String;
      For_Call : Positive;
      Size_In_Bits : Natural;
      Overhead_Bytes : Natural := Storage_Overhead)
     return Stream_Access;


   --  Called (after all input values have been saved) to check
   --  whether any exception is to be raised and, if so, raise it.
   --
   --  Exceptions are to be stored by using Set_Output_Value with an
   --  Exception_ID and the special parameter name "exception".
   procedure Check_For_Exception (For_Subprogram_Named : String;
                                  For_Call : Positive);


   --  Called (after all input values have been saved and exceptions
   --  checked for) to get the stream from which the For_Call'th
   --  output value of For_Parameter_Named for the subprogram
   --  For_Subprogram_Named is to be retrieved, using type-name'Input.
   --
   --  For function return values, the values are to be stored by
   --  using Set_Output_Value with the special parameter name
   --  "return".
   function Get_Output_Value_Stream (For_Subprogram_Named : String;
                                     For_Parameter_Named : String;
                                     For_Call : Positive)
                                    return Stream_Access;


   --  Used for mutual exclusion via the RAII (Resource Acquisition Is
   --  Initialization) pattern.
   --
   --  declare
   --     L : Lock (Mutex'Access);
   --     pragma Unreferenced (L);
   --     --  now locked
   --  begin
   subtype Lock is ColdFrame.Synchronization.Lock;
   Mutex : aliased ColdFrame.Synchronization.Semaphore;


end ColdFrame.Stubs;
