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

--  $RCSfile: coldframe-stubs.adb,v $
--  $Revision: 4740bc252c95 $
--  $Date: 2005/02/24 20:54:32 $
--  $Author: simon $

with Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with BC.Containers.Bags.Unmanaged;
with BC.Containers.Collections.Unmanaged;
with BC.Containers.Collections.Ordered.Unmanaged;
with BC.Containers.Maps.Unmanaged;
with BC.Support.Memory_Streams;
with BC.Support.Smart_Pointers;
with ColdFrame.Hash.Strings.Unbounded;

package body ColdFrame.Test_Stub_Support is

   -------------------------------------
   --  D a t a   s t r u c t u r e s  --
   -------------------------------------

   --  We need to be case-insensitive when mapping from subprogram and
   --  parameter names.
   function Case_Insensitive_Equality
     (L, R : Ada.Strings.Unbounded.Unbounded_String) return Boolean;
   function Case_Insensitive_Hash
     (S : Ada.Strings.Unbounded.Unbounded_String) return Natural;


   --  We need to count subprogram entries.
   package Abstract_Unbounded_String_Containers
   is new BC.Containers
     (Item => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Case_Insensitive_Equality);
   package Abstract_Unbounded_String_Bags
   is new Abstract_Unbounded_String_Containers.Bags;
   package Unbounded_String_Bags
   is new Abstract_Unbounded_String_Bags.Unmanaged
     (Hash => Case_Insensitive_Hash,
      Buckets => 37);


   --  We need varying-capacity Memory Streams, so we need
   --  pointers. Use smart pointers so that the Streams get
   --  automatically freed when the Container they're in gets cleared.
   package Stream_Pointers
   is new BC.Support.Smart_Pointers (T => Ada.Streams.Root_Stream_Type'Class,
                                     P => Stream_Access);


   --  For Inputs, we need plain collections of Memory Streams (there
   --  will be an input for each occurrence).
   package Abstract_Stream_Pointer_Containers
   is new BC.Containers (Item => Stream_Pointers.Pointer,
                         "=" => Stream_Pointers."=");
   package Abstract_Stream_Pointer_Collections
   is new Abstract_Stream_Pointer_Containers.Collections;
   package Stream_Pointer_Collections
   is new Abstract_Stream_Pointer_Collections.Unmanaged;

   --  We need to hold mutable instances in containers, so we need
   --  pointers.
   type Stream_Pointer_Collection_Access
      is access Stream_Pointer_Collections.Collection;
   package Stream_Pointer_Collection_Pointers
   is new BC.Support.Smart_Pointers
     (T => Stream_Pointer_Collections.Collection,
      P => Stream_Pointer_Collection_Access);

   --  We need to map from subprogram.parameter names to collections
   --  of Memory Streams.
   package Abstract_Stream_Pointer_Collection_Containers
   is new BC.Containers (Item => Stream_Pointer_Collection_Pointers.Pointer,
                         "=" => Stream_Pointer_Collection_Pointers."=");
   package Abstract_Stream_Pointer_Collection_Maps
   is new Abstract_Stream_Pointer_Collection_Containers.Maps
     (Key => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Case_Insensitive_Equality);
   package Stream_Pointer_Collection_Maps
   is new Abstract_Stream_Pointer_Collection_Maps.Unmanaged
     (Hash => Case_Insensitive_Hash,
      Buckets => 37);


   --  For Exceptions, we need collections of Exception_Ids, organized to
   --  provide a sparse array. The collection is reverse-ordered, so
   --  that we can find the appropriate entry for a particular entry
   --  by iterating until we come to a cell whose Ordinal is less than
   --  or equal to the one we require.
   type Exception_Cell is record
      Ordinal : Positive;
      E : Ada.Exceptions.Exception_Id;
   end record;

   function "=" (L, R : Exception_Cell) return Boolean;
   function ">" (L, R : Exception_Cell) return Boolean;

   package Abstract_Sparse_Exception_Containers
   is new  BC.Containers (Item => Exception_Cell);
   package Abstract_Sparse_Exception_Collections
   is new Abstract_Sparse_Exception_Containers.Collections;
   package Abstract_Ordered_Sparse_Exception_Collections
   is new Abstract_Sparse_Exception_Collections.Ordered ("<" => ">");
   package Sparse_Exception_Collections
   is new Abstract_Ordered_Sparse_Exception_Collections.Unmanaged;

   --  We need to hold mutable instances in containers, so we need
   --  pointers.
   type Sparse_Exception_Collection_Access
      is access Sparse_Exception_Collections.Collection;
   package Sparse_Exception_Collection_Pointers
   is new BC.Support.Smart_Pointers
     (T => Sparse_Exception_Collections.Collection,
      P => Sparse_Exception_Collection_Access);

   package Abstract_Sparse_Exception_Collection_Containers
   is new BC.Containers
     (Item => Sparse_Exception_Collection_Pointers.Pointer,
      "=" => Sparse_Exception_Collection_Pointers."=");
   package Abstract_Sparse_Exception_Collection_Maps
   is new Abstract_Sparse_Exception_Collection_Containers.Maps
     (Key => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Case_Insensitive_Equality);
   package Sparse_Exception_Collection_Maps
   is new Abstract_Sparse_Exception_Collection_Maps.Unmanaged
     (Hash => Case_Insensitive_Hash,
      Buckets => 37);


   --  For Outputs, we need collections of Memory Streams, organized to
   --  provide a sparse array. The collection is reverse-ordered, so
   --  that we can find the appropriate entry for a particular entry
   --  by iterating until we come to a cell whose Ordinal is less than
   --  or equal to the one we require.
   type Output_Cell is record
      Ordinal : Positive;
      Stream : Stream_Pointers.Pointer;
   end record;

   function "=" (L, R : Output_Cell) return Boolean;
   function ">" (L, R : Output_Cell) return Boolean;

   package Abstract_Sparse_Stream_Pointer_Containers
   is new  BC.Containers (Item => Output_Cell);
   package Abstract_Sparse_Stream_Pointer_Collections
   is new Abstract_Sparse_Stream_Pointer_Containers.Collections;
   package Abstract_Ordered_Sparse_Stream_Pointer_Collections
   is new Abstract_Sparse_Stream_Pointer_Collections.Ordered ("<" => ">");
   package Sparse_Stream_Pointer_Collections
   is new Abstract_Ordered_Sparse_Stream_Pointer_Collections.Unmanaged;

   --  We need to hold mutable instances in containers, so we need
   --  pointers.
   type Sparse_Stream_Pointer_Collection_Access
      is access Sparse_Stream_Pointer_Collections.Collection;
   package Sparse_Stream_Pointer_Collection_Pointers
   is new BC.Support.Smart_Pointers
     (T => Sparse_Stream_Pointer_Collections.Collection,
      P => Sparse_Stream_Pointer_Collection_Access);

   package Abstract_Sparse_Stream_Pointer_Collection_Containers
   is new BC.Containers
     (Item => Sparse_Stream_Pointer_Collection_Pointers.Pointer,
      "=" => Sparse_Stream_Pointer_Collection_Pointers."=");
   package Abstract_Sparse_Stream_Pointer_Collection_Maps
   is new Abstract_Sparse_Stream_Pointer_Collection_Containers.Maps
     (Key => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Case_Insensitive_Equality);
   package Sparse_Stream_Pointer_Collection_Maps
   is new Abstract_Sparse_Stream_Pointer_Collection_Maps.Unmanaged
     (Hash => Case_Insensitive_Hash,
      Buckets => 37);


   Entries : Unbounded_String_Bags.Bag;
   Inputs : Stream_Pointer_Collection_Maps.Map;
   Exceptions : Sparse_Exception_Collection_Maps.Map;
   Outputs : Sparse_Stream_Pointer_Collection_Maps.Map;


   -------------------------------
   --  T e s t   c o n t r o l  --
   -------------------------------

   procedure Set_Up is
   begin
      Unbounded_String_Bags.Clear (Entries);
      Stream_Pointer_Collection_Maps.Clear (Inputs);
      Sparse_Exception_Collection_Maps.Clear (Exceptions);
      Sparse_Stream_Pointer_Collection_Maps.Clear (Outputs);
   end Set_Up;


   procedure Tear_Down is
   begin
      Unbounded_String_Bags.Clear (Entries);
      Stream_Pointer_Collection_Maps.Clear (Inputs);
      Sparse_Exception_Collection_Maps.Clear (Exceptions);
      Sparse_Stream_Pointer_Collection_Maps.Clear (Outputs);
   end Tear_Down;


   -------------------------------------------------------------
   --  O p e r a t i o n s   f o r   u s e r   s u p p o r t  --
   -------------------------------------------------------------

   procedure Set_Output_Value (For_Subprogram_Named : String;
                               For_Parameter_Named : String;
                               To : T;
                               For_Occurrence : Positive := 1) is
      pragma Assert (T'Constrained,
                     "Set_Output_Value's generic type must be constrained");
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      SPU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (SP);
      Str : constant Stream_Pointers.Pointer
        := Stream_Pointers.Create
        (new BC.Support.Memory_Streams.Stream_Type
           (Capacity => Ada.Streams.Stream_Element_Offset
              (T'Max_Size_In_Storage_Elements)));
      Coll : Sparse_Stream_Pointer_Collection_Pointers.Pointer;
   begin
      if not Sparse_Stream_Pointer_Collection_Maps.Is_Bound (Outputs, SPU) then
         Coll := Sparse_Stream_Pointer_Collection_Pointers.Create
           (new Sparse_Stream_Pointer_Collections.Collection);
         Sparse_Stream_Pointer_Collection_Maps.Bind (Outputs, SPU, Coll);
      else
         Coll := Sparse_Stream_Pointer_Collection_Maps.Item_Of (Outputs, SPU);
      end if;
      Sparse_Stream_Pointer_Collections.Append
        (Sparse_Stream_Pointer_Collection_Pointers.Value (Coll).all,
         Output_Cell'(For_Occurrence, Str));
      T'Write (Stream_Pointers.Value (Str), To);
   end Set_Output_Value;


   procedure Set_Exception (For_Subprogram_Named : String;
                            E : Ada.Exceptions.Exception_Id;
                            For_Occurrence : Positive := 1) is
      SEU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (For_Subprogram_Named);
      Coll : Sparse_Exception_Collection_Pointers.Pointer;
   begin
      if not Sparse_Exception_Collection_Maps.Is_Bound (Exceptions, SEU) then
         Coll := Sparse_Exception_Collection_Pointers.Create
           (new Sparse_Exception_Collections.Collection);
         Sparse_Exception_Collection_Maps.Bind (Exceptions, SEU, Coll);
      else
         Coll := Sparse_Exception_Collection_Maps.Item_Of (Exceptions, SEU);
      end if;
      Sparse_Exception_Collections.Append
        (Sparse_Exception_Collection_Pointers.Value (Coll).all,
         (Ordinal => For_Occurrence,
          E => E));
   end Set_Exception;


   function Get_Input_Value (For_Subprogram_Named : String;
                             For_Parameter_Named : String;
                             For_Occurrence : Positive := 1) return T is
      pragma Assert (T'Constrained,
                     "Get_Input_Value's generic type must be constrained");
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      SPU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (SP);
   begin
      if not Stream_Pointer_Collection_Maps.Is_Bound (Inputs, SPU) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Input " & SP & " not found");
      end if;
      declare
         Pointers : Stream_Pointer_Collections.Collection
           renames Stream_Pointer_Collection_Pointers.Value
           (Stream_Pointer_Collection_Maps.Item_Of (Inputs, SPU)).all;
         Str : Stream_Access
           renames Stream_Pointers.Value
           (Stream_Pointer_Collections.Item_At
              (Pointers, For_Occurrence));
      begin
         return T'Input (Str);
      end;
   end Get_Input_Value;


   -----------------------------------------------------------------
   --  O p e r a t i o n s   f o r   g e n e r a t e d   c o d e  --
   -----------------------------------------------------------------

   function Note_Entry (For_Subprogram_Named : String) return Positive is
      S : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (For_Subprogram_Named);
      Added : Boolean;
   begin
      Unbounded_String_Bags.Add (Entries, S, Added);
      return Unbounded_String_Bags.Count (Entries, S);
   end Note_Entry;


   function Get_Input_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named : String;
      For_Occurrence : Positive;
      Max_Size_In_Storage_Elements : Ada.Streams.Stream_Element_Offset := 512)
     return Stream_Access is
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      SPU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (SP);
      Str : Stream_Pointers.Pointer
        := Stream_Pointers.Create
        (new BC.Support.Memory_Streams.Stream_Type
           (Capacity => Max_Size_In_Storage_Elements));
      Coll : Stream_Pointer_Collection_Pointers.Pointer;
   begin
      if not Stream_Pointer_Collection_Maps.Is_Bound (Inputs, SPU) then
         Coll := Stream_Pointer_Collection_Pointers.Create
           (new Stream_Pointer_Collections.Collection);
         Stream_Pointer_Collection_Maps.Bind (Inputs, SPU, Coll);
      else
         Coll := Stream_Pointer_Collection_Maps.Item_Of (Inputs, SPU);
      end if;
      pragma Assert
        (For_Occurrence =
         Stream_Pointer_Collections.Length
         (Stream_Pointer_Collection_Pointers.Value (Coll).all) + 1,
         "mismatch in number of occurrences");
      Stream_Pointer_Collections.Append
        (Stream_Pointer_Collection_Pointers.Value (Coll).all, Str);
      return Stream_Pointers.Value (Str);
   end Get_Input_Value_Stream;


   procedure Check_For_Exception (For_Subprogram_Named : String;
                                  For_Occurrence : Positive) is
      SEU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (For_Subprogram_Named);
   begin
      if not Sparse_Exception_Collection_Maps.Is_Bound (Exceptions, SEU) then
         return;
      end if;
      declare
         Pointers : Sparse_Exception_Collections.Collection
           renames Sparse_Exception_Collection_Pointers.Value
           (Sparse_Exception_Collection_Maps.Item_Of (Exceptions, SEU)).all;
         It : Abstract_Sparse_Exception_Containers.Iterator'Class
           := Sparse_Exception_Collections.New_Iterator (Pointers);
         use Abstract_Sparse_Exception_Containers;
      begin
         while not Is_Done (It)
         loop
            if Current_Item (It).Ordinal <= For_Occurrence then
               declare
                  E : constant Ada.Exceptions.Exception_Id :=
                    Current_Item (It).E;
                  use type Ada.Exceptions.Exception_Id;
               begin
                  if E /= Ada.Exceptions.Null_Id then
                     Ada.Exceptions.Raise_Exception (E, "from stub");
                  else
                     exit;
                  end if;
               end;
            end if;
            Next (It);
         end loop;
      end;
   end Check_For_Exception;


   function Get_Output_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named : String;
      For_Occurrence : Positive)
     return Stream_Access is
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      SPU : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (SP);
   begin
      if not Sparse_Stream_Pointer_Collection_Maps.Is_Bound (Outputs, SPU) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Output " & SP & " not found");
      end if;
      declare
         Pointers : Sparse_Stream_Pointer_Collections.Collection
           renames Sparse_Stream_Pointer_Collection_Pointers.Value
           (Sparse_Stream_Pointer_Collection_Maps.Item_Of (Outputs, SPU)).all;
         It : Abstract_Sparse_Stream_Pointer_Containers.Iterator'Class
           := Sparse_Stream_Pointer_Collections.New_Iterator (Pointers);
         use Abstract_Sparse_Stream_Pointer_Containers;
      begin
         while not Is_Done (It)
         loop
            if Current_Item (It).Ordinal <= For_Occurrence then
               return Stream_Pointers.Value (Current_Item (It).Stream);
            end if;
            Next (It);
         end loop;
      end;
      raise Program_Error;
   end Get_Output_Value_Stream;


   -------------------------------------------------
   --  L o c a l   i m p l e m e n t a t i o n s  --
   -------------------------------------------------

   function Case_Insensitive_Equality
     (L, R : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return Translate (L, Ada.Strings.Maps.Constants.Lower_Case_Map)
        = Translate (R, Ada.Strings.Maps.Constants.Lower_Case_Map);
   end Case_Insensitive_Equality;


   function Case_Insensitive_Hash
     (S : Ada.Strings.Unbounded.Unbounded_String) return Natural is
      use Ada.Strings.Unbounded;
   begin
      return ColdFrame.Hash.Strings.Unbounded
        (Translate (S, Ada.Strings.Maps.Constants.Lower_Case_Map));
   end Case_Insensitive_Hash;


   function "=" (L, R : Exception_Cell) return Boolean is
   begin
      return L.Ordinal = R.Ordinal;
   end "=";


   function ">" (L, R : Exception_Cell) return Boolean is
   begin
      return L.Ordinal > R.Ordinal;
   end ">";


   function "=" (L, R : Output_Cell) return Boolean is
   begin
      return L.Ordinal = R.Ordinal;
   end "=";


   function ">" (L, R : Output_Cell) return Boolean is
   begin
      return L.Ordinal > R.Ordinal;
   end ">";


end ColdFrame.Test_Stub_Support;
