------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2004 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with Unchecked_Deallocation;

package body McKae.Space.Unbounded_String_Expandable is
   -- An expandable string type that automatically adds to its length
   --  when the allocated space has been used up.  More efficient than
   --  the standard Unbounded_Strings package because it will almost
   --  always allocate more space than is needed for whatever is being
   --  appended, so appending can continue for awhile before needing
   --  to allocate more space.

   -- Services provided are general string manipulation and extraction
   --  ones.

   ------------------------------------------------------------------------

   procedure Free is new Unchecked_Deallocation(String, String_Access);

   ------------------------------------------------------------------------

   procedure Copy (Target :    out Expandable_String;
                   Source : in     Expandable_String)
   is
   begin
      Target := Source;
      Target.S := new String(1 .. Target.Allocated);
      Target.S(1 .. Target.Length) := Source.S(1 .. Source.Length);
   end Copy;

   ------------------------------------------------------------------------

   procedure Append(Es   : in out Expandable_String;
                    Char : in     Character) is
   begin
      Append(Es, String'(1 => Char));
   end Append;

   ------------------------------------------------------------------------

   procedure Append(Es  : in out Expandable_String;
                    Str : in     String) is
      New_Size : constant Natural := Es.Length + Str'Length;

      Prev     : String_Access := Es.S;
   begin
      if New_Size > Es.Allocated then
         -- Don't just add in a new allocation size, add in the size
         --  of the extension PLUS the size of the appended string.
         Es.Allocated := New_Size + Es.Expansion;
         Es.S := new String(1 .. Es.Allocated);

         -- Copy over the contents to the new allocation
         Es.S(1 .. Es.Length) := Prev(1 .. Es.Length);

         Free(Prev);
      end if;

      -- Append the new part
      Es.S(Es.Length + 1 .. New_Size) := Str;
      Es.Length := New_Size;
   end Append;

   ------------------------------------------------------------------------

   procedure Append(Es      : in out Expandable_String;
                    More_ES : in     Expandable_String) is
   begin
      Append(Es, Extract(More_Es));
   end Append;

   ------------------------------------------------------------------------

   function Length(Es : Expandable_String) return Natural is
   begin
      return Es.Length;
   end Length;

   ------------------------------------------------------------------------

   function Get(Es    : Expandable_String;
                Index : Positive) return Character is
   begin
      if Index <= Es.Length then
         return Es.S(Index);
      else
         raise Constraint_Error;
      end if;
   end Get;

   ------------------------------------------------------------------------

   function Slice(Es : Expandable_String;
                  First : Positive;
                  Last  : Natural) return String is
   begin
      if First > Last then
         return "";
      elsif Last <= Es.Length then
         return Es.S(First .. Last);
      else
         raise Constraint_Error;
      end if;
   end Slice;

   ------------------------------------------------------------------------

   -- Return the whole valid part of the expandable string as a string
   function Extract(Es : Expandable_String) return String is
   begin
      return Es.S(1 .. Es.Length);
   end Extract;

   ------------------------------------------------------------------------

   -- Destroy the string and deallocate any storage
   procedure Destroy(ES : in out Expandable_String) is
   begin
      Es.Length := 0;
      Free(Es.S);
      Es.Allocated := 0;
      Es.S := null;
   end Destroy;

   ------------------------------------------------------------------------

   -- Clear the string; does not release any space.
   procedure Clear (ES : in out Expandable_String) is
   begin
      Es.Length := 0;
   end Clear;


   ------------------------------------------------------------------------

   function "=" (Left, Right : Expandable_String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) = Right.S(1 .. Right.Length);
   end "=";

   function "<" (Left, Right : Expandable_String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) < Right.S(1 .. Right.Length);
   end "<";

   function "<="(Left, Right : Expandable_String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) < Right.S(1 .. Right.Length);
   end "<=";

   function ">" (Left, Right : Expandable_String) return Boolean is
   begin
      return Right <= Left;
   end ">";

   function ">="(Left, Right : Expandable_String) return Boolean is
   begin
      return Right < Left;
   end ">=";

   function "=" (Left : Expandable_String; Right : String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) = Right;
   end "=";

   function "<" (Left : Expandable_String; Right : String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) < Right;
   end "<";

   function "<="(Left : Expandable_String; Right : String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) <= Right;
   end "<=";

   function ">" (Left : Expandable_String; Right : String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) > Right;
   end ">";

   function ">="(Left : Expandable_String; Right : String) return Boolean is
   begin
      return Left.S(1 .. Left.Length) >= Right;
   end ">=";

   ------------------------------------------------------------------------

end McKae.Space.Unbounded_String_Expandable;
