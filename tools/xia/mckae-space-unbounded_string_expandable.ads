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

package McKae.Space.Unbounded_String_Expandable is
   -- An expandable string type that automatically adds to its length
   --  when the allocated space has been used up.  More efficient that
   --  the standard Unbounded_Strings package because it will almost
   --  always allocate more space than is needed for whatever is being
   --  appended.

   -- Services provided are general string manipulation and extraction
   --  ones.

   -- The expandable string type
   type Expandable_String (Initial_Size : Positive := 2000;
                           Expansion    : Positive := 2000) is private;

   -- Make a deep copy of the source string
   procedure Copy(Target :    out Expandable_String;
                  Source : in     Expandable_String);

   procedure Append(Es   : in out Expandable_String;
                    Char : in     Character);

   procedure Append(Es  : in out Expandable_String;
                    Str : in     String);

   procedure Append(Es      : in out Expandable_String;
                    More_ES : in     Expandable_String);

   function Length(Es : Expandable_String) return Natural;

   function Get(Es    : Expandable_String;
                Index : Positive) return Character;

   function Slice(Es : Expandable_String;
                  First : Positive;
                  Last  : Natural) return String;

   -- Return the whole valid part of the expandable string as a string
   function Extract(Es : Expandable_String) return String;

   procedure Destroy(ES : in out Expandable_String);
   -- Destroy the string and deallocate any storage

   procedure Clear (ES : in out Expandable_String);
   -- Clear the string; does not release any space.

   -- String comparisons
   function "=" (Left, Right : Expandable_String) return Boolean;
   function "<" (Left, Right : Expandable_String) return Boolean;
   function "<="(Left, Right : Expandable_String) return Boolean;
   function ">" (Left, Right : Expandable_String) return Boolean;
   function ">="(Left, Right : Expandable_String) return Boolean;

   function "=" (Left : Expandable_String; Right : String) return Boolean;
   function "<" (Left : Expandable_String; Right : String) return Boolean;
   function "<="(Left : Expandable_String; Right : String) return Boolean;
   function ">" (Left : Expandable_String; Right : String) return Boolean;
   function ">="(Left : Expandable_String; Right : String) return Boolean;


private
   type String_Access is access all String;
   --  General purpose string access type. Note that the caller is
   --  responsible for freeing allocated strings to avoid memory leaks.

   -- The implementation of the expandable string
   type Expandable_String (Initial_Size : Positive := 2000;
                           Expansion    : Positive := 2000) is
      record
         S         : String_Access := new String(1 .. Initial_Size);
         Length    : Natural       := 0;
         Allocated : Natural       := Initial_Size;
      end record;

end McKae.Space.Unbounded_String_Expandable;
