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

with Text_Io; use Text_IO;

package body Mckae.XML.XPath.Locations is

   use McKae.XML.XPath;

   -------------------------------------------------------------------

   Parsing_Path : Location_Paths;

   -------------------------------------------------------------------

   procedure Free is new
     Unchecked_Deallocation(Location_Path_Steps,
                            Location_Path_Steps_Handle);

   -------------------------------------------------------------------

   procedure Add (Location_Step : in     Location_Steps;
                  Location_Path : in out Location_Paths) is

      Curr_Path : Location_Path_Steps_Handle := Location_Path.Path;
      Steps     : Natural renames Location_Path.Steps;

   begin
      pragma Debug(Put_Line("Adding: " & To_String(Location_Step.Node_Test.Name)));

      if Location_Path.Steps = Location_Path.Path'Length then
         -- Reallocate a larger path
         Location_Path.Path := new Location_Path_Steps(1 .. Steps * 2);
         Location_Path.Path(Curr_Path'Range) := Curr_Path.all;
         Free(Curr_Path);
      end if;

      Steps := Steps + 1;
      Location_Path.Path(Steps) := Location_Step;
   end Add;

   -------------------------------------------------------------------

   procedure Add (Location_Step : in     Location_Steps) is

   begin
      Add(Location_Step, Parsing_Path);
   end Add;

   -------------------------------------------------------------------

   procedure Free(Location_Path : in out Location_Paths) is
   begin
      for P in Location_Path.Path'Range loop
         Predicates.Release(Location_Path.Path(P).Location_Predicates);
      end loop;
      Free(Location_Path.Path);
      Location_Path.Path  := null;
   end Free;

   -------------------------------------------------------------------

   function Get_Path return Location_Paths
   is
   begin
      return Parsing_Path;
   end Get_Path;

   -------------------------------------------------------------------

   procedure Reset_For_Parsing
   is
   begin
      Parsing_Path :=
        (Steps    => 0,
         Absolute => False,
         Path     => new Location_Path_Steps(1 .. 10));
   end Reset_For_Parsing;

end Mckae.XML.XPath.Locations;
