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

with Ada.Characters.Handling;
With Ada.Io_Exceptions;
with Ada.Long_Float_Text_Io;
use  Ada.Long_Float_Text_Io;
with Ada.Strings.Maps;

with Dom.Core.Attrs;
with Dom.Core.Documents;
with Dom.Core.Nodes;
use  Dom.Core.Nodes;
with Dom.Core.Append_Node;
with Mckae.XML.XPath.XIA;

package body Mckae.XML.XPath.Expressions is

   use Ada;
   use Ada.Strings;

   ----------------------------------------------------------------------

   -- Result of comparing two expressions
   type Expression_Comparison_Results is
     (Lesser,
      Equal,
      Greater);

   subtype Lesser_Equal is Expression_Comparison_Results range Lesser .. Equal;
   subtype Greater_Equal is Expression_Comparison_Results range Equal .. Greater;

   ----------------------------------------------------------------------

   function "+"(S : String) return Unbounded_String
     renames To_Unbounded_String;

   -- Create the string-value of the expression in accorance with its
   --  type.
   function String_Value(N : Dom.Core.Node
                         -- Node for which to create the string-value
                        ) return Dom.Core.DOM_String;

   ----------------------------------------------------------------------

   function Trim(S : String) return Unbounded_String is
   begin
      return Trim(+S, Ada.Strings.Left);
   end Trim;

   ----------------------------------------------------------------------

   procedure Coerce (Value   : in out Expression_Values;
                     To_Type : in     Value_Types       := As_String) is

      Replacement : Expression_Values(To_Type);
      Temp_String : String(1 .. Long_Float'Width);
      Last        : Positive;
      Interim     : Expression_Values;

   begin
      case To_Type is
         when As_String =>
            case Value.Value_Type is
               when As_String =>
                  Replacement := Value;
               when As_Number =>
                  case Value.Special is
                     when NaN =>
                        Replacement.S := +"NaN";
                     when Positive_Infinity =>
                        Replacement.S := +"Infinity";
                     when Negative_Infinity =>
                        Replacement.S := +"-Infinity";
                     when Normal =>
                        if Long_Float'Truncation(Value.F) = Value.F then
                           -- Integer
                           Put(Temp_String, Value.F, Aft => 0, Exp => 0);
                           -- Cut off the ".0" at the end
                           Replacement.S := Trim(Temp_String(1 .. Temp_String'Last - 2));
                        else
                           -- Float
                           Put(Temp_String, Value.F, Exp => 0);
                           Replacement.S := Trim(Temp_String);
                        end if;
                  end case;
               when As_Boolean =>
                  Replacement.B := Value.B;
               when As_Node_List =>
                  if Length(Value.Ns) > 0 then
                     Replacement.S := +String_Value(Item(Value.Ns, 0));
                  else
                     Replacement.S := Null_Unbounded_String;
                  end if;

               when As_Expr_Text =>
                  pragma Assert(False);
                  null;
            end case;

         when As_Number =>
            case Value.Value_Type is
               when As_String =>
                  Value.S := Trim(Value.S, Ada.Strings.Both);
                  begin
                     Get(To_String(Value.S), Replacement.F, Last);
                     if Last /= Length(Value.S) then
                        raise Io_Exceptions.Data_Error;
                     end if;
                  exception
                     when Io_Exceptions.Data_Error | IO_Exceptions.End_Error =>
                        Replacement.F := 0.0;
                        Replacement.Special := NaN;
                  end;
               when As_Number =>
                  Replacement := Value;
               when As_Boolean =>
                  if Value.B then
                     Replacement.F := 1.0;
                  else
                     Replacement.F := 0.0;
                  end if;
               when As_Node_List =>
                  Interim := Value;
                  Coerce(Interim, As_String);
                  Coerce(Interim, As_Number);
                  Replacement := Interim;
               when As_Expr_Text =>
                  pragma Assert(False);
                  null;
            end case;

         when As_Boolean =>
            case Value.Value_Type is
               when As_String =>
                  Replacement.B := Length(Value.S) > 0;
               when As_Number =>
                  Replacement.B := (Value.Special = Normal) and (Value.F /= 0.0);
               when As_Boolean =>
                  Replacement := Value;
               when As_Node_List =>
                  Replacement.B := Length(Value.Ns) > 0;
               when As_Expr_Text =>
                  pragma Assert(False);
                  null;
            end case;

         when As_Node_List =>
            if Value.Value_Type /= As_Node_List then
               raise Invalid_Coercion;
            else
               Replacement := Value;
            end if;
      end case;
      Value := Replacement;
   end Coerce;

   ----------------------------------------------------------------------

   procedure Harmonize
     (Expr1           : in out Expression_Values;
      -- Operand

      Expr2           : in out Expression_Values
      -- Operand
     )
   is
   begin
      if Expr1.Value_Type /= Expr2.Value_Type then
         if (Expr1.Value_Type = As_Node_List) or (Expr2.Value_Type = As_Node_List) then
            null;
         elsif (Expr1.Value_Type = As_Boolean) or (Expr2.Value_Type = As_Boolean) then
            Coerce(Expr1, As_Boolean);
            Coerce(Expr2, As_Boolean);

         elsif (Expr1.Value_Type = As_Number) or (Expr2.Value_Type = As_Number) then
            Coerce(Expr1, As_Number);
            Coerce(Expr2, As_Number);

         else
            Coerce(Expr1, As_String);
            Coerce(Expr2, As_String);
         end if;
      end if;
   end Harmonize;

   ----------------------------------------------------------------------

   function Compare
     (Expr1 : Expression_Values;
      -- Comparison operand 1

      Expr2 : Expression_Values
      -- Comparison operand 2
     ) return Expression_Comparison_Results
   is
      Result : Expression_Comparison_Results := Equal;

      E1 : Expression_Values := Expr1;
      E2 : Expression_Values := Expr2;
   begin
      Harmonize(E1, E2);
      case E1.Value_Type is
         when As_Boolean =>
            if E1.B < E2.B then
               Result := Lesser;
            elsif E1.B > E2.B then
               Result := Greater;
            else
               Result := Equal;
            end if;

         when As_Number =>
            if (E1.Special = Normal) and (E2.Special = Normal) then
               if E1.F < E2.F then
                  Result := Lesser;
               elsif E1.F > E2.F then
                  Result := Greater;
               else
                  Result := Equal;
               end if;
            else
               null;
            end if;

         when As_String =>
            if E1.S < E2.S then
               Result := Lesser;
            elsif E1.S > E2.S then
               Result := Greater;
            else
               Result := Equal;
            end if;

         when As_Node_List =>
            if Length(E1.NS) < Length(E2.NS) then
               Result := Lesser;
            elsif Length(E1.NS) > Length(E2.NS) then
               Result := Greater;
            else
               Result := Equal;
            end if;

         when As_Expr_Text =>
            pragma Assert(False);
            null;
      end case;

      return Result;
   end Compare;

   ----------------------------------------------------------------------

   function Check_Relation
     (Operator : Relational_Operators;
      Relation : Expression_Comparison_Results) return Boolean is

   begin
      case Operator is
         when Less_Than =>
            return Relation = Lesser;
         when Less_Or_Equal =>
            return Relation in Lesser_Equal;
         when Equal =>
            return Relation = Equal;
         when Not_Equal =>
            return Relation /= Equal;
         when Greater_Or_Equal =>
            return Relation in Greater_Equal;
         when Greater_Than =>
            return Relation = Greater;
      end case;
   end Check_Relation;

   ----------------------------------------------------------------------

   function Compare
     (Expr1 : Expression_Values;
      -- Comparison operand 1

      Expr2 : Expression_Values;
      -- Comparison operand 2

      Operator : Relational_Operators
      -- The relationship to evaluate between the two expressions
     ) return Boolean is

      Result : Expression_Comparison_Results := Equal;

      E1 : Expression_Values;
      E2 : Expression_Values;

   begin
      if (Expr1.Value_Type = As_Node_List) and (Expr2.Value_Type = As_Node_List) then
         -- Check whether any pair in the expressions' node lists have
         --  the specified relationship
         for N1 in 0 .. Length(Expr1.Ns) - 1 loop
            declare
               S1 : constant Unbounded_String := +String_Value(Item(Expr1.Ns, N1));
            begin
               for N2 in 0 .. Length(Expr2.Ns) - 1 loop
                  Result := Compare((As_String, S1),
                                    (As_String,
                                     +String_Value(Item(Expr2.Ns, N2))));
                  if Check_Relation(Operator, Result) then
                     return True;
                  end if;
               end loop;
            end;
         end loop;
         return False;

      elsif (Expr1.Value_Type = As_Node_List) then
         case Expr2.Value_Type is
            when As_Number =>
               for N1 in 0 .. Length(Expr1.Ns) - 1 loop
                  declare
                     S1 : constant Unbounded_String := +String_Value(Item(Expr1.Ns, N1));
                  begin
                     E1 := (As_String, S1);
                     Coerce(E1, As_Number);
                     return Check_Relation(Operator, Compare(E1, Expr2));
                  end;
               end loop;

            when As_String =>
               for N1 in 0 .. Length(Expr1.Ns) - 1 loop
                  declare
                     S1 : constant Unbounded_String := +String_Value(Item(Expr1.Ns, N1));
                  begin
                     E1 := (As_String, S1);
                     return Check_Relation(Operator, Compare(E1, Expr2));
                  end;
               end loop;

            when As_Boolean =>
               E1 := Expr1;
               Coerce(E1, As_Boolean);
               return Check_Relation(Operator, Compare(E1, Expr2));

            when others =>
               pragma Assert(False);
               null;
         end case;
         return False;

      elsif (Expr2.Value_Type = As_Node_List) then
         case Expr1.Value_Type is
            when As_Number =>
               for N2 in 0 .. Length(Expr2.Ns) - 1 loop
                  declare
                     S2 : constant Unbounded_String := +String_Value(Item(Expr2.Ns, N2));
                  begin
                     E2 := (As_String, S2);
                     Coerce(E2, As_Number);
                     return Check_Relation(Operator, Compare(Expr1, E2));
                  end;
               end loop;

            when As_String =>
               for N2 in 0 .. Length(Expr2.Ns) - 1 loop
                  declare
                     S2 : constant Unbounded_String := +String_Value(Item(Expr2.Ns, N2));
                  begin
                     E2 := (As_String, S2);
                     return Check_Relation(Operator, Compare(Expr1, E2));
                  end;
               end loop;

            when As_Boolean =>
               E2 := Expr2;
               Coerce(E2, As_Boolean);
               return Check_Relation(Operator, Compare(E1, Expr2));

            when others =>
               pragma Assert(False);
               null;
         end case;
         return False;

      else
         Result := Compare(Expr1, Expr2);

         return Check_Relation(Operator, Result);

      end if;
   end Compare;

   ----------------------------------------------------------------------

   function Compute
     (Expr1 : Expression_Values;
      -- Comparison operand 1

      Expr2 : Expression_Values;
      -- Comparison operand 2

      Computation : Computations
      -- The mathematical operation to perform
     ) return Expression_Values
   is
      Result : Expression_Values(As_Number) := (As_Number, 0.0, Normal);
   begin
      pragma Assert(Expr1.Value_Type = As_Number);
      pragma Assert(Expr2.Value_Type = As_Number);

      if (Expr1.Special = NaN) or (Expr2.Special = NaN) then
         Result.Special := NaN;
      else
         case Computation is
            when Add =>
               if (Expr1.Special = Normal) and (Expr2.Special = Normal) then
                  Result.F := Expr1.F + Expr2.F;
               elsif (Expr1.Special = Positive_Infinity) and (Expr2.Special = Positive_Infinity) then
                  Result.Special := Positive_Infinity;
               elsif (Expr1.Special = Negative_Infinity) and (Expr2.Special = Negative_Infinity) then
                  Result.Special := Negative_Infinity;
               elsif ((Expr1.Special = Negative_Infinity) and (Expr2.Special = Positive_Infinity))
                 or ((Expr1.Special = Positive_Infinity) and (Expr2.Special = Negative_Infinity)) then
                  Result.Special := NaN;
               else
                  pragma Assert(False);
                  null;
               end if;

            when Subtract =>
               if (Expr1.Special = Normal) and (Expr2.Special = Normal) then
                  Result.F := Expr1.F - Expr2.F;
               elsif (Expr1.Special = Positive_Infinity) and (Expr2.Special = Negative_Infinity) then
                  Result.Special := Positive_Infinity;
               elsif (Expr1.Special = Negative_Infinity) and (Expr2.Special = Positive_Infinity) then
                  Result.Special := Negative_Infinity;
               elsif ((Expr1.Special = Positive_Infinity) and (Expr2.Special = Positive_Infinity))
                 or ((Expr1.Special = Negative_Infinity) and (Expr2.Special = Negative_Infinity)) then
                  Result.Special := NaN;
               else
                  pragma Assert(False);
                  null;
               end if;

            when Multiply =>
               if (Expr1.Special = Normal) and (Expr2.Special = Normal) then
                  Result.F := Expr1.F * Expr2.F;
               elsif ((Expr1.Special = Positive_Infinity) and (Expr2.Special = Negative_Infinity))
                 or ((Expr1.Special = Negative_Infinity) and (Expr2.Special = Positive_Infinity)) then
                  Result.Special := Negative_Infinity;
               elsif ((Expr1.Special = Positive_Infinity) and (Expr2.Special = Positive_Infinity))
                 or ((Expr1.Special = Negative_Infinity) and (Expr2.Special = Negative_Infinity)) then
                  Result.Special := Positive_Infinity;
               elsif Expr2.F = 0.0 then
                  Result.Special := NaN;
               else
                  pragma Assert(False);
                  null;
               end if;

            when Divide =>
               if (Expr1.Special = Normal)
                 and ((Expr2.Special = Positive_Infinity) or (Expr2.Special = Negative_Infinity)) then
                  Result.F := 0.0;
               elsif (Expr2.Special = Normal) and (Expr2.F = 0.0) then
                  if Expr1.F = 0.0 then
                     Result.Special := NaN;
                  elsif (Expr1.Special = Normal) then
                     if (Expr1.F > 0.0) then
                        Result.Special := Positive_Infinity;
                     else
                        Result.Special := Negative_Infinity;
                     end if;
                  else
                     Result.Special := Expr1.Special;
                  end if;
               elsif (Expr1.Special = Normal) and (Expr2.Special = Normal) then
                  Result.F := Expr1.F / Expr2.F;
               else
                  Result.Special := NaN;
               end if;

            when Modulo =>
               if Expr2.F = 0.0 then
                  Result.Special := Positive_Infinity;
               else
                  -- Yes, _rem_, not "mod"!
                  Result.F := Long_Float(Integer(Long_Float'Truncation(Expr1.F))
                                         rem Integer(Long_Float'Truncation(Expr2.F)));
               end if;
            when Negate =>
               Result.F := -Expr1.F;
            when others =>
               null;
         end case;
      end if;
      return Result;
   end Compute;

   ----------------------------------------------------------------------

   procedure Check_Arg_Count (Args      : in     Argument_List;
                              Minimum   : in     Natural;
                              Evaluated :    out Boolean;
                              Maximum   : in     Natural       := 0) is
      Max_Args : constant Natural := Natural'Max(Minimum, Maximum);
   begin
      if Args'Length not in Minimum .. Max_Args then
         raise Invalid_Expression;
      end if;
      Evaluated := True;
   end Check_Arg_Count;

   ----------------------------------------------------------

   function Round_Value(Expr : Expression_Values)
                       return Expression_Values is
      Result : Expression_Values := (As_Number, 0.0, Normal);
   begin
      pragma Assert(Expr.Value_Type = As_Number);
      if (Expr.Special = Normal) then
         Result.F := Long_Float'Rounding(Expr.F);
      else
         Result.Special := Expr.Special;
      end if;
      return Result;
   end Round_Value;

   ----------------------------------------------------------

   function Sum_Values (Expr : in Expression_Values)
     return Expression_Values is
      Sum  : Expression_Values := (As_Number, 0.0, Normal);
      Temp : Expression_Values;
   begin
      pragma Assert(Expr.Value_Type = As_Node_List);

      for N in 0 .. Length(Expr.Ns) - 1 loop
         -- Get each node's string value
         Temp := (As_String, +String_Value(Item(Expr.Ns, N)));

         -- Convert that string value to an equivalent number
         Coerce(Temp, As_Number);

         -- Add it to the working sum, following all the XPath IEEE
         --  754 addition rules
         Sum := Compute(Sum, Temp, Add);
      end loop;
      return Sum;
   end Sum_Values;

   ----------------------------------------------------------

   function Scrub_String(Expr : Expression_Values)
                        return Dom.Core.DOM_String is

      White_Space     : constant String := ' ' & Ascii.LF & Ascii.CR & ASCII.HT;
      White_Space_Set : constant Maps.Character_Set := Maps.To_Set(White_Space);

      -- Start by removing all the leading and trailing white space characters
      S : constant String := To_String(Trim(Expr.S, White_Space_Set, White_Space_Set));
      Result : Unbounded_String;

      Copying : Boolean := True;

   begin
      for I in S'Range loop
         if not Maps.Is_In(S(I), White_Space_Set) then
            Append(Result, S(I));
            Copying := True;
         else
            if Copying then
               Append(Result, S(I));
            end if;
            Copying := False;
         end if;
      end loop;

      return To_String(Result);
   end Scrub_String;

   ----------------------------------------------------------

   function Translation(Args : Argument_List)
                       return Unbounded_String is

      use Maps;

      From_Set   : Maps.Character_Set;
      Omit_Set   : Maps.Character_Set;
      Mapping    : Maps.Character_Mapping;
      Result     : Unbounded_String;
      C          : Character;
      Translated : Boolean := True;

   begin
      pragma Assert(Args(1).Value_Type = As_String);
      pragma Assert(Args(2).Value_Type = As_String);
      pragma Assert(Args(3).Value_Type = As_String);

      -- If the length of the "from" string set is equal (or shorter)
      --  than the "to" string set, use the standard Strings.Translate
      --  function.  Strings.Translate requires that the sizes of the
      --  from/to mappings be the same, which is not required by
      --  XPath's translate.
      begin
         if Length(Args(2).S) <= Length(Args(3).S) then
            Mapping := To_Mapping(To_String(Args(2).S),
                                  Slice(Args(3).S, 1, Length(Args(2).S)));
            Result := Translate(Args(1).S, Mapping);
         else
            -- Set up for just the characters for which a mapping has
            --  been defined
            From_Set := To_Set(Slice(Args(2).S, 1, Length(Args(3).S)));
            Omit_Set := To_Set(Slice(Args(2).S, Length(Args(3).S) + 1, Length(Args(2).S)));
            Mapping  := To_Mapping(Slice(Args(2).S, 1, Length(Args(3).S)),
                                   To_String(Args(3).S));

            for I in 1 .. Length(Args(1).S) loop
               C := Element(Args(1).S, I);
               if Is_In(C, From_Set) then
                  Append(Result, Value(Mapping, C));
               elsif not Is_In(C, Omit_Set) then
                  Append(Result, C);
               end if;
            end loop;
         end if;

      exception
         when Translation_Error =>
            Translated := False;
      end;

      if not Translated then
         -- There was an error in the translation mapping,
         --  specifically, there were duplicates in the "From" string,
         --  which Ada's Translate() function doesn't allow.  So, to
         --  work around this we're going to dork with a copy of the
         --  From string, replacing the duplicate characters with the
         --  corresponding character in the "To" set, the effect being
         --  that those characters will just continue to map to
         --  themselves.
         declare
            Translation_Count : constant Positive :=
              Positive'Min(Length(Args(2).S), Length(Args(3).S));
            From_String : String := Slice(Args(2).S, 1, Translation_Count);
            To_String : constant String := Slice(Args(3).S, 1, Translation_Count);
            New_From_String : String := From_String;
         begin
            -- The lengths of the From and To strings have been
            --  equalized, now start replacing duplicate characters.
            --  By going in reverse, we just replace the character
            --  that has been identified as a duplicate and then move
            --  on to the next one.  The last one encountered will be
            --  its first appearance in the string.
            for I in reverse 2 .. From_String'Last loop
               for J in reverse 1 .. I - 1 loop
                  if From_String(I) = From_String(J) then
                     New_From_String(I) := To_String(I);
                     exit;
                  end if;
               end loop;
            end loop;

            -- Now that the From string has been reconstructed with
            --  its duplicates replaced, go do the mapping.
            Mapping := To_Mapping(New_From_String, To_String);
            Result := Translate(Args(1).S, Mapping);
         end;
      end if;

      return Result;
   end Translation;

   ----------------------------------------------------------

   function Extract_Slice(Args : Argument_List)
                         return Unbounded_String is
      -- Handles the weirdnesses surrounding rounding and infinity and
      --  stuff.
      Low_Bound    : Integer;
      High_Bound   : Integer;
      Start_Index  : Natural;
      Slice_High   : Natural;

      No_Substring : Boolean := False;  -- No valid substring

      Source_Length : Natural := Length(Args(1).S);

   begin
      -- Figure out the starting index of the slice
      if Args(2).Special = Normal then
         Low_Bound   := Integer(Round_Value(Args(2)).F);
         Start_Index := Integer'Max(1, Low_Bound);
      elsif Args(2).Special = Negative_Infinity then
         Low_Bound   := Integer'First;
         Start_Index := 1;
      else -- Positive_Infinity
         No_Substring := True;
      end if;
      No_Substring := No_Substring Or (Low_Bound > Source_Length);

      if not No_Substring then
         -- If there's a third argument, figure out the end of the slice
         if Args'Length = 3 then
            if Args(3).Special = Normal then
               High_Bound := Low_Bound + Integer(Round_Value(Args(3)).F) - 1;
               if High_Bound < Low_Bound then
                  No_Substring := True;
               else
                  Slice_High := Integer'Min(High_Bound, Source_Length);
               end if;
            elsif Args(3).Special = Positive_Infinity then
               Slice_High := Source_Length;
            else -- Negative_Infinity
               No_Substring := True;
            end if;
         else
            Slice_High := Source_Length;
         end if;
      end if;

      if No_Substring then
         return Null_Unbounded_String;
      else
         return +Slice(Args(1).S, Start_Index, Slice_High);
      end if;
   end Extract_Slice;

   ----------------------------------------------------------

   function Match_Lang (Lang         : Expression_Values;
                        Context_Node : Node_Items)
                       return Boolean is

      use Ada.Characters.Handling;
      use DOM.Core;

      Search_Lang : constant Dom.Core.DOM_String := To_Upper(To_String(Lang.S));
      Lang_Query  : constant Dom.Core.DOM_String
        := "ancestor-or-self::*[@xml:lang][1]/@xml:lang";
      Node_Set    : Dom.Core.Node_List;
      Result      : Boolean := False;

   begin
      Node_Set := XIA.Xpath_Query(Context_Node.N, Lang_Query);

      if Length(Node_Set) > 0 then
         declare
            Lang_Value : constant DOM_String := To_Upper(Attrs.Value(Item(Node_Set, 0)));
            Search_Length : constant Positive := Search_Lang'Length;
         begin
            -- First check to see if there's a simple match
            Result := Search_Lang = Lang_Value;

            if (not Result) and (Search_Lang'Length < Lang_Value'Length) then
               -- Check if it's the prefix of a language
               Result := (Lang_Value(Search_Lang'Range) = Search_Lang)
                 and (Lang_Value(Search_Length + 1) = '-');
            end if;
         end;
      end if;

      return Result;
   end Match_Lang;

   ----------------------------------------------------------

   function Extract_By_ID (N   : Dom.Core.Node;
                           Arg : Expression_Values)
                         return Expression_Values is
      Result       : Expression_Values(As_Node_List);
      Owner_Doc    : Dom.Core.Document;
      Node_With_Id : Dom.Core.Node;

      use type Dom.Core.Node;

      -- By the way, this doesn't work.  At least as of XMLAda 1.0,
      --  because Get_Element_By_ID is not yet implemented.  Therefore
      --  this always returns an empty node-set.
   begin
      Owner_Doc := Owner_Document(N);
      Node_With_Id := Dom.Core.Documents.Get_Element_By_ID(Owner_Doc, To_String(Arg.S));
      if Node_With_Id /= null then
         Dom.Core.Append_Node(Result.Ns, Node_With_ID);
      end if;
      return Result;
   end Extract_By_ID;

   ----------------------------------------------------------

   function Get_Local_Name(Arg : Expression_Values)
                          return Expression_Values is
   begin
      pragma Assert(Arg.Value_Type = As_Node_List);

      if Length(Arg.Ns) > 0 then
         return (As_String, +Dom.Core.Nodes.Local_Name(Item(Arg.Ns, 0)));
      else
         return (As_String, Null_Unbounded_String);
      end if;
   end Get_Local_Name;

   ----------------------------------------------------------

   function Get_Namespace_URI(Arg : Expression_Values)
                             return Expression_Values is
   begin
      pragma Assert(Arg.Value_Type = As_Node_List);

      if Length(Arg.Ns) > 0 then
         return (As_String, +Dom.Core.Nodes.Namespace_URI(Item(Arg.Ns, 0)));
      else
         return (As_String, Null_Unbounded_String);
      end if;
   end Get_Namespace_URI;

   ----------------------------------------------------------

   function Get_Expanded_Name(Arg : Expression_Values)
                             return Expression_Values is
   begin
      pragma Assert(Arg.Value_Type = As_Node_List);

      if Length(Arg.Ns) > 0 then
         return (As_String, +(Dom.Core.Nodes.Node_Name(Item(Arg.Ns, 0))));
      else
         return (As_String, Null_Unbounded_String);
      end if;
   end Get_Expanded_Name;

   ----------------------------------------------------------

   procedure Evaluate_Function (Function_Name : in     String_Ptr;
                                Context_Node  : in     Node_Items;
                                Args          : in out Argument_List;
                                Result        :    out Expression_Values) is

      Func_Name : constant String := Function_Name.all;
      Evaluated : Boolean := False;

      Working_Node_List : Dom.Core.Node_List;

      Slice_Index       : Natural;

   begin
      -- Reduce (at least a bit) the number of string comparisons that
      --  have to be done to locate the function name
      case Func_Name(1) is
         when 'b' =>
            if Func_Name = "boolean" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Boolean);
               Result := Args(1);
            end if;
         when 'c' =>
            if Func_Name = "count" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Node_List);
               Result := (As_Number, Long_Float(Length(Args(1).Ns)), Normal);
            elsif Func_Name = "concat" then
               Check_Arg_Count(Args, 2, Evaluated, Natural'Last);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_String);
               Result := (As_String, Args(1).S & Args(2).S);
               for A in 3 .. Args'Last loop
                  Coerce(Args(A), As_String);
                  Append(Result.S, Args(A).S);
               end loop;
            elsif Func_Name = "contains" then
               Check_Arg_Count(Args, 2, Evaluated);
               Result := (As_Boolean, Index(Args(1).S, To_String(Args(2).S)) /= 0);
            elsif Func_Name = "ceiling" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Number);
               Result := (As_Number, Long_Float'Ceiling(Args(1).F), Normal);
            end if;

         when 'f' =>
            if Func_Name = "false" then
               Check_Arg_Count(Args, 0, Evaluated);
               Result := (As_Boolean, False);
            elsif Func_Name = "floor" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Number);
               Result := (As_Number, Long_Float'Floor(Args(1).F), Normal);
            end if;

         when 'i' =>
            if Func_Name = "id" then
               Check_Arg_Count(Args, 1, Evaluated);
               Result := Extract_By_Id(Context_Node.N, Args(1));
            end if;

         when 'l' =>
            if Func_Name = "last" then
               Check_Arg_Count(Args, 0, Evaluated);
               Result := (As_Number, Long_Float(Context_Node.Node_Set_Size), Normal);
            elsif Func_Name = "lang" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_String);
               Result := (As_Boolean, Match_Lang(Args(1), Context_Node));
            elsif Func_Name = "local-name" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
               else
                  Coerce(Args(1), As_Node_List);
                  Result := Get_Local_Name(Args(1));
               end if;
            end if;

         when 'n' =>
            if Func_Name = "not" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Boolean);
               Result := (As_Boolean, not Args(1).B);
            elsif Func_Name = "number" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
               else
                  Coerce(Args(1), As_Number);
                  Result := Args(1);
               end if;
            elsif Func_Name = "normalize-space" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Result := (As_String, +String_Value(Context_Node.N));
               else
                  Coerce(Args(1), As_String);
                  Result := (As_String, +Scrub_String(Args(1)));
               end if;
            elsif Func_Name = "namespace-uri" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
               else
                  Coerce(Args(1), As_Node_List);
                  Result := Get_Namespace_URI(Args(1));
               end if;
            elsif Func_Name = "name" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
               else
                  Coerce(Args(1), As_Node_List);
                  Result := Get_Expanded_Name(Args(1));
               end if;
            end if;

         when 'p' =>
            if Func_Name = "position" then
               Check_Arg_Count(Args, 0, Evaluated);
               Result := (As_Number, Long_Float(Context_Node.Node_Position), Normal);
            end if;

        when 'r' =>
            if Func_Name = "round" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Number);
               Result := Round_Value(Args(1));
            end if;

         when 's' =>
            if Func_Name = "string" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
               else
                  Coerce(Args(1), As_String);
                  Result := Args(1);
               end if;
            elsif Func_Name = "starts-with" then
               Check_Arg_Count(Args, 2, Evaluated);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_String);
               Result := (As_Boolean, Index(Args(1).S, To_String(Args(2).S)) = 1);
            elsif Func_Name = "substring-before" then
               Check_Arg_Count(Args, 2, Evaluated);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_String);
               Result := (As_String, Null_Unbounded_String);
               Slice_Index := Index(Args(1).S, To_String(Args(2).S));
               if Slice_Index /= 0 then
                  Result.S := Head(Args(1).S, Slice_Index - 1);
               end if;
            elsif Func_Name = "substring-after" then
               Check_Arg_Count(Args, 2, Evaluated);
               Result := (As_String, Null_Unbounded_String);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_String);
               Slice_Index := Index(Args(1).S, To_String(Args(2).S));
               if Slice_Index /= 0 then
                  Result.S := Tail(Args(1).S,
                                   Length(Args(1).S) - (Slice_Index + Length(Args(2).S) - 1));
               end if;
            elsif Func_Name = "substring" then
               Check_Arg_Count(Args, 2, Evaluated, 3);
               Result := (As_String, Null_Unbounded_String);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_Number);

               Result.S := Extract_Slice(Args);
            elsif Func_Name = "string-length" then
               Check_Arg_Count(Args, 0, Evaluated, 1);
               if Args'Length = 0 then
                  Dom.Core.Append_Node(Working_Node_List, Context_Node.N);
                  Result := (As_Node_List, Working_Node_List);
                  Coerce(Result, As_String);
               else
                  Coerce(Args(1), As_String);
                  Result := (As_Number, Long_Float(Length(Args(1).S)), Normal);
               end if;
            elsif Func_Name = "sum" then
               Check_Arg_Count(Args, 1, Evaluated);
               Coerce(Args(1), As_Node_List);
               Result := Sum_Values(Args(1));
            end if;

         when 't' =>
            if Func_Name = "true" then
               Check_Arg_Count(Args, 0, Evaluated);
               Result := (As_Boolean, True);
            elsif Func_Name = "translate" then
               Check_Arg_Count(Args, 3, Evaluated);
               Coerce(Args(1), As_String);
               Coerce(Args(2), As_String);
               Coerce(Args(3), As_String);

               Result := (As_String, Translation(Args));
            end if;

         when others =>
            Evaluated := False;
      end case;

      if not Evaluated then
         raise Invalid_Expression;
      end if;
   end Evaluate_Function;

   ----------------------------------------------------------

   function Concat_Text_Nodes(Node_Set : Dom.Core.Node_List) return Dom.Core.DOM_String is
      Value    : Unbounded_String;
   begin
      for N in 0 .. Length(Node_Set) - 1 loop
         Append(Value, Node_Value(Item(Node_Set, N)));
      end loop;
      return To_String(Value);
   end Concat_Text_Nodes;

   ----------------------------------------------------------

   function String_Value(N : Dom.Core.Node
                         -- Node for which to create the string-value
                        ) return Dom.Core.DOM_String
   is
      use Dom.Core;

      Node_Set : Node_List;

   begin
      case N.Node_Type is
         when Attribute_Node
           | Text_Node
           | Comment_Node
           | Processing_Instruction_Node =>
            return Node_Value(N);

         when Element_Node =>
            Node_Set := XIA.Xpath_Query(N, "./text()");

         when Document_Node =>
            Node_Set := XIA.Xpath_Query(N, "//text()");

         when others =>
            null;
      end case;

      return Concat_Text_Nodes(Node_Set);

   end String_Value;

end Mckae.XML.XPath.Expressions;
