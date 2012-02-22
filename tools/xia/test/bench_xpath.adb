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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Calendar;
use Ada.Calendar;

with Dom.Core.Nodes;
with Mckae.XML.XPath.XIA;
with Dom.Readers;
with Unicode.CES;

--  with Dtraq.Client_Transport;
--  use  Dtraq.Client_Transport;

with Mckae.Xml.Xpath.Locations;

with Input_Sources.File;

with Text_IO; use Text_IO;

procedure Bench_Xpath is

   use Dom;
   use Dom.Core;
   use Input_Sources;
   use Mckae.Xml.XPath.Locations;
   use Mckae.Xml.XPath.XIA;
   use Ada.Strings.Fixed;
   use Ada.Strings.Maps;


   XML_Source_Reader : Dom.Readers.Tree_Reader;
   Queried_Nodes : Node_List;
   N             : Node;

   File_Source : File.File_Input;
   
   Start, Stop : Ada.Calendar.Time;

begin
   File.Open("personal.xml", File_Source);

   Readers.Parse(XML_Source_Reader, File_Source);
   File.Close(File_Source);
   
   N := Readers.Get_Tree(XML_Source_Reader);
   for I in 1 .. 10 loop
      Start := Clock;
      Queried_Nodes := Xpath_Query(N, "//given/..");
      Queried_Nodes := Xpath_Query(N, "//email");
      Queried_Nodes := Xpath_Query(N, "//@id");
      Queried_Nodes := Xpath_Query(N, "//@aid");
      Queried_Nodes := Xpath_Query(N, "/personnel/person/link/attribute::subordinates");
      Queried_Nodes := Xpath_Query(N, "personnel/person/name/family");
      Queried_Nodes := Xpath_Query(N, "//family/ancestor::name");
      Queried_Nodes := Xpath_Query(N, "//family/text()");
      Queried_Nodes := Xpath_Query(N, "/personnel/person/name/family/..");
      Queried_Nodes := Xpath_Query(N, "./pesonnel");
      Queried_Nodes := Xpath_Query(N, "./personnel");
      Queried_Nodes := Xpath_Query(N, "personnel/person/email/preceding::name/given/text()");
      Queried_Nodes := Xpath_Query(N, ".//*");
      Queried_Nodes := Xpath_Query(N, "//email/following-sibling::link/@manager");
      Queried_Nodes := Xpath_Query(N, "//email/following-sibling::link/@subordinates");
      Queried_Nodes := Xpath_Query(N, "//@id/parent::*");
      Queried_Nodes := Xpath_Query(N, "//given/..");
      Queried_Nodes := Xpath_Query(N, "//email/preceding-sibling::*");
      Queried_Nodes := Xpath_Query(N, "//email/following-sibling::*");
      Queried_Nodes := Xpath_Query(N, "//family/ancestor::person/link");
      Queried_Nodes := Xpath_Query(N, "//name/ancestor::*");
      Queried_Nodes := Xpath_Query(N, "//name/ancestor-or-self::*");
      Queried_Nodes := Xpath_Query(N, "//name/descendant::*");
      Queried_Nodes := Xpath_Query(N, "//name/descendant-or-self::*");
      Queried_Nodes := Xpath_Query(N, "//@subordinates");
      Queried_Nodes := Xpath_Query(N, "//@subordinates/..");
      Queried_Nodes := Xpath_Query(N, "//@subordinates/../preceding::*");
      Queried_Nodes := Xpath_Query(N, "//@subordinates/../following::*");
      Queried_Nodes := Xpath_Query(N, "..");
      Queried_Nodes := Xpath_Query(N, ".");
      Queried_Nodes := Xpath_Query(N, "..//email");
      Queried_Nodes := Xpath_Query(N, "//person[3]/following-sibling::*");
      Queried_Nodes := Xpath_Query(N, "//person[3]/following-sibling::*/@id[2]");
      Queried_Nodes := Xpath_Query(N, "//person[3]/preceding-sibling::*/@id");
      Queried_Nodes := Xpath_Query(N, "//person[3]/preceding-sibling::*[2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[3]/preceding-sibling::*/@id[2]");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[2]/email/preceding-sibling::name/given/ancestor-or-self::*[3]");
      Queried_Nodes := Xpath_Query(N, "//family");
      Queried_Nodes := Xpath_Query(N, "//given");
      Queried_Nodes := Xpath_Query(N, "//family|//given");
      Queried_Nodes := Xpath_Query(N, "//family|//given[2]");
      Queried_Nodes := Xpath_Query(N, "//person[last()]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position()=2]/@id");
--      Queried_Nodes := Xpath_Query(N, "//person[position(3)=2]/@id");
      Queried_Nodes := Xpath_Query(N, "//@id[3]");
      Queried_Nodes := Xpath_Query(N, "//@id[true()]");
      Queried_Nodes := Xpath_Query(N, "//@id[false()]");
      Queried_Nodes := Xpath_Query(N, "//@id[not(position()=2)]");
      Queried_Nodes := Xpath_Query(N, "//@id[position() >= 5]");
      Queried_Nodes := Xpath_Query(N, "//person[3 - 1]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[3 * 2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[5 mod 2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[5 mod -2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[(-5 mod 2) + 4]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[(-5 mod -2) + 2 * true()]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[round(1.1*3)]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[1 or 2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[4 and -2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[4 and (3 > 10)]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() != 3]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[3 * 1.1]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[5 - 0.0001]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[(((position() mod 2)))]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() mod 2 = 0]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() = floor(last() div 2 + 0.5) or position() = ceiling(last() div 2 + 0.5)]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() > 3]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() < 3]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() <= 2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[position() div 2 + 2]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[5 + -true()]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[number(""This is not a number"")]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[4 div 0]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number(""This is not a number"")) = ""NaN""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number(""This is not a number"")) = ""This is not a number""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(4 div 0) = ""Infinity""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[concat(string(position()), ""23"") = ""323""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[starts-with(concat(string(position()), ""23""), ""22"")]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[contains(concat(string(position()), ""23"", string(position()), ""47""), ""34"")]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring-before(concat(string(position()), ""23"", string(position()), ""47""), ""35"") = ""52""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring-after(concat(string(position()), ""23"", string(position()), ""47""), ""35"") = ""47""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 4, 3) = ""347""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 4) = ""347""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), -42, 1 div 0) = ""223247""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 0 div 0, 3) = ""3""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 0 div 0, 3) = """"]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 1.5, 2.6) = '234']/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), 0, 3) = '52']/@id");
--      Queried_Nodes := Xpath_Query(N, "//person[substring(concat(string(position()), ""23"", string(position()), ""47""), -1 div 0, 1 div 0) = ""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[(string-length(string(position())) * position()) = ""4""]/@id");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(-3)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(35.3)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(0)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(0 div 0)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(-4 div 0)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(10 div 0)]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(string())]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(string(last()))]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(substring-before(""123"", ""5""))]/text()");
      Queried_Nodes := Xpath_Query(N, "/descendant-or-self::email[boolean(number())]/text()");
      Queried_Nodes := Xpath_Query(N, "//person[email]/@id");
      Queried_Nodes := Xpath_Query(N, "//name[given = ""Big""]/family");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[string-length(string(name/family)) < 5]/@id");
      Queried_Nodes := Xpath_Query(N, "//*[attribute::id=""five.worker""]/@id");
      Queried_Nodes := Xpath_Query(N, "//*[../email=""chief@foo.com""]");
      Queried_Nodes := Xpath_Query(N, "//given[.=""Four""]");
      Queried_Nodes := Xpath_Query(N, "/descendant::node()[count(ancestor::node()) = 3]");
      Queried_Nodes := Xpath_Query(N, "//name[given=""Five""]/given");
      Queried_Nodes := Xpath_Query(N, "//name[normalize-space(given)=""Five""]/given");
      Queried_Nodes := Xpath_Query(N, "//name[translate(normalize-space(string(given)), ""Five"", ""Four"")=""Four""]/given");
      Queried_Nodes := Xpath_Query(N, "//name[translate(string(given), ""Oneabcd"", ""Two"")=""Two""]/given");
      Queried_Nodes := Xpath_Query(N, "//name[translate(string(given), ""FFFF"", ""Four"")=""Four""]/given");
      Queried_Nodes := Xpath_Query(N, "//name[translate(string(given), ""Thre"", ""One"")=""One""]/given");
      Queried_Nodes := Xpath_Query(N, "//name/given[normalize-space()=""Big""]/text()");
      Queried_Nodes := Xpath_Query(N, "//name/given[number()=""Big""]/text()");
      Queried_Nodes := Xpath_Query(N, "/personnel/person/age[count(preceding::age)=2]/../@id");
      Queried_Nodes := Xpath_Query(N, "/personnel/person/age[(sum(preceding::age) - 21)=56]/../@id");
      Queried_Nodes := Xpath_Query(N, "//personnel[lang(""en"")]");
      Queried_Nodes := Xpath_Query(N, "//personnel[lang(""pl"")]");
      Queried_Nodes := Xpath_Query(N, "//*[lang(""en"")]");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[5]/name/given/ancestor-or-self::*[@xml:lang][2]");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[5]/name/given/ancestor-or-self::*[@xml:lang][last()]");
      Queried_Nodes := Xpath_Query(N, "//person[2][@id=""three.worker""]/name/given");
      Queried_Nodes := Xpath_Query(N, "//*[lang(""EN-US"")]");
      Queried_Nodes := Xpath_Query(N, "//person[number(age)=""37""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number(email))=""NaN""]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number(email))=""5""]/@id");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[@id=""four.worker"" and @xml:lang=""en-US""]");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[@id=""five.worker"" and @xml:lang=""en-US""]/email");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[number(age) > 40]/@id");
--      Queried_Nodes := Xpath_Query(N, "/personnel/person[(number(age) > 40) | (number(age) < 30)]/@id");
      Queried_Nodes := Xpath_Query(N, "/personnel/person[@id=""four.worker""]|/personnel/person/age");
      Queried_Nodes := Xpath_Query(N, "//@*");
      Queried_Nodes := Xpath_Query(N, "//person/@*");
      Queried_Nodes := Xpath_Query(N, "//node()[name(.)=""#text""]");
      Queried_Nodes := Xpath_Query(N, "//node()[local-name(.)=name(.)]");
      Queried_Nodes := Xpath_Query(N, "//@*[string(namespace-uri(.))]");
      Queried_Nodes := Xpath_Query(N, "//@*[namespace-uri(.)]");
      Queried_Nodes := Xpath_Query(N, "//link[@manager]");
      Queried_Nodes := Xpath_Query(N, "//link[@subordinates]");
      Queried_Nodes := Xpath_Query(N, "//link[@manager|@subordinates]");
      Queried_Nodes := Xpath_Query(N, "//person[number('This is not a number')]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[4 div 0]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number('This is not a number')) = 'NaN']/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(number('This is not a number')) = 'This is not a number']/@id");
      Queried_Nodes := Xpath_Query(N, "//person[string(4 div 0) = 'Infinity']/@id");
      Queried_Nodes := Xpath_Query(N, "//person[concat(string(position()), '23') = '323']/@id");
      Queried_Nodes := Xpath_Query(N, "//person[starts-with(concat(string(position()), '23'), '22')]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[contains(concat(string(position()), '23', string(position()), '47'), '34')]/@id");
      Queried_Nodes := Xpath_Query(N, "//person[substring-before(concat(string(position()), '23', string(position()), '47'), '35') = '52']/@id");
      Stop := Clock;
      Put_Line(Duration'Image(Stop - Start));
   end loop;

-- Disconnect_From_Server;

end Bench_Xpath;
