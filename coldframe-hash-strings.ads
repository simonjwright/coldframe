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

--  $RCSfile: coldframe-hash-strings.ads,v $
--  $Revision: e4e81eacd986 $
--  $Date: 2009/07/14 20:30:42 $
--  $Author: simonjwright $

package ColdFrame.Hash.Strings is

   pragma Pure;

private

   type Special_Integer is mod 2 ** 31;  --  simple to convert to Natural

   --  The values of this data set were calculated by
   --  ColdFrame.Hash.Strings.Generate.
   Character_Hash : constant array (Character) of Special_Integer :=
     (
      984719331,        5549118,        766024072,      1782170249,
      1979122325,       879201502,      1188582954,     1471758832,
      883313938,        1010239068,     2120250247,     1663891563,
      1400435399,       611768381,      965286016,      291061516,
      602049945,        341840586,      446736412,      534326988,
      879400590,        115975401,      287300770,      99519131,
      1883128801,       742427720,      1718263868,     370966478,
      573380604,        499552044,      914792461,      152207158,
      1922008231,       1426479855,     439787893,      1199048137,
      1749450733,       395638881,      98857777,       252054864,
      2091470093,       593075735,      1445994177,     571956618,
      1507345341,       1215780988,     830274073,      304982643,
      1794041579,       568616719,      985045689,      18643032,
      412691184,        1873382913,     1555552003,     1841145219,
      570255970,        1554859928,     1616963541,     1034438736,
      1492001286,       938221550,      123656034,      249099585,
      1244150815,       1845304696,     2005652032,     666663477,
      265047742,        1691335513,     76356327,       2006216891,
      1399775779,       295113889,      1728693893,     739294244,
      2027351481,       1631702726,     1785514192,     1003652189,
      1486642239,       1348722995,     1166100456,     1430738785,
      58988669,         615038664,      673717869,      815434322,
      1521494432,       735620373,      1647632929,     1570991298,
      808044639,        242057220,      2129886914,     1943863516,
      573812329,        350459942,      689723930,      510598,
      1750189842,       596879579,      2108972031,     182585311,
      346109517,        164353064,      1732057982,     995159026,
      230900665,        1867714139,     657230337,      1521972453,
      1913743015,       530591469,      1982072772,     1856926551,
      645722519,        1870167045,     1395907006,     2076062945,
      1291878066,       1469831406,     1763291070,     2004608395,
      1914849634,       1341755874,     94132563,       1520064351,
      2114305534,       874812304,      849179966,      1354429011,
      1872732391,       1745321010,     1846620327,     520500233,
      1932454136,       1377421040,     248111538,      246504133,
      1611532319,       32107552,       1410571232,     1355667112,
      64815800,         848347156,      321879765,      1416668103,
      1554145580,       1304621195,     1953934492,     1084256023,
      1113438057,       1423583286,     1951967713,     1077669717,
      40221201,         1212969690,     1305023324,     1475727721,
      703806124,        972417764,      1947193660,     790672341,
      2075514973,       1847499900,     426071092,      146144493,
      1810368595,       1297848239,     995855583,      615701776,
      1367416720,       1244913862,     984771352,      1380850509,
      856458461,        232725667,      282442108,      383403775,
      591882437,        1802130623,     145038230,      1798295904,
      139536673,        396889937,      2097007166,     742139008,
      1278585130,       1071412542,     241637753,      516703097,
      430544809,        1871515881,     1574127672,     667811495,
      1904637653,       189326086,      1198401831,     1537642430,
      782668529,        617760709,      792405589,      1426210626,
      524993632,        1754509651,     619889727,      88905132,
      1128674030,       1164153273,     783876669,      2042468472,
      182052484,        1787508288,     546347304,      953034154,
      1753454440,       1755927260,     1165313566,     1295837358,
      1792972769,       1252063075,     882638397,      1363150071,
      872550284,        1626288328,     1247891274,     1535970459,
      647411431,        2117463231,     1535374504,     84860480,
      569436335,        1213935667,     1276574044,     167503846,
      1078724342,       1323561729,     402427666,      795585067,
      1951362545,       1272639454,     900767062,      1336167258,
      35204691,         1112472178,     1395596327,     1906942073,
      165835247,        1324754806,     709857495,      1386898601,
      1197238176,       1036156512,     240838629,      1616275360
     );

end ColdFrame.Hash.Strings;
