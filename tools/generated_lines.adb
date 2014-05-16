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

with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Generated_Lines_Support;

procedure Generated_Lines is

   procedure Scan_Directory (Named : Dir_Name_Str;
                             Verbosely : Boolean;
                             Recursively : Boolean;
                             Logging : Boolean);
   procedure Scan_Directory (Named : Dir_Name_Str;
                             Verbosely : Boolean;
                             Recursively : Boolean;
                             Logging : Boolean) is
      Wd : Dir_Type;
   begin
      Open (Dir => Wd,
            Dir_Name => Named);
      declare
         Str : String (1 .. 1024);
         Last : Natural;
      begin
         loop
            Read (Dir => Wd,
                  Str => Str,
                  Last => Last);
            exit when Last = 0;
            if Str (1 .. Last) /= "." and Str (1 .. Last) /= ".." then
               if Is_Directory (Named & Str (1 .. Last))
                 and then Recursively
               then
                  Scan_Directory
                    (Named & Str (1 .. Last) & Directory_Separator,
                     Verbosely => Verbosely,
                     Recursively => Recursively,
                     Logging => Logging);
               else
                  Generated_Lines_Support.Count (Named
                                                   & Directory_Separator
                                                   & Str (1 .. Last),
                                                 Verbosely => Verbosely,
                                                 Logging => Logging);
               end if;
            end if;
         end loop;
      end;
      Close (Dir => Wd);
   end Scan_Directory;

   --  Option flags
   Logging : Boolean := False;
   Recursive : Boolean := False;
   Verbose : Boolean := False;

begin

   loop
      case GNAT.Command_Line.Getopt ("l r v") is
         when ASCII.NUL => exit;
         when 'l' => Logging := True;
         when 'r' => Recursive := True;
         when 'v' => Verbose := True;
         when others => raise Program_Error;
      end case;
   end loop;

   declare
      Dir : constant String := GNAT.Command_Line.Get_Argument;
   begin

      if Dir'Length = 0 then

         declare
            D : constant String := Get_Current_Dir;
         begin
            Scan_Directory (D,
                            Verbosely => Verbose,
                            Recursively => Recursive,
                            Logging => Logging);
            Generated_Lines_Support.Report (D, With_Header => Verbose);
         end;

      else

         declare
            D : constant String :=
              GNAT.OS_Lib.Normalize_Pathname (Dir & Dir_Separator);
         begin
            Scan_Directory
              (D,
               Verbosely => Verbose,
               Recursively => Recursive,
               Logging => Logging);
            Generated_Lines_Support.Report (D, With_Header => Verbose);
         end;

      end if;

   end;

end Generated_Lines;
