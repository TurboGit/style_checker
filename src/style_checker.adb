------------------------------------------------------------------------------
--                              Style Checker                               --
--                                                                          --
--                  Copyright (C) 2006-2010, Pascal Obry                    --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------

--
--  Usage:
--
--  style_checker [options] [-lang name] [options]
--
--  The first options are set for all available languages.
--  Options that are set after a -lang name are only set for this specific
--  language (language names are not case sensitive).
--
--  To display the usage information:
--     $ style_checker
--
--  To check Ada files only (syntax, line length, trailing spaces):
--     $ style_checker -BCELS -lang Ada -slt file.ad*
--
--  To list available languages:
--     $ style_checker -lang
--

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Regpat;

with Version;
with Checks;
with File_Reader;
with Languages;
with Supported_Languages;

procedure Style_Checker is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use GNAT;

   use type Directories.File_Kind;
   use type Checks.Line_Ending_Style;
   use type Checks.Mode;

   package Ext_Set is new Containers.Indefinite_Hashed_Sets
     (String, Hash, "=", "=");

   Y : constant String :=
         Calendar.Year_Number'Image (Calendar.Year (Calendar.Clock));
   Current_Year : constant String := Y (Y'First + 1 .. Y'Last);

   Absolute_Pathname : Boolean := False;
   Style_Error       : Boolean := False;
   Ignore_Set        : Ext_Set.Set;
   Max_Error         : Natural := Natural'Last;
   Error_Count       : Natural := 0;
   Real_Filename     : Unbounded_String;

   type File_Checker is record
      File                 : File_Reader.File_Type;
      Lang                 : Languages.Lang_Access;
      Count_Blank          : Natural := 0;
      Copyright_Found      : Boolean := False;
      Copyright_Year       : Boolean := False;
      Header_Size          : Natural := 0;
      In_Header            : Boolean := True;
      Multiline_Comment    : Boolean := False;
      Consecutive_Comment  : Natural := 0;
      Last_Comment_Dot_EOL : Boolean := False;
   end record;

   procedure Check (Filename : in String);
   --  Check this file

   procedure Check_Line
     (Checker     : in out File_Checker;
      Line        : in     String;
      Line_Ending : in     Checks.Line_Ending_Style);
   --  Pass all checks that are line related

   subtype Line_Offset is Integer range -1 .. 0;

   procedure Report_Error
     (File    : in File_Reader.File_Type;
      Message : in String;
      Offset  : in Line_Offset := 0);
   --  Report an error to standard error

   procedure Report_Error
     (Filename : in String;
      Message  : in String;
      At_Line  : in Natural := 1);
   --  Report an error to standard error

   procedure Usage;
   --  Display the usage information

   procedure List_Languages;
   --  Display supported languages

   function Unquote (Str : in String) return String;
   --  Removes leading/trailing spaces and quote if present

   -----------
   -- Check --
   -----------

   procedure Check (Filename : in String) is
      Checker : File_Checker;
      Line    : String (1 .. 2_048);
      K       : Natural;
      Nb_Line : Natural := 0;
      Ending  : Checks.Line_Ending_Style;
   begin
      Checker.Lang := new Languages.Lang'Class'(Languages.Get (Filename));

      --  Run line oriented tests

      File_Reader.Open (Checker.File, Filename);

      while not File_Reader.End_Of_File (Checker.File) loop
         File_Reader.Get_Line (Checker.File, Line, K, Ending);
         Check_Line (Checker, Line (1 .. K), Ending);
      end loop;

      Nb_Line := File_Reader.Line (Checker.File);

      File_Reader.Close (Checker.File);

      --  Run file oriented tests

      if Checker.Lang.Get_Syntax_Check then
         if not Languages.Run_Syntax_Check (Checker.Lang.all, Filename) then
            Style_Error := True;
         end if;
      end if;

      if Checker.Lang.Get_Header_Size > Checker.Header_Size then
         if Checker.Header_Size = 0 then
            Report_Error
              (Filename, "missing file header (must start on first line)");
         else
            Report_Error
              (Filename, "file header should have at least"
                 & Positive'Image (Checker.Lang.Get_Header_Size)
                 & " lines, found" & Integer'Image (Checker.Header_Size));
         end if;
      end if;

      if Checker.Lang.Get_Copyright_Present
        and then not Checker.Copyright_Found
      then
         Report_Error (Filename, "missing copyright notice");
      end if;

      if Checker.Copyright_Found
        and then Checker.Lang.Get_Copyright_Year
        and then not Checker.Copyright_Year
      then
         Report_Error
           (Filename, "missing year " & Current_Year & " in copyright");
      end if;

      if Checker.Lang.Get_Duplicate_Blank_Line = Checks.Rejected
        and then Checker.Count_Blank >= 1
      then
         Report_Error
           (Filename => Filename,
            Message => "blank line not allowed at end of file",
            At_Line => Nb_Line);
      end if;

   exception
      when IO_Exceptions.Name_Error =>
         Report_Error (Filename, "can't open file");
   end Check;

   ----------------
   -- Check_Line --
   ----------------

   procedure Check_Line
     (Checker     : in out File_Checker;
      Line        : in     String;
      Line_Ending : in     Checks.Line_Ending_Style)
   is
      procedure Check_Ending;

      procedure Check_Length_Max;

      procedure Check_Duplicate_Blank;

      procedure Check_Trailing_Spaces;

      procedure Check_Header;

      procedure Check_Copyright;

      procedure Check_Space_Comment;

      procedure Check_Comment_Dot_EOL;

      procedure Check_Tab;

      ---------------------------
      -- Check_Comment_Dot_EOL --
      ---------------------------

      procedure Check_Comment_Dot_EOL is
         Pos : Natural;
      begin
         if not Checker.Lang.Get_Comment_Dot_EOL
           and then Checker.Lang.Comment /= ""
         then
            if Fixed.Index (Line, String'(Checker.Lang.Comment)) /= 0 then
               --  This is a comment
               Checker.Consecutive_Comment := Checker.Consecutive_Comment + 1;

               Pos := Fixed.Index_Non_Blank (Line, Going => Backward);

               if Line (Pos) = '.'
                 and then Pos > Line'First + 1
                 and then Line (Pos - 2 .. Pos - 1) /= ".."
               then
                  Checker.Last_Comment_Dot_EOL := True;
               else
                  Checker.Last_Comment_Dot_EOL := False;
               end if;

            else
               --  No more in a comment line

               if Checker.Consecutive_Comment = 1
                 and then Checker.Last_Comment_Dot_EOL
               then
                  Report_Error
                    (Checker.File,
                     "single line comment should not terminate with dot",
                     Offset => -1);
               end if;

               Checker.Consecutive_Comment := 0;
               Checker.Last_Comment_Dot_EOL := False;
            end if;
         end if;
      end Check_Comment_Dot_EOL;

      ---------------------
      -- Check_Copyright --
      ---------------------

      procedure Check_Copyright is
         use Text_IO;
         Co_Start : Natural := 0;
         Cp_Start : Natural := Fixed.Index (Line, " Copyright");
      begin
         if Checker.Lang.Comment /= "" then
            Co_Start := Fixed.Index (Line, String'(Checker.Lang.Comment));
         end if;

         if Cp_Start /= 0
           and then Cp_Start + 10 <= Line'Length
           and then Line (Cp_Start + 10) /= ' '
         then
            --  We are not at the end of the line and no space after Copyright
            Cp_Start := 0;
         end if;

         if (Checker.Lang.Get_Copyright_Present
             or else Checker.Lang.Get_Copyright_Year)
           and then Cp_Start /= 0
           and then Co_Start /= 0
           and then Cp_Start > Co_Start
         then
            Checker.Copyright_Found := True;

            if Checker.Lang.Get_Copyright_Year then
               if Fixed.Index (Line, Current_Year) /= 0 then
                  Checker.Copyright_Year := True;
               end if;
            end if;
         end if;

         --  Check that the copyright year follow the given regexp

         if Cp_Start /= 0
           and then Checker.Lang.Get_Copyright_Pattern /= ""
         then
            declare
               Pattern : constant Regpat.Pattern_Matcher :=
                           Regpat.Compile (Checker.Lang.Get_Copyright_Pattern);
            begin
               if not Regpat.Match (Pattern, Line) then
                  Report_Error
                    (Checker.File,
                     "copyright line not matching expected pattern");
               end if;
            end;
         end if;
      end Check_Copyright;

      ---------------------------
      -- Check_Duplicate_Blank --
      ---------------------------

      procedure Check_Duplicate_Blank is
      begin
         if Checker.Lang.Get_Duplicate_Blank_Line = Checks.Rejected
           and then (Line'Length = 0
                     or else Fixed.Count (Line, " " & ASCII.HT) = Line'Length)
         then
            Checker.Count_Blank := Checker.Count_Blank + 1;

            if Checker.Count_Blank > 1 then
               Report_Error (Checker.File, "duplicate blank line");
            end if;

         else
            Checker.Count_Blank := 0;
         end if;
      end Check_Duplicate_Blank;

      ------------------
      -- Check_Ending --
      ------------------

      procedure Check_Ending is
      begin
         if Checker.Lang.Get_Line_Ending /= Checks.Any then
            if Line_Ending = Checks.No then
               Report_Error
                 (Checker.File,
                  "missing line terminator");
            elsif Checker.Lang.Get_Line_Ending /= Line_Ending then
               Report_Error
                 (Checker.File,
                  "wrong " & Checks.Line_Ending_Style'Image (Line_Ending) &
                  " line ending");
            end if;
         end if;
      end Check_Ending;

      ------------------
      -- Check_Header --
      ------------------

      procedure Check_Header is
         C     : constant String := Checker.Lang.Comment;
         CS    : constant String := Checker.Lang.Start_Multiline_Comment;
         CE    : constant String := Checker.Lang.End_Multiline_Comment;
         Is_C  : constant Boolean :=
                   C /= ""
                   and then Line'Length >= C'Length
                   and then Line
                     (Line'First .. Line'First + C'Length - 1) = C;
         Is_CS : constant Boolean :=
                   CS /= ""
                   and then File_Reader.Line (Checker.File) = 1
                   and then Line'Length >= CS'Length
                   and then Line
                     (Line'First .. Line'First + CS'Length - 1) = CS;
         Is_CE : constant Boolean :=
                   CE /= ""
                   and then Line'Length >= CE'Length
                   and then Line
                     (Line'Last - CE'Length + 1 .. Line'Last) = CE;
      begin
         --  Check that we are starting with a multi-line comment

         if File_Reader.Line (Checker.File) = 1 then
            if Is_C or else Is_CS then
               Checker.Header_Size := Checker.Header_Size + 1;

               if Is_CS then
                  Checker.Multiline_Comment := True;
               end if;

            else
               Checker.In_Header := False;
            end if;

         else
            if Checker.In_Header
              and then
                (Is_C or else (Checker.Multiline_Comment and then not Is_CE))
            then
               Checker.Header_Size := Checker.Header_Size + 1;
            else
               if Is_CE then
                  Checker.Header_Size := Checker.Header_Size + 1;
               end if;
               Checker.In_Header := False;
            end if;
         end if;
      end Check_Header;

      ----------------------
      -- Check_Length_Max --
      ----------------------

      procedure Check_Length_Max is
      begin
         if Line'Length > Checker.Lang.Get_Line_Length_Max then
            Report_Error (Checker.File, "line too long");
         end if;
      end Check_Length_Max;

      -------------------------
      -- Check_Space_Comment --
      -------------------------

      procedure Check_Space_Comment is
         N  : constant Natural := Checker.Lang.Get_Space_Comment;
         NI : constant String := Natural'Image (N);
         C  : constant String := Checker.Lang.Comment;
         I  : constant Natural := Fixed.Index_Non_Blank (Line);
      begin
         if N /= 0
           and then I /= 0
           and then I + C'Length - 1 <= Line'Last
           and then Line (I .. I + C'Length - 1) = C
           and then Line (Line'Last - C'Length + 1 .. Line'Last) /= C
           and then (Line (I .. I + 1) /= "#!"
                     or else File_Reader.Line (Checker.File) > 1)
         --  Do no check script headers
         then
            for K in I + C'Length .. I + C'Length + N - 1 loop
               if Line (K) /= ' ' then
                  Report_Error
                    (Checker.File,
                     NI (NI'First + 1 .. NI'Last) & " spaces after " & C);
                  exit;
               end if;
            end loop;
         end if;
      end Check_Space_Comment;

      ---------------
      -- Check_Tab --
      ---------------

      procedure Check_Tab is
      begin
         if Checker.Lang.Get_Tabulation = Checks.Rejected
           and then Strings.Fixed.Index (Line, String'(1 => ASCII.HT)) /= 0
         then
            Report_Error (Checker.File, "no tabulations allowed");
         end if;
      end Check_Tab;

      ---------------------------
      -- Check_Trailing_Spaces --
      ---------------------------

      procedure Check_Trailing_Spaces is
      begin
         if Checker.Lang.Get_Trailing_Spaces = Checks.Rejected
           and then Line'Length > 0
           and then (Line (Line'Last) = ' '
                     or else Line (Line'Last) = ASCII.HT)
         then
            Report_Error (Checker.File, "no trailing spaces allowed");
         end if;
      end Check_Trailing_Spaces;

   begin
      Check_Ending;
      Check_Length_Max;
      Check_Duplicate_Blank;
      Check_Trailing_Spaces;
      Check_Header;
      Check_Copyright;
      Check_Space_Comment;
      Check_Comment_Dot_EOL;
      Check_Tab;
   end Check_Line;

   --------------------
   -- List_Languages --
   --------------------

   procedure List_Languages is
      procedure P (Str : in String) renames Text_IO.Put_Line;
   begin
      Text_IO.New_Line;
      P ("Style Checker " & Version.Simple);
      Text_IO.New_Line;
      Languages.List;
      Text_IO.New_Line;
   end List_Languages;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (File    : in File_Reader.File_Type;
      Message : in String;
      Offset  : in Line_Offset := 0)
   is
      Line : constant String :=
               Natural'Image (File_Reader.Line (File) + Offset);
   begin
      Error_Count := Error_Count + 1;
      if Error_Count <= Max_Error then
         if Real_Filename = Null_Unbounded_String then
            Text_IO.Put_Line
              (Text_IO.Standard_Error,
               File_Reader.Name (File, Absolute_Pathname) & ':'
                 & Line (Line'First + 1 .. Line'Last) & ": " & Message);
         else
            Text_IO.Put_Line
              (Text_IO.Standard_Error,
               To_String (Real_Filename) & ':'
                 & Line (Line'First + 1 .. Line'Last) & ": " & Message);
         end if;
      end if;
   end Report_Error;

   procedure Report_Error
     (Filename : in String;
      Message  : in String;
      At_Line  : in Natural := 1)
   is
      Line : constant String := Natural'Image (At_Line);
   begin
      Error_Count := Error_Count + 1;
      if Error_Count <= Max_Error then
         if Real_Filename = Null_Unbounded_String then
            Text_IO.Put_Line
              (Text_IO.Standard_Error, Filename & ':'
               & Line (Line'First + 1 .. Line'Last) & ": " & Message);
         else
            Text_IO.Put_Line
              (Text_IO.Standard_Error,
               To_String (Real_Filename) & ':'
               & Line (Line'First + 1 .. Line'Last) & ": " & Message);
         end if;
      end if;
   end Report_Error;

   -------------
   -- Unquote --
   -------------

   function Unquote (Str : in String) return String is
      S : constant String := Fixed.Trim (Str, Strings.Both);
   begin
      if (S (S'First) = ''' and then S (S'Last) = ''')
        or else (S (S'First) = '"' and then S (S'Last) = '"')
      then
         return S (S'First + 1 .. S'Last - 1);
      else
         return S;
      end if;
   end Unquote;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure P (Str : in String) renames Text_IO.Put_Line;
   begin
      Text_IO.New_Line;
      P ("Style Checker " & Version.Simple);
      Text_IO.New_Line;
      P ("style_checker [-lang name] [options] file1 file2...");
      P ("   -lang       : list all built-in supported languages");
      P ("   -lang NAME  : following options are for this specific language");
      P ("   -a          : check for tabulations (default)");
      P ("   -A          : disable tabulations check");
      P ("   -abs        : output absolute path name");
      P ("   -ign EXT    : ignore files having EXT has extension");
      P ("   -b          : no duplicate blank lines (default)");
      P ("   -B          : disable duplicate blank lines check");
      P ("   -c          : check for space after comment tag (default)");
      P ("   -C          : disable space in comment check");
      P ("   -cp         : check copyright presence");
      P ("   -cP         : disable check for copyright presence (default)");
      P ("   -cy         : check for copyright year");
      P ("   -cY         : disable check for copyright year (default)");
      P ("   -cf         : if present a copyright line should match the"
         & " given pattern");
      P ("   -cF         : disable copyright pattern check");
      P ("   -d          : check single comment line dot ending");
      P ("   -D          : disable check for single comment line dot"
         & " ending (default)");
      P ("   -e DOS|UNIX : line ending style (UNIX default)");
      P ("   -E          : disable line ending check");
      P ("   -h N        : start with an header of N line (default N  20)");
      P ("   -H          : disable header check");
      P ("   -l N        : line length <= N (default 79)");
      P ("   -L          : disable line length check");
      P ("   -m N        : output only the first N errors");
      P ("   -n NAME     : filename to report in error message");
      P ("   -s          : syntax check (default)");
      P ("   -sp PARAM   : additional parameter for the style checker");
      P ("   -S          : disable syntax check");
      P ("   -t          : check for trailing spaces (default)");
      P ("   -T          : disable trailing spaces check");
      P ("   -v          : display version");
      Text_IO.New_Line;
   end Usage;

   Lang : Languages.Lang_Access;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      raise Checks.Syntax_Error;

   elsif Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-lang"
   then
      List_Languages;

   elsif Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-h"
   then
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   else
      loop
         case GNAT.Command_Line.Getopt
           ("a A abs lang: ign: e: E l? h? H "
              & "L b B s S t T v c? C cp cy cP cY cf: cF d D sp: m: n:")
         is
            when ASCII.NUL =>
               exit;

            when 'a' =>
               if GNAT.Command_Line.Full_Switch = "abs" then
                  Absolute_Pathname := True;

               elsif GNAT.Command_Line.Full_Switch = "a" then
                  Languages.Set_Tabulation (Lang, Checks.Rejected);

               else
                  raise Checks.Syntax_Error;
               end if;

            when 'A' =>
               Languages.Set_Tabulation (Lang, Checks.Accepted);

            when 'd' =>
               Languages.Set_Comment_Dot_EOL (Lang, False);

            when 'D' =>
               Languages.Set_Comment_Dot_EOL (Lang, True);

            when 'e' =>
               Languages.Set_Line_Ending
                 (Lang, Checks.Line_Ending_Style'Value
                    (GNAT.Command_Line.Parameter));

            when 'E' =>
               Languages.Set_Line_Ending (Lang, Checks.Any);

            when 'i' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "ign" then
                     Ignore_Set.Include (GNAT.Command_Line.Parameter);
                  else
                     raise Checks.Syntax_Error;
                  end if;
               end;

            when 'l' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "lang" then
                     Lang := Languages.Get_From_Name
                       (GNAT.Command_Line.Parameter);

                  elsif Full = "l" then
                     declare
                        P : constant String := GNAT.Command_Line.Parameter;
                     begin
                        if P = "" then
                           Languages.Set_Line_Length_Max (Lang, 79);
                        else
                           Languages.Set_Line_Length_Max
                             (Lang, Positive'Value (P));
                        end if;
                     exception
                        when Constraint_Error | IO_Exceptions.Name_Error =>
                           raise Checks.Syntax_Error;
                     end;
                  end if;
               end;

            when 'L' =>
               Languages.Set_Line_Length_Max (Lang, Positive'Last);

            when 'h' =>
               declare
                  P : constant String := GNAT.Command_Line.Parameter;
               begin
                  if P = "" then
                     Languages.Set_Header_Size (Lang, 20);
                  else
                     Languages.Set_Header_Size (Lang, Positive'Value (P));
                  end if;
               exception
                  when Constraint_Error | IO_Exceptions.Name_Error =>
                     raise Checks.Syntax_Error;
               end;

            when 'H' =>
               Languages.Set_Header_Size (Lang, 0);

            when 'b' =>
               Languages.Set_Duplicate_Blank_Line (Lang, Checks.Rejected);

            when 'B' =>
               Languages.Set_Duplicate_Blank_Line (Lang, Checks.Accepted);

            when 't' =>
               Languages.Set_Trailing_Spaces (Lang, Checks.Rejected);

            when 'T' =>
               Languages.Set_Trailing_Spaces (Lang, Checks.Accepted);

            when 's' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "sp" then
                     Languages.Add_Style_Checker_Parameter
                       (Lang, GNAT.Command_Line.Parameter);

                  else
                     Languages.Set_Syntax_Check (Lang, True);
                  end if;
               end;

            when 'S' =>
               Languages.Set_Syntax_Check (Lang, False);

            when 'c' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "c" then
                     declare
                        P : constant String := GNAT.Command_Line.Parameter;
                     begin
                        if P = "" then
                           Languages.Set_Space_Comment (Lang, 2);
                        else
                           Languages.Set_Space_Comment
                             (Lang, Positive'Value (P));
                        end if;
                     end;

                  elsif Full = "cp" then
                     Languages.Set_Copyright_Present (Lang, True);

                  elsif Full = "cP" then
                     Languages.Set_Copyright_Present (Lang, False);

                  elsif Full = "cy" then
                     Languages.Set_Copyright_Year (Lang, True);

                  elsif Full = "cY" then
                     Languages.Set_Copyright_Year (Lang, False);

                  elsif Full = "cf" then
                     Languages.Set_Copyright_Pattern
                       (Lang, Unquote (GNAT.Command_Line.Parameter));

                  elsif Full = "cF" then
                     Languages.Set_Copyright_Pattern (Lang, "");
                  end if;
               end;

            when 'C' =>
               Languages.Set_Space_Comment (Lang, 0);

            when 'm' =>
               Max_Error := Natural'Value (GNAT.Command_Line.Parameter);

            when 'n' =>
               Real_Filename :=
                 To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'v' =>
               Text_IO.Put_Line ("Style Checker " & Version.Complete);
               exit;

            when others =>
               raise Checks.Syntax_Error;
         end case;
      end loop;

      --  Register some known extension to ignore

      Ignore_Set.Include ("gif");
      Ignore_Set.Include ("png");
      Ignore_Set.Include ("jpg");
      Ignore_Set.Include ("pdf");
      Ignore_Set.Include ("ps");
      Ignore_Set.Include ("exe");
      Ignore_Set.Include ("dll");
      Ignore_Set.Include ("so");
      Ignore_Set.Include ("o");
      Ignore_Set.Include ("obj");
      Ignore_Set.Include ("tar");
      Ignore_Set.Include ("gz");
      Ignore_Set.Include ("bz2");
      Ignore_Set.Include ("7z");

      loop
         declare
            Filename : constant String :=
                         GNAT.Command_Line.Get_Argument (Do_Expansion => True);
         begin
            exit when Filename'Length = 0;

            if Directories.Exists (Filename) then
               if Directories.Kind (Filename) /= Directories.Directory then
                  declare
                     Ext : constant String := Directories.Extension (Filename);
                  begin
                     if (Ext /= "" and then not Ignore_Set.Contains (Ext))
                       or else
                         (Ext = "" and then not Ignore_Set.Contains
                              (Directories.Simple_Name (Filename)))
                     then
                        --  Do not check directory
                        Check (Filename);
                     end if;
                  end;
               end if;

            else
               Report_Error (Filename, "file not found");
            end if;
         end;
      end loop;

   end if;

   if Style_Error or else Error_Count > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

exception
   when Checks.Syntax_Error | GNAT.Command_Line.Invalid_Switch  =>
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Style_Checker;
