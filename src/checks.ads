------------------------------------------------------------------------------
--                              Style Checker                               --
--                                                                          --
--                   Copyright (C) 2006-2011, Pascal Obry                   --
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

with Ada.Strings.Unbounded;
with GNAT.OS_Lib;

package Checks is

   use Ada.Strings.Unbounded;

   Syntax_Error : exception;
   --  Raised when a syntax error is found

   Max_Parameters : constant := 30;
   --  Maximum number of parameters that can be specified for a style checker
   --  for a single language.

   type Mode is (Rejected, Accepted);
   type Line_Ending_Style is (DOS, UNIX, MAC, No, Any);
   --  No means that the line is the last of the file and does not have a line
   --  terminator.

   type Data is record
      Line_Ending          : Line_Ending_Style := UNIX;
      --  The line ending style accepted

      Line_Length_Max      : Positive := 79;
      --  The maximum line length

      Duplicate_Blank_Line : Mode := Rejected;
      --  If double blank line are accepted or not

      Trailing_Spaces      : Mode := Rejected;
      --  Reject any line with trailing blanks (space or HT)

      Tabulation           : Mode := Rejected;
      --  Reject any line with tabulations

      Header_Size          : Natural := 20;
      --  Minimum header size

      Copyright_Present    : Boolean := False;
      --  Copyright notice must be present

      Copyright_Year       : Boolean := True;
      --  Copyright year must include current year

      Copyright_Pattern    : Unbounded_String;
      --  Copyright line must match the given regexp pattern

      Check_Syntax         : Boolean := True;
      --  Syntax must be checked

      Space_Comment        : Natural := 2;
      --  Number of spaces after a comment tag

      Checker_Params       : GNAT.OS_Lib.Argument_List (1 .. Max_Parameters);
      --  Style checker parameters

      Comment_Dot_EOL      : Boolean := True;
      --  Single line comment can terminate with a dot

      Index                : Natural := 0;

      Operator_EOL         : Mode := Accepted;
      --  Check for operators at end of line

      Then_Layout          : Mode := Accepted;
      --  Check for Then layout (Ada), should be on the line with the if or the
      --  first word on its line.

      With_Use             : Mode := Accepted;
      --  Should we reject unsorted with/use clauses and block of with/use
      --  without separator line.
   end record;

end Checks;
