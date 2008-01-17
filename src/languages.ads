------------------------------------------------------------------------------
--                              Style Checker                               --
--                                                                          --
--                    Copyright (C) 2006, Pascal Obry                       --
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

with Checks;

package Languages is

   use Ada.Strings.Unbounded;

   type Lang is tagged private;
   type Lang_Access is access Lang'Class;

   function Name (L : in Lang) return String;

   function Is_Extension (L : in Lang; Ext : in String) return Boolean;
   --  Returns True if Ext (an extension) belongs to the given language

   function Comment (L : in Lang) return String;
   --  Returns the comment characters used for the given language

   function Run_Syntax_Check
     (L : in Lang; Filename : in String) return Boolean;
   --  Run the syntax checker for the given languages, returns true if no
   --  error has been found.

   function Get (Filename : in String) return Lang'Class;
   --  Returns the language handler for the given filename

   procedure Register (L : in Lang'Class; Name : in String);
   --  Register a new language handler with the given name

   function Get_From_Name (Name : in String) return Lang_Access;
   --  Returns the language handler for Name, null if not found

   procedure List;
   --  Output on the list of registered languages

   --  Line ending

   procedure Set_Line_Ending
     (L      : in Lang_Access;
      Ending : in Checks.Line_Ending_Style);

   function Get_Line_Ending (L : in Lang) return Checks.Line_Ending_Style;

   --  Line length

   procedure Set_Line_Length_Max
     (L      : in Lang_Access;
      Length : in Positive);

   function Get_Line_Length_Max (L : in Lang) return Positive;

   --  Duplicate blank line

   procedure Set_Duplicate_Blank_Line
     (L    : in Lang_Access;
      Mode : in Checks.Mode);

   function Get_Duplicate_Blank_Line (L : in Lang) return Checks.Mode;

   --  Trailing spaces

   procedure Set_Trailing_Spaces
     (L    : in Lang_Access;
      Mode : in Checks.Mode);

   function Get_Trailing_Spaces (L : in Lang) return Checks.Mode;

   --  Syntax check

   procedure Set_Syntax_Check
     (L    : in Lang_Access;
      Mode : in Boolean);

   function Get_Syntax_Check (L : in Lang) return Boolean;

   --  Header size

   procedure Set_Header_Size
     (L    : in Lang_Access;
      Size : in Natural);

   function Get_Header_Size (L : in Lang) return Natural;

   --  Copyright present

   procedure Set_Copyright_Present
     (L    : in Lang_Access;
      Mode : in Boolean);

   function Get_Copyright_Present (L : in Lang) return Boolean;

   --  Copyright year

   procedure Set_Copyright_Year
     (L    : in Lang_Access;
      Mode : in Boolean);

   function Get_Copyright_Year (L : in Lang) return Boolean;

   --  Copyright pattern

   procedure Set_Copyright_Pattern
     (L       : in Lang_Access;
      Pattern : in String);

   function Get_Copyright_Pattern (L : in Lang) return String;

   --  Comments

   procedure Set_Space_Comment
     (L      : in Lang_Access;
      Number : in Natural);

   function Get_Space_Comment (L : in Lang) return Natural;

   --  Comments ending

   procedure Set_Comment_Dot_EOL
     (L    : in Lang_Access;
      Mode : in Boolean);

   function Get_Comment_Dot_EOL (L : in Lang) return Boolean;

   --  Style checker parameter

   procedure Add_Style_Checker_Parameter
     (L         : in Lang_Access;
      Parameter : in String);
   --  Add a parameter for the style checker

   function Get_Style_Checker_Parameters
     (L : in Lang) return GNAT.OS_Lib.Argument_List;
   --  Returns the arguments for the style checker

private

   type Lang is tagged record
      Name : Unbounded_String;
      C    : Checks.Data;
   end record;

end Languages;
