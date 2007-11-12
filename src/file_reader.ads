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
with Ada.Directories;
with Ada.Streams.Stream_IO;

with Checks;

package File_Reader is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   type File_Type is limited private;

   procedure Open (File : in out File_Type; Name : in String);
   --  Open file for reading

   procedure Get_Line
     (File   : in     File_Type;
      Buffer :    out String;
      Last   :    out Natural;
      Ending :    out Checks.Line_Ending_Style);
   --  Read a line in the file

   function Line (File : in File_Type) return Natural;
   --  Returns the current line number

   function Name
     (File     : in File_Type;
      Absolute : in Boolean) return String;
   --  Returns the name of the file

   function End_Of_File (File : in File_Type) return Boolean;
   --  Returns True if there is nothing more to read

   procedure Close (File : in out File_Type);
   --  Close file

private

   Cache_Size : constant := 4_096;

   type File_Type_Access is access all File_Type;

   type File_Type is limited record
      Self          : File_Type_Access := File_Type'Unchecked_Access;
      Relative_Name : Unbounded_String;
      File          : Stream_IO.File_Type;
      Line          : Natural := 0;
      Cache         : String (1 .. Cache_Size);
      Size          : Natural := 0;
      Index         : Natural := 0;
   end record;

end File_Reader;
