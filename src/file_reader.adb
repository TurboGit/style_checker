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

with Ada.Unchecked_Conversion;

package body File_Reader is

   use Ada;

   function Read_Char (File : in File_Type) return Character;
   --  Returns one character, ASCII.EOT at end of file

   function Peek_Char (File : in File_Type) return Character;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Stream_IO.Close (File.File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
   begin
      return File.Index > File.Size and then Stream_IO.End_Of_File (File.File);
   end End_Of_File;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File   : in     File_Type;
      Buffer :    out String;
      Last   :    out Natural;
      Ending :    out Checks.Line_Ending_Style)
   is
      C : Character := ASCII.NUL;
   begin
      Last := Buffer'First - 1;

      while Last < Buffer'Last
        and then C /= ASCII.LF
        and then C /= ASCII.CR
        and then C /= ASCII.EOT
      loop
         C := Read_Char (File);
         Last := Last + 1;
         Buffer (Last) := C;
      end loop;

      if C = ASCII.EOT then
         Ending := Checks.No;

      elsif Last = Buffer'First - 1 then
         Ending := Checks.Any;
         Last := 0;
         return;

      elsif Buffer (Last) = ASCII.LF then

         if Peek_Char (File) = ASCII.CR then
            Ending := Checks.MAC;
            C := Read_Char (File);

         else
            Ending := Checks.UNIX;
         end if;

      elsif Buffer (Last) = ASCII.CR then

         if Peek_Char (File) = ASCII.LF then
            Ending := Checks.DOS;
            C := Read_Char (File);

         else
            Ending := Checks.MAC;
         end if;
      end if;

      Last := Last - 1;
      File.Self.Line := File.Self.Line + 1;
   end Get_Line;

   ----------
   -- Line --
   ----------

   function Line (File : in File_Type) return Natural is
   begin
      return File.Line;
   end Line;

   ----------
   -- Name --
   ----------

   function Name
     (File     : in File_Type;
      Absolute : in Boolean) return String is
   begin
      if Absolute then
         return Directories.Full_Name (Stream_IO.Name (File.File));
      else
         return To_String (File.Relative_Name);
      end if;
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Type; Name : in String) is
   begin
      Stream_IO.Open (File.File, Stream_IO.In_File, Name);
      File.Relative_Name := To_Unbounded_String (Name);
   end Open;

   ---------------
   -- Peek_Char --
   ---------------

   function Peek_Char (File : in File_Type) return Character is

      function To_String (Data : in Stream_Element_Array) return String;
      --  Convert a Stream_Element_Array to a String. Fast version working on
      --  any computer where a Character is equal to a Byte.

      --------------
      -- To_String --
      ---------------

      function To_String
        (Data : in Stream_Element_Array)
         return String
      is

         subtype Fixed_String
           is String (Integer (Data'First) .. Integer (Data'Last));

         subtype Fixed_Array is Stream_Element_Array
           (Data'First .. Data'Last);

         function To_Characters is
           new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);

      begin
         return To_Characters (Data);
      end To_String;

      BC   : Stream_Element_Array (1 .. Cache_Size);
      Size : Stream_Element_Count;

   begin
      if File.Size = 0 or else File.Index = File.Size + 1 then
         if Stream_IO.End_Of_File (File.File) then
            return ASCII.EOT;
         end if;

         Stream_IO.Read (File.File, BC, Size);
         File.Self.Cache (1 .. Integer (Size)) := To_String (BC (1 .. Size));
         File.Self.Index := 1;
         File.Self.Size := Integer (Size);
      end if;

      return File.Cache (File.Index);
   end Peek_Char;

   ---------------
   -- Read_Char --
   ---------------

   function Read_Char (File : in File_Type) return Character is
      C : constant Character := Peek_Char (File);
   begin
      File.Self.Index := File.Index + 1;
      return  C;
   end Read_Char;

end File_Reader;
