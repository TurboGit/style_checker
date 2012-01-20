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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

package body Languages is

   use Ada;
   use Ada.Characters.Handling;

   Lang_Set : array (1 .. 50) of Lang_Access;
   Index    : Natural := 0;

   function Get_From_Name (Name : in String) return Lang'Class;
   --  Return a language given its name

   ---------------------------------
   -- Add_Style_Checker_Parameter --
   ---------------------------------

   procedure Add_Style_Checker_Parameter
     (L         : in Lang_Access;
      Parameter : in String) is
   begin
      if L = null then
         raise Checks.Syntax_Error;

      else
         L.C.Index := L.C.Index + 1;

         if L.C.Index > L.C.Checker_Params'Last then
            raise Checks.Syntax_Error;
         else
            L.C.Checker_Params (L.C.Index) := new String'(Parameter);
         end if;
      end if;
   end Add_Style_Checker_Parameter;

   -------------
   -- Comment --
   -------------

   function Comment (L : in Lang) return String is
      pragma Unreferenced (L);
   begin
      return "";
   end Comment;

   ---------------------------
   -- End_Multiline_Comment --
   ---------------------------

   function End_Multiline_Comment (L : in Lang) return String is
      pragma Unreferenced (L);
   begin
      return "";
   end End_Multiline_Comment;

   ---------
   -- Get --
   ---------

   function Get (Filename : in String) return Lang'Class is
      Ext   : aliased constant String := Directories.Extension (Filename);
      Base  : aliased constant String := Directories.Base_Name (Filename);
      Check : access constant String;
   begin
      if Ext = "" then
         Check := Base'Access;
      else
         Check := Ext'Access;
      end if;

      for K in 1 .. Index loop
         if Is_Extension (Lang_Set (K).all, Check.all) then
            return Lang_Set (K).all;
         end if;
      end loop;

      return Get_From_Name ("unknown");
   end Get;

   -------------------------
   -- Get_Comment_Dot_EOL --
   -------------------------

   function Get_Comment_Dot_EOL (L : in Lang) return Boolean is
   begin
      return L.C.Comment_Dot_EOL;
   end Get_Comment_Dot_EOL;

   ---------------------------
   -- Get_Copyright_Pattern --
   ---------------------------

   function Get_Copyright_Pattern (L : in Lang) return String is
   begin
      return To_String (L.C.Copyright_Pattern);
   end Get_Copyright_Pattern;

   ---------------------------
   -- Get_Copyright_Present --
   ---------------------------

   function Get_Copyright_Present (L : in Lang) return Boolean is
   begin
      return L.C.Copyright_Present;
   end Get_Copyright_Present;

   ------------------------
   -- Get_Copyright_Year --
   ------------------------

   function Get_Copyright_Year (L : in Lang) return Boolean is
   begin
      return L.C.Copyright_Year;
   end Get_Copyright_Year;

   ------------------------------
   -- Get_Duplicate_Blank_Line --
   ------------------------------

   function Get_Duplicate_Blank_Line (L : in Lang) return Checks.Mode is
   begin
      return L.C.Duplicate_Blank_Line;
   end Get_Duplicate_Blank_Line;

   -------------------
   -- Get_From_Name --
   -------------------

   function Get_From_Name (Name : in String) return Lang_Access is
      L_Name : constant String := To_Lower (Name);
   begin
      for K in 1 .. Index loop
         if To_Lower (To_String (Lang_Set (K).Name)) = L_Name then
            return Lang_Set (K);
         end if;
      end loop;

      return Get_From_Name ("unknown");
   end Get_From_Name;

   function Get_From_Name (Name : in String) return Lang'Class is
   begin
      return Get_From_Name (Name).all;
   end Get_From_Name;

   ---------------------
   -- Get_Header_Size --
   ---------------------

   function Get_Header_Size (L : in Lang) return Natural is
   begin
      return  L.C.Header_Size;
   end Get_Header_Size;

   ---------------------
   -- Get_Line_Ending --
   ---------------------

   function Get_Line_Ending
     (L : in Lang) return Checks.Line_Ending_Style is
   begin
      return L.C.Line_Ending;
   end Get_Line_Ending;

   -------------------------
   -- Get_Line_Length_Max --
   -------------------------

   function Get_Line_Length_Max (L : in Lang) return Positive is
   begin
      return L.C.Line_Length_Max;
   end Get_Line_Length_Max;

   ----------------------
   -- Get_Operator_EOL --
   ----------------------

   function Get_Operator_EOL (L : in Lang) return Checks.Mode is
   begin
      return L.C.Operator_EOL;
   end Get_Operator_EOL;

   -----------------------
   -- Get_Space_Comment --
   -----------------------

   function Get_Space_Comment (L : in Lang) return Natural is
   begin
      return L.C.Space_Comment;
   end Get_Space_Comment;

   ----------------------------------
   -- Get_Style_Checker_Parameters --
   ----------------------------------

   function Get_Style_Checker_Parameters
     (L : in Lang) return GNAT.OS_Lib.Argument_List is
   begin
      return L.C.Checker_Params (1 .. L.C.Index);
   end Get_Style_Checker_Parameters;

   ----------------------
   -- Get_Syntax_Check --
   ----------------------

   function Get_Syntax_Check (L : in Lang) return Boolean is
   begin
      return L.C.Check_Syntax;
   end Get_Syntax_Check;

   --------------------
   -- Get_Tabulation --
   --------------------

   function Get_Tabulation (L : in Lang) return Checks.Mode is
   begin
      return L.C.Tabulation;
   end Get_Tabulation;

   ---------------------
   -- Get_Then_Layout --
   ---------------------

   function Get_Then_Layout (L : in Lang) return Checks.Mode is
   begin
      return L.C.Then_Layout;
   end Get_Then_Layout;

   -------------------------
   -- Get_Trailing_Spaces --
   -------------------------

   function Get_Trailing_Spaces (L : in Lang) return Checks.Mode is
   begin
      return L.C.Trailing_Spaces;
   end Get_Trailing_Spaces;

   ------------------
   -- Get_With_Use --
   ------------------

   function Get_With_Use (L : in Lang) return Checks.Mode is
   begin
      return L.C.With_Use;
   end Get_With_Use;

   ------------------
   -- Is_Extension --
   ------------------

   function Is_Extension (L : in Lang; Ext : in String) return Boolean is
      pragma Unreferenced (L, Ext);
   begin
      return False;
   end Is_Extension;

   ----------
   -- List --
   ----------

   procedure List is
   begin
      for K in 1 .. Index loop
         declare
            L_Name : constant String := Name (Lang_Set (K).all);
         begin
            if L_Name /= "unknown" then
               Text_IO.Put_Line ("   " & L_Name);
            end if;
         end;
      end loop;
   end List;

   ----------
   -- Name --
   ----------

   function Name (L : in Lang) return String is
   begin
      return To_String (L.Name);
   end Name;

   --------------
   -- Register --
   --------------

   procedure Register (L : in Lang'Class; Name : in String) is
   begin
      Index := Index + 1;
      Lang_Set (Index) :=  new Lang'Class'(L);
      Lang_Set (Index).Name := To_Unbounded_String (Name);
   end Register;

   ----------------------
   -- Run_Syntax_Check --
   ----------------------

   function Run_Syntax_Check
     (L : in Lang; Filename : in String) return Boolean is
      pragma Unreferenced (L, Filename);
   begin
      return True;
   end Run_Syntax_Check;

   -------------------------
   -- Set_Comment_Dot_EOL --
   -------------------------

   procedure Set_Comment_Dot_EOL
     (L    : in Lang_Access;
      Mode : in Boolean) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Comment_Dot_EOL (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Comment_Dot_EOL := Mode;
      end if;
   end Set_Comment_Dot_EOL;

   ---------------------------
   -- Set_Copyright_Pattern --
   ---------------------------

   procedure Set_Copyright_Pattern
     (L       : in Lang_Access;
      Pattern : in String) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Copyright_Pattern (Lang_Set (K), Pattern);
         end loop;

      else
         L.C.Copyright_Pattern := To_Unbounded_String (Pattern);
      end if;
   end Set_Copyright_Pattern;

   ---------------------------
   -- Set_Copyright_Present --
   ---------------------------

   procedure Set_Copyright_Present
     (L    : in Lang_Access;
      Mode : in Boolean) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Copyright_Present (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Copyright_Present := Mode;
      end if;
   end Set_Copyright_Present;

   ------------------------
   -- Set_Copyright_Year --
   ------------------------

   procedure Set_Copyright_Year
     (L    : in Lang_Access;
      Mode : in Boolean) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Copyright_Year (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Copyright_Year := Mode;
      end if;
   end Set_Copyright_Year;

   ------------------------------
   -- Set_Duplicate_Blank_Line --
   ------------------------------

   procedure Set_Duplicate_Blank_Line
     (L    : in Lang_Access;
      Mode : in Checks.Mode) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Duplicate_Blank_Line (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Duplicate_Blank_Line := Mode;
      end if;
   end Set_Duplicate_Blank_Line;

   ---------------------
   -- Set_Header_Size --
   ---------------------

   procedure Set_Header_Size
     (L    : in Lang_Access;
      Size : in Natural) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Header_Size (Lang_Set (K), Size);
         end loop;

      else
         L.C.Header_Size := Size;
      end if;
   end Set_Header_Size;

   ---------------------
   -- Set_Line_Ending --
   ---------------------

   procedure Set_Line_Ending
     (L      : in Lang_Access;
      Ending : in Checks.Line_Ending_Style) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Line_Ending (Lang_Set (K), Ending);
         end loop;

      else
         L.C.Line_Ending := Ending;
      end if;
   end Set_Line_Ending;

   -------------------------
   -- Set_Line_Length_Max --
   -------------------------

   procedure Set_Line_Length_Max
     (L      : in Lang_Access;
      Length : in Positive) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Line_Length_Max (Lang_Set (K), Length);
         end loop;

      else
         L.C.Line_Length_Max := Length;
      end if;
   end Set_Line_Length_Max;

   ----------------------
   -- Set_Operator_EOL --
   ----------------------

   procedure Set_Operator_EOL
     (L    : in Lang_Access;
      Mode : in Checks.Mode) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Operator_EOL (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Operator_EOL := Mode;
      end if;
   end Set_Operator_EOL;

   -----------------------
   -- Set_Space_Comment --
   -----------------------

   procedure Set_Space_Comment
     (L      : in Lang_Access;
      Number : in Natural) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Space_Comment (Lang_Set (K), Number);
         end loop;

      else
         L.C.Space_Comment := Number;
      end if;
   end Set_Space_Comment;

   ----------------------
   -- Set_Syntax_Check --
   ----------------------

   procedure Set_Syntax_Check
     (L    : in Lang_Access;
      Mode : in Boolean) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Syntax_Check (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Check_Syntax := Mode;
      end if;
   end Set_Syntax_Check;

   --------------------
   -- Set_Tabulation --
   --------------------

   procedure Set_Tabulation
     (L    : in Lang_Access;
      Mode : in Checks.Mode) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Tabulation (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Tabulation := Mode;
      end if;
   end Set_Tabulation;

   ---------------------
   -- Set_Then_Layout --
   ---------------------

   procedure Set_Then_Layout
     (L    : in Lang_Access;
      Mode : in Checks.Mode)
   is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Then_Layout (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Then_Layout := Mode;
      end if;
   end Set_Then_Layout;

   -------------------------
   -- Set_Trailing_Spaces --
   -------------------------

   procedure Set_Trailing_Spaces
     (L    : in Lang_Access;
      Mode : in Checks.Mode) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_Trailing_Spaces (Lang_Set (K), Mode);
         end loop;

      else
         L.C.Trailing_Spaces := Mode;
      end if;
   end Set_Trailing_Spaces;

   ------------------
   -- Set_With_Use --
   ------------------

   procedure Set_With_Use
     (L    : in Lang_Access;
      Mode : in Checks.Mode) is
   begin
      if L = null then
         for K in 1 .. Index loop
            Set_With_Use (Lang_Set (K), Mode);
         end loop;

      else
         L.C.With_Use := Mode;
      end if;
   end Set_With_Use;

   -----------------------------
   -- Start_Multiline_Comment --
   -----------------------------

   function Start_Multiline_Comment (L : in Lang) return String is
      pragma Unreferenced (L);
   begin
      return "";
   end Start_Multiline_Comment;

end Languages;
