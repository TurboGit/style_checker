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

with Ada.Command_Line;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body Languages.Ada is

   use Standard.Ada.Text_IO;
   use GNAT;
   use type OS_Lib.String_Access;

   Handler : Lang;

   GCC : constant OS_Lib.String_Access := OS_Lib.Locate_Exec_On_Path ("gcc");

   -------------
   -- Comment --
   -------------

   function Comment (L : in Lang) return String is
   begin
      return "--";
   end Comment;

   ------------------
   -- Is_Extension --
   ------------------

   function Is_Extension (L : in Lang; Ext : in String) return Boolean is
   begin
      return Ext = "ads" or else Ext = "adb" or else Ext = "ada";
   end Is_Extension;

   ----------------------
   -- Run_Syntax_Check --
   ----------------------

   function Run_Syntax_Check
     (L : in Lang; Filename : in String) return Boolean
   is
      use type OS_Lib.Argument_List;

      Compile : aliased String := "-c";
      Syntax  : aliased String := "-gnats";
      File    : aliased String := Filename;

      Args    : OS_Lib.Argument_List :=
                  (Compile'Unchecked_Access,
                   Syntax'Unchecked_Access) &
                  L.Get_Style_Checker_Parameters &
                  (1 => File'Unchecked_Access);
      Success : Boolean := True;
   begin
      if GCC = null then
         Put_Line
           (Standard_Error, "can't check Ada style, missing gcc");
      else
         OS_Lib.Spawn (GCC.all, Args, Success);
      end if;
      return Success;
   end Run_Syntax_Check;

begin
   Register (Handler, "Ada");
end Languages.Ada;
