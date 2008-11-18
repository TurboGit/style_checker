------------------------------------------------------------------------------
--                              Style Checker                               --
--                                                                          --
--                   Copyright (C) 2006-2008, Pascal Obry                   --
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

package body Languages.Script is

   Handler : Lang;

   -------------
   -- Comment --
   -------------

   overriding function Comment (L : in Lang) return String is
      pragma Unreferenced (L);
   begin
      return "#";
   end Comment;

   ------------------
   -- Is_Extension --
   ------------------

   overriding function Is_Extension
     (L : in Lang; Ext : in String) return Boolean is
      pragma Unreferenced (L);
   begin
      return Ext = "sh" or else Ext = "csh" or else Ext = "zsh";
   end Is_Extension;

begin
   Register (Handler, "Script");
end Languages.Script;
