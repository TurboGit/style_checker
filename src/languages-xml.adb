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

package body Languages.XML is

   Handler : Lang;

   ------------------
   -- Is_Extension --
   ------------------

   function Is_Extension (L : in Lang; Ext : in String) return Boolean is
   begin
      return Ext = "xml" or else Ext = "html"
        or else Ext = "txml" or else Ext = "thtml";
   end Is_Extension;

begin
   Register (Handler, "XML");
end Languages.XML;
