--  Abstract :
--
--  Test partial parser
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

-- FIXME: Debugging
--EMACSCMD:(setq debug-on-error t)

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

-- emulate font-lock (disabled below for debugging); parse in 500 byte chunks
--EMACSCMD:(wisi-validate-cache 1 501 t 'face)
--EMACSCMD:(wisi-validate-cache 501 1001 t 'face)
--EMACSCMD:(wisi-validate-cache 1001 1501 t 'face)
--EMACSCMD:(wisi-validate-cache 1501 2001 t 'face)
--EMACSCMD:(wisi-validate-cache 2001 (point-max) t 'face)
pragma License (Gpl);
procedure Ada_Mode.Partial_Parse
is
   --  test-face will invoke the parser on Nested, not the full file.
   procedure Nested
   is
      --EMACSCMD:(test-face "Ada" font-lock-function-name-face)
      use Ada.Strings;

      A : Boolean;
      I : Integer;
   begin

      if A then
         I := I + 1 -- missing semicolon
      end if;
   end Nested;

   --  There can be several compilation_units in a requested parse region.
   --EMACSCMD:(progn (forward-line 2) (insert "  ") (forward-line 4) (insert "  ") )
   --EMACSCMD:(wisi-indent-region (line-beginning-position 2) (line-end-position 5))
   procedure Small_1 is begin null; end Small_1;
   procedure Small_2 is begin null; end Small_2;
   procedure Small_3 is begin null; end Small_3;
   procedure Small_4 is begin null; end Small_4;
   --EMACSCMD:(progn (forward-line -4) (current-indentation))
   --EMACSRESULT: 3
   --EMACSCMD:(progn (forward-line -4) (current-indentation))
   --EMACSRESULT: 3
begin
   Nested;

end Ada_Mode.Partial_Parse;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- wisi-disable-face: t
-- End:
