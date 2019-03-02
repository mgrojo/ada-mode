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
     (Arg_1 : Integer;
      Arg_2 : Integer);
   is
      --EMACSCMD:(test-face "Ada" font-lock-function-name-face)
      use Ada.Strings; -- target 1

      --EMACSCMD:(progn (wisi-backward-statement-keyword) (looking-at "; -- target 1"))
      --EMACSRESULT: t

      -- The expanded region is all of Nested
      --EMACSCMD:(progn (forward-line 1)(ada-align))
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

   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-sexp 3)(looking-at "; -- target 2"))
   --EMACSRESULT: t
   procedure Forward
   is begin
      null;
   end Forward; -- target 2

   --EMACSCMD:(progn (forward-line 7)(goto-char (line-end-position))(backward-sexp 2)(looking-at "begin -- target 3"))
   --EMACSRESULT: t
   --EMACSCMD:(progn (forward-line 3)(forward-word 2)(backward-sexp 3)(looking-at "procedure Backward"))
   --EMACSRESULT: t
   procedure Backward
   is begin -- target 3
      null;
   end Backward;

   -- The expanded region ends at 'begin'; ensure align works
   --EMACSCMD:(progn (forward-line 1)(ada-align))
   C : Integer;
   D : Integer;
begin
   Nested;

   -- interactive edit of structures that change indent

   --EMACSCMD:(progn (end-of-line 3)(ada-indent-newline-indent)(back-to-indentation)(current-column))
   --EMACSRESULT: 6
   begin
      A;
   end;

   case Data.Post_Parse_Action is
      --  Parse region starts after 'end;'
      --EMACSCMD:(progn (end-of-line 3)(ada-indent-newline-indent)(back-to-indentation)(current-column))
      --EMACSRESULT: 9
      when Navigate =>
         loop
            Put (Cache);
         end loop;

         -- Parse region starts after 'end loop;'
         -- indent leading comment
         --EMACSCMD:(progn (forward-line -1)(delete-char 2)(indent-for-tab-command)(back-to-indentation)(current-column))
         --EMACSRESULT: 6

         --EMACSCMD:(progn (end-of-line 3)(ada-indent-newline-indent)(back-to-indentation)(current-column))
         --EMACSRESULT: 9
      when Face =>
         --EMACSCMD:(progn (end-of-line 4)(ada-indent-newline-indent)(back-to-indentation)(current-column))
         --EMACSRESULT: 9
         Resolve_Anchors
           (Data       => User_Data,
            Descriptor => User_Descriptor);
   end case;

   if A then
      B;
   else
      C;
   end if;
   -- blank line before "end"
   --EMACSCMD:(progn (forward-line 3)(wisi-indent-line)(back-to-indentation)(current-column))
   --EMACSRESULT: 3


end Ada_Mode.Partial_Parse;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- wisi-disable-face: t
-- End:
