-- Test navigation on case statement before and after editing it

-- Does not compile; missing bodies, undefined vars.

-- Since we are editing, the syntax will be illegal at times; don't fail for that.
--EMACSCMD:(setq wisi-debug 0)

-- Test the buffer does parse initially
--EMACSCMD:(progn (wisi-parse-buffer 'face) (marker-position (wisi-cache-max 'face)))
--EMACSRESULT:(point-max)

procedure Ada_Mode.Interactive_Case_Statement
is
begin
   --EMACSCMD:(progn (forward-line 4)(back-to-indentation)(forward-sexp)(looking-at "when 1"))
   --EMACSRESULT: t
   --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(forward-sexp)(looking-at "when 2"))
   --EMACSRESULT: t
   case A is
      when 1 =>
         Stuff_1;
      when 2 =>
         Stuff_2;
         --EMACSCMD: (progn (forward-line 2)(kill-line 2))
         --EMACSCMD: (progn (forward-line 1)(forward-sexp)(forward-sexp -2)(looking-at "when 1"))
      when 3 =>
         Stuff_3;
   end case;
   --EMACSCMD: (progn (forward-line -1)(yank))

end Ada_Mode.Interactive_Case_Statement;
