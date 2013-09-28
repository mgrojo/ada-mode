--  test ada-skel.el; does not compile
--
--  We expand each token in ada-skel-token-alist, after first deleting
--  the expected expansion. The result is checked by diff.
package body Ada_Skel is
begin
   --  ada-skel-case
   --EMACSCMD:(progn (end-of-line 2)(kill-word -1)(forward-char 1) (kill-line 3)(forward-char -2)(funcall ada-expand))
   case A is
      when =>

   end case;

   --  ada-skel-declare with block name
   --EMACSCMD:(progn (forward-line 1)(kill-line 1)(forward-word 1)(forward-line 1)(kill-line 2)(forward-char -1)(insert " Block_1")(funcall ada-expand))
Block_1:
   declare
   begin
   end Block_1;

   -- FIXME: test rest
end Ada_Skel;
