--  FIXME: ada-declarative-region needs work
--EMACS_SKIP_UNLESS:nil
procedure Ada_Mode.Ancestor
is
   A : Integer;

   procedure B
   is -- Empty B
      --EMACSCMD:(progn (goto-char (cdr (ada-declarative-region (progn (forward-line 2)(back-to-indentation)(point))))) (looking-at " -- Ada_Mode.Ancestor declarative region end"))
      --EMACSRESULT:t
   begin
      --EMACSCMD:(progn (goto-char (cdr (ada-declarative-region (progn (forward-line 2)(back-to-indentation)(point))))) (looking-at " -- Empty B"))
      A := 1;
   end B;

   procedure C
   is -- Empty declarative region C

      --EMACSCMD:(progn (goto-char (cdr (ada-declarative-region (progn (back-to-indentation)(point))))) (looking-at " -- Empty declarative region C"))
      --EMACSRESULT:t
   begin
      A := 2;
   end C;

   D : Integer; -- Ada_Mode.Ancestor declarative region end

begin
   Declare_1 :
   declare
      --EMACSCMD:(progn (goto-char (cdr (ada-declarative-region (progn (forward-line 2)(back-to-indentation)(point))))) (looking-at " -- declare 1 end"))
      --EMACSRESULT:t
      A : Integer; -- declare 1 end
   begin
      null;
   end Declare_1;

   Declare_2 :
   declare --  Empty declare 2
      --EMACSCMD:(progn (goto-char (cdr (ada-declarative-region (progn (back-to-indentation)(point))))) (looking-at " --  Empty declare 2"))
      --EMACSRESULT:t
   begin
      null;
   end Declare_2;

end Ada_Mode.Ancestor;
