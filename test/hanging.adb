--  Test ada-indent-hanging-rel-exp

procedure Hanging
is
   X_Long_Name : Integer;
   A, B, C     : Boolean := True;

   function F_Long_Name (A, B : in Boolean) return Integer
   is begin
      return 42;
   end F_Long_Name;

begin
   --EMACSCMD:(progn (forward-line 5)(ada-align)(forward-line -2)(current-indentation))
   --EMACSRESULT: 5
   X_Long_Name
     := F_Long_Name
          (A => True,
           B => True);

   if A
        and then B
        and then C
        /= A
   then
      null;
   end if;
end Hanging;
--  Local Variables:
--  ada-indent-hanging-rel-exp: t
--  end:
