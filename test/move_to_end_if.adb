--EMACSCMD: (progn (goto-line 9)(forward-char 3)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 13
--EMACSCMD: (progn (goto-line 9)(forward-char 4)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 13

procedure Move_To_End_If is
   I : Integer := 0;
begin
   if I = 1 then
      I := 2;
   else
      I := 3;
   end if;
end Move_To_End_If;
