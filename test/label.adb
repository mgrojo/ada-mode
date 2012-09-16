--  test effect of ada-label-indent, and indentation of statement
--  following goto label.

--EMACSCMD: (setq ada-label-indent 0)
procedure Label is
   I : Integer := 1;
begin
   <<Start>>
   I := I + 1;
end Label;
