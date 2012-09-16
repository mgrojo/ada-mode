--  Indentation problem with ada-mode 3.2
--  Incorrect alignment on opening parenthesis

procedure Indent3 is
   type Wait_Return is (Read_Success, Read_Timeout, Wait_Timeout,
                        Nothing_To_Wait_For_In_Wait_List); -- Should be indented on '('

begin
   null;
end Indent3;
