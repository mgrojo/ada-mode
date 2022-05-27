-- From a real editing session. Encounters FAIL_ENQUEUE_LIMIT.
-- FIXME: need separate minimal_complete queue.
--EMACS_SKIP_UNLESS: nil
procedure Ada_Mode.Recover_51
is
begin
      declare
         Err_Ref : Stream_Error_Ref := Tree.First_Error (Stream);
      begin
         loop
            exit when not Has_Error (Err_Ref);
            declare
               Err       : constant Error_Data'Class := Error (Err_Ref);
               Error_Ref : constant Stream_Node_Ref  := Tree.Error_Node (Err_Ref);
            begin
               if Err in Lexer_Error then
                  --  We don't know Shift_Bytes yet, so we can't find Scan_Start_Node or Scan_End.
                  Lexer_Errors.Append
                    ((Error_Ref.Node,
                      Edit_Region => Lexer.Find_Edit_Region (Tree.ID (Err_Ref.Node), Tree.
                      others => <>));
                  Tree.Next_Error (Err_Ref);
               else
                  if Trace_Incremental_Parse > Detail then
                     Tree.Lexer.Trace.Put_Line ("delete " & Err.Image (Tree, Error_Ref.Node));
                  end if;
                  Tree.Delete_Error (Err_Ref);
               end if;
            end;
         end loop;
      end;
end Ada_Mode.Recover_51;
