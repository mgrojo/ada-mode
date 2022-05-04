-- From a real editing session. Found a bug in recording a deleted node.

--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
--EMACSCMD:(setq skip-reindent-test t skip-recase-test t)

-- No "." in name to preserve test semantics.
procedure Ada_Mode_Incremental_Recover_03
  (Parser : in out Base_Parser'Class;
   Edits  : in     KMN_Lists.List)
is

begin

   --  Now process source edits. We have to start with SOI to handle
   --  edits in leading non-grammar. test_incremental.adb Edit_Comment_12
   Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));

KMN_Loop :
   loop
      declare
      begin
         declare
            use Lexer_Error_Data_Lists;
            Cur : Cursor := Lexer_Errors.First;
         begin
            loop
               exit when Cur = No_Element;

               if Tree.Byte_Region (Lexer_Errors (Cur).Node, Trailing_Non_Grammar => False).First in
                 Stable_Region.First .. Next_KMN_Stable_First
               then
                  --  Now we know Shift for this lexer error.
                  declare
                     Node : constant Valid_Node_Access := Lexer_Errors (Cur).Node;
                  begin
                     --EMACSCMD:(progn (delete-region (line-beginning-position 5)(line-beginning-position 8))(wisi-parse-incremental-none))
                     --EMACSCMD:(progn (delete-region (line-beginning-position 25)(line-beginning-position 26))(wisi-parse-incremental-none))
                     --EMACSCMD:(progn (delete-region (line-beginning-position 15)(line-beginning-position 23))(wisi-parse-incremental-none))
                     --EMACSCMD:(progn (delete-region (line-beginning-position 2)(line-beginning-position 3))(wisi-parse-incremental-none))
                     --  Node has not yet been shifted.
                     if Tree.Lexer.Is_Block_Delimited (Tree.ID (Node)) then
                        --  test_incremental.adb Edit_String_09, Lexer_Errors_03.
                        declare
                           Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Node, Trailing_Non_Grammar => False);
                        begin
                           Lexer_Errors (Cur).Scan_End := Tree.Lexer.Find_Scan_End
                             (Tree.ID (Node), Node_Byte_Region + Shift_Bytes +
                                (if Node_Byte_Region.First > Stable_Region.Last
                                 then 0
                                 else KMN.Inserted_Bytes),
                              Inserted  => True,
                              Start     => True);
                        end;
                     else
                        --  The lexer error occurred while scanning the token or one of the
                        --  following non_grammars. test_incremental.adb Lexer_Errors_04.
                        declare
                           Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Node, Trailing_Non_Grammar => True);
                        begin
                           Lexer_Errors (Cur).Scan_End := Node_Byte_Region.Last;
                        end;
                     end if;
                  end;

                  if Lexer_Errors (Cur).Scan_End <= Stable_Region.Last + Shift_Bytes then
                     --  This lexer error is not fixed by these edits.
                     declare
                        To_Delete : Cursor := Cur;
                     begin
                        Next (Cur);
                        Lexer_Errors.Delete (To_Delete);
                     end;
                  else
                     --  We must scan from this lexer error to find out if it is fixed.
                     if Trace_Lexer > Outline then
                        declare
                           Data : Lexer_Error_Data renames Lexer_Errors (Cur).Element.all;
                        begin
                           Tree.Lexer.Trace.Put_Line
                             ("lexer error on " & Tree.Image (Data.Node, Node_Numbers => True) &
                                " possibly fixed by this KMN; scan end" & Data.Scan_End'Image);
                        end;
                     end if;
                     Next (Cur);
                  end if;
               else
                  exit;
               end if;
            end loop;
         end;

      end;
   end loop KMN_Loop;

   if Tree.ID (Terminal.Node) /= Parser.Tree.Lexer.Descriptor.EOI_ID then
      raise User_Error with "edit list does not cover entire tree";
   end if;

   if not Floating_Non_Grammar.Is_Empty then
      raise SAL.Programmer_Error with "floating_non_grammar not emptied: " & Lexer.Image
        (Floating_Non_Grammar, Tree.Lexer.Descriptor.all);
   end if;

   if Debug_Mode then
      declare
         Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
      begin
         Parser.Tree.Validate_Tree (Parser.User_Data.all, Error_Reported, Node_Index_Order => False);
         if Error_Reported.Count /= 0 then
            if Trace_Incremental_Parse > Outline then
               Tree.Lexer.Trace.Put_Line ("edit_tree: validate_tree failed");
               Tree.Print_Streams (Children => True, Non_Grammar => True);
            end if;
            raise WisiToken.Parse_Error with "edit_tree: validate_tree failed";
         end if;
      end;
   end if;
end Ada_Mode_Incremental_Recover;
