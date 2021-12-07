-- Recover failed with ENQUEUE_LIMIT
                        procedure Handle_Non_Grammar
                          (Non_Grammar : in out WisiToken.Lexer.Token_Arrays.Vector;
                           Floating    : in     Boolean)
                        is
                           Last_Byte : constant Buffer_Pos :=
                             (if Non_Grammar.Length = 0
                              then Buffer_Pos'Last
                              else Non_Grammar (Non_Grammar.Last_Index).Byte_Region.Last +
                                (if Floating then Shift_Bytes else 0));

                           Delete : SAL.Base_Peek_Type := 0;
                        begin
                           if Non_Grammar.Length = 0 then
                              --  Edit start is in whitespace before Terminal.
                              --  test_incremental.adb Edit_Whitespace_1, _2
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Tree.Line_Region (Last_Grammar).Last;
                              --  start_line test case ada_mode-incremental_02.adb
                              Do_Scan        := True;

                           elsif Last_Byte <= Scanned_Byte_Pos then
                              --  Edit start is in whitespace before Terminal.
                              --  ada_mode-incremental_04.adb
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
                              Do_Scan        := True;

                           else
                              for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                 declare
                                    Byte_Last : constant Buffer_Pos := Non_Grammar (I).Byte_Region.Last +
                                      (if Floating then Shift_Bytes else 0);
                                 begin
                                    if Byte_Last + 1 >= Inserted_Region.First and
                                      Byte_Last > Scanned_Byte_Pos
                                      --  test case: ada_mode-recover_align_1.adb, test_incremental.adb Edit_Comment_2
                                    then
                                       Delete  := I;
                                       Do_Scan := True;
                                       exit;
                                    end if;
                                 end;
                              end loop;

                              if Delete > 0 and then Non_Grammar (Delete).ID = Tree.Lexer.Descriptor.SOI_ID then
                                 if Delete = Non_Grammar.Last_Index then
                                    Delete := 0;
                                 else
                                    Delete := Delete + 1;
                                 end if;
                              end if;

                              if Delete > 0 then
                                 --  Edit is in or before Non_Grammar (Delete) (ie a comment); set
                                 --  Lex_Start_* to scan from edit start or start of Token, whichever
                                 --  is earlier.

                                 declare
                                    Token : WisiToken.Lexer.Token renames Non_Grammar (Delete);
                                 begin
                                    if (Tree.Lexer.Is_Comment (Token.ID) and
                                          Inserted_Region.First < Token.Byte_Region.Last)
                                      --  Inserting in middle of Token, not adding to end.
                                      and then Tree.Lexer.Contains_New_Line (Inserted_Region)
                                    then
                                       Comment_End_Inserted := True;
                                       --  The exposed code is in the inserted text after the new comment
                                       --  end, plus in the old comment, terminated by the old comment
                                       --  end. test_incremental.adb Edit_Comment_4, Edit_Comment_7
                                       --  New_Code_End is shifted.
                                       New_Code_End := Token.Byte_Region.Last + KMN.Inserted_Bytes + Shift_Bytes;
                                       if Trace_Incremental_Parse > Detail then
                                          Parser.Trace.Put_Line
                                            ("comment_end_inserted:" &
                                               Token.Byte_Region.First'Image & " .." &
                                               New_Code_End'Image);
                                       end if;
                                    end if;

                                    Lex_Start_Byte := Buffer_Pos'Min
                                      (Token.Byte_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region.First);

                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region_Chars.First);

                                    if Floating then
                                                --  If this token contributed to Shift_Lines, ignore that part.
                                                --  ada_mode-recover_14 comment after extra 'begin'.
                                       declare
                                          Temp_Shift_Lines : Integer := Shift_Lines;
                                       begin
                                          for I in Delete .. Non_Grammar.Last_Index loop
                                             Temp_Shift_Lines := @ + New_Line_Count (Non_Grammar (I).Line_Region);
                                          end loop;
                                          Lex_Start_Line := Token.Line_Region.First +
                                             then
                                                Shift_Lines +
                                             else 0);
                                       end;
                                    else
                                       Lex_Start_Line := Token.Line_Region.First;
                                    end if;
                                 end;

                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ((if Floating
                                        then "delete floating_non_grammar"
                                        else "delete non_grammar" & Tree.Get_Node_Index (Last_Grammar.Node)'Image) &
                                         Delete'Image & " .." & Non_Grammar.Last_Index'Image);
                                 end if;

                                 if not Floating then
                                    for I in Delete .. Non_Grammar.Last_Index loop
                                       Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    end loop;
                                 end if;

                                 Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Delete - 1);
                              else
                                 --  Edit is in whitespace between last non_grammar and Terminal
                                 Lex_Start_Byte := Inserted_Region.First;
                                 Lex_Start_Char := Inserted_Region_Chars.First;
                                 Lex_Start_Line := Tree.Line_Region (Terminal).First;
                                 Do_Scan        := True;
                              end if;
                           end if;
                        end Handle_Non_Grammar;
