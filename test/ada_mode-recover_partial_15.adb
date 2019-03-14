--  Raised exception in recover



                             declare
                              Min_rhs := Min ();
                             begin
                                                              Possible_Left_Recursive and
                              All_Sequences (ID)(Min_RHS).Left_Recursive;

                        All_Sequences (Nonterm)(RHS).Sequence.Append (Min (All_Sequences (ID), RHS_Set (ID)).Sequence);
                     end if;
                  end;
               end loop;
               RHS_Set (Nonterm)(RHS) := True;
               if Trace_Generate > Extra then
                  Ada.Text_IO.Put_Line
                    (Trimmed_Image (Production_ID'(Nonterm, RHS)) & " => " &
                       Image (All_Sequences (Nonterm)(RHS), Descriptor));
