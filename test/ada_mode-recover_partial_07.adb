-- Recover failed with enqueue_limit. FIXME: Not yet fixed; requires
-- minimal_complete with 'end' target instead of 'wisi_eoi'.
--
--EMACS_SKIP_UNLESS: nil

begin
New_Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);
                     Insert_Count            := Insert_Count + 1;

                     Do_Shift
                       (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Action.ID,
                        Cost_Delta,
                        Strategy   => Minimal_Complete);
                  end;
                  end if;
                  end case;
