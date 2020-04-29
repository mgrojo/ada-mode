-- From a real editing session

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(setq skip-recase-test t)

package body Wisi.Libadalang is

   procedure To_WisiToken_Tree (Parser : in out Wisi.Libadalang.Parser)
   is
      function Create_Tree_Node (Ast_Node : in Ada_Node) return Create_Result
      is
         W_Token_ID : WisiToken.Token_ID := L_Node_To_Wisi_ID (Kind (Ast_Node));
      begin
         if W_Token_ID = No_Direct_Map then
         else
            declare
               use WisiToken.Syntax_Trees;

               procedure Find_Production
               is begin
               --  extra 'begin'. The cheapest solution inserts 'begin' after 'Prod
               -- :', turning that into a block label.
               Prod : WisiToken.Productions.Instance renames Grammar
                 (if K = 0 then Prod_ID.LHS else LHS_Descendants (K));
                     begin
                        for I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                           declare
                              use all type SAL.Base_Peek_Type;
                           begin
                              for J in Tokens.First_Index .. Tokens.Last_Index loop
                              end loop;
                           end;
                           Prod_ID.RHS := I;
                           return;
                        <<Next_RHS>>
                        end loop;
                        exit when not K > Descendants.Last_Index;
                        K := K + 1;
                     end loop; --  missing 'loop'
                     raise SAL.Programmer_Error with "production not found";
               end Find_Production;
            begin
               Find_Production;
            end;
         end if;
      end Create_Tree_Node;
   begin
   end To_WisiToken_Tree;

end Wisi.Libadalang;
-- Error recovery has a race condition; force it to return repeatable results
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
