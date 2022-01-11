-- FIXME: incremental parse gets "error during resume".
-- partial parse gets bad indent
--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
--EMACSCMD:(switch-to-lr1)
package body Ada_Mode.Recover_45 is


   procedure Edit_Tree
     (Parser : in out Base_Parser'Class;
      Edits  : in     Kmn_Lists.List)
   is
   begin
   Kmn_Loop :
      loop
         declare
         begin
            declare
            begin
               if Do_Scan then
               Scan_Changed_Loop :
                  loop
                     declare
                     begin
                        exit Scan_Changed_Loop when Token.Id = Parser.Tree.Lexer.Descriptor.Eoi_Id;

                        if Token.Id >= Parser.Tree.Lexer.Descriptor.First_Terminal and then
                          Comment_End_Inserted or Comment_Start_Deleted
                        then
                           Comment_End_Inserted  := False;
                        end if;
                     end;
                  end loop Scan_Changed_Loop;
               end if;

               if Do_Scan then
                  Delete_Scanned_Loop :
                  loop
                     exit Delete_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

                  end loop Delete_Scanned_Loop;
               end if;
            end;
      end loop KMN_Loop;

   end Edit_Tree;

end Ada_Mode.Recover_45;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
