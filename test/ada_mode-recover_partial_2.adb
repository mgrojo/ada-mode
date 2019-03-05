   procedure Update_First
     (Data   : in out Parse_Data_Type;
      Errors : in     WisiToken.Parse.LR.Parse_Error_Lists.List)
   is
      use all type WisiToken.Parse.LR.Config_Op_Label;
   begin
      for Error of Errors loop
         for Op of Error.Recover.Ops loop
            case Op.Op is
            when Delete =>
               if Data.Terminals (Op.Token_Index).First then
               if Op.Token_Index = Data.Terminals.Last_Index then
-- newline here encounters enqueue limit.

                    (not Data.Terminals (Op.Token_Index + 1).First))
               then
                  declare
                     Deleted_Term : Augmented_Token renames Data.Terminals (Op.Token_Index);
                     Term : Augmented_Token renames Data.Terminals (Op.Token_Index + 1);
                  begin
                     Term.First             := True;
                     Term.First_Indent_Line := Deleted_Term.First_Indent_Line;
                     Term.Last_Indent_Line  := Deleted_Term.Last_Indent_Line;
                  end;
               end if;
            when others =>
               null;
            end case;
         end loop;
      end loop;
   end Update_First;
