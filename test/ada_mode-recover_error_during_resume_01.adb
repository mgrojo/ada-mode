declare
            Value      : constant String := Get_Text (Data, Tree, Node, Strip_Quotes => True);
            Name       : constant String := Ada.Characters.Handling.To_Upper (Value);
            Name_Ident : Identifier_Index;
            Kind       : Valid_Node_Index;
            Node       : Node_Index      := Tree.Root;
            Found : Boolean := False;
            begin
               --  See if Name is already declared
               loop
                  if (Tree.ID (Node) = +declaration_ID and Tree.RHS_Index = 0) then
                     declare
                     ID : constant Identifier_Index := Tree.Identifier (Tree.Child (Node, 3));
                     begin
                        if Data.Identifiers (ID) = Name then
                        Name_Ident := ID;
                        Found := True;

               end loop;
               Name_Ident := New_Identifier (Name);
                  Tree.Set_Children
                    (Tree.Parent (Node),
                     (+rhs_item_ID, 0),
                     (1 => Tree.Add_Identifier (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node))));
               if GNAT.Regexp.Match (Value, Symbol_Regexp) then
                  declare
                     Keyword : constant Valid_Node_Index := Tree.Add_Identifier
                       (+KEYWORD_ID, New_Identifier ("keyword"), Tree.Byte_Region (Node));
                  begin
                     Kind := Tree.Add_Nonterm ((+token_keyword_non_grammar_ID, 0), (1 => Keyword));
                  end;
               else
                  declare
                     Token : constant Valid_Node_Index := Tree.Add_Identifier
                       (+Wisitoken_Grammar_Actions.TOKEN_ID, New_Identifier ("token"), Tree.Byte_Region (Node));
                     Less : constant Valid_Node_Index := Tree.Add_Identifier
                       (+LESS_ID, Less_Ident, Tree.Byte_Region (Node));
                     Punc : constant Valid_Node_Index := Tree.Add_Identifier
                       (+IDENTIFIER_ID, Punctuation_Ident, Tree.Byte_Region (Node));
                     Greater : constant Valid_Node_Index := Tree.Add_Identifier
                       (+GREATER_ID, Greater_Ident, Tree.Byte_Region (Node));
                  begin
                     Kind := Tree.Add_Nonterm ((+token_keyword_non_grammar_ID, 0), (Token, Less, Punc, Greater));
                  end;
               end if;

               declare
                  Ident          : constant Valid_Node_Index := Tree.Add_Identifier
                    (+IDENTIFIER_ID, New_Identifier (Value), Tree.Byte_Region (Node));
                  Decl_Item      : constant Valid_Node_Index := Tree.Add_Nonterm
                    ((+declaration_item_ID, 1), (1      => Ident));
                  Decl_Item_List : constant Valid_Node_Index := Tree.Add_Nonterm
                    ((+declaration_item_list_ID, 0), (1 => Decl_Item));

                  Percent : constant Valid_Node_Index := Tree.Add_Identifier
                    (+PERCENT_ID, New_Identifier ("percent"), Tree.Byte_Region (Node));
                  Name    : constant Valid_Node_Index := Tree.Add_Identifier
                    (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node));
                  Decl    : constant Valid_Node_Index := Tree.Add_Nonterm
                    ((+declaration_ID, 0), (Percent, Kind, Name, Decl_Item_List));
               begin
                  Add_Compilation_Unit (Decl, Prepend => True);
               end;
            end;
