--  Used to encounter some problem in recover.

procedure Ada_Mode.Recover_34
is begin
   for Item_State of Item_States loop
      case Item_State.Label is
         when Unknown =>
            raise Programmer_Error with "Item_State.Label = Unknown";

         when Always_Keep =>
            if
         when Keep_If_Minimal =>
            if Min_Length < Length then
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_Io.Put_Line ("delete " & Length'Image & " >" & Min_Length'Image);
               end if;
               Delete;
            else
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_Io.Put_Line
                    ("keep " & Image (Constant_Ref (I).Prod) & Length'Image & " =" & Min_Length'Image);
               end if;
               Next (I);
               Kernel_Index := Kernel_Index + 1;
            end if;

      end case;
   end loop;
end Ada_Mode.Recover_34;
