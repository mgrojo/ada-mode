-- -*- ada-indent-when: 3 -*-

--  7402-004
--  Indentation problem for a 'begin' in a 'case... when' statement

procedure Adacore_7402_004 is
   type Action_Type is (Ma_Process_Products, Ma_Open_Module, Ma_Close_Module, Ma_Setup);
   package Info is
      Action : Action_Type := Action_Type'First;
   end Info;
begin
   case Info.Action is
      when Ma_Process_Products | Ma_Open_Module | Ma_Close_Module =>
         begin     --  <=== Wrong indent
            case Info.Action is
               when Ma_Process_Products =>
                  null;
               when Ma_Open_Module =>
                  null;
               when others =>
                  null;
            end case;
         exception
            when E : others =>
               null;
         end;
      when Ma_Setup =>
         null;
   end case;
end Adacore_7402_004;
