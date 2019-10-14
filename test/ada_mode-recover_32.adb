--  Encounter lots of "config full" in try_insert_string_quote

procedure Apply_Clues
is
begin
   loop
      if Run = 0 and then Clue_Index <= Max_Clue_Index then
         case Get_Cell (Line, Pos) is
            when Black =>
               if Run > 0 then
                  if First_Black (Clue_Index) = 0 then
                     First_Black (Clue_Index) := Run;
                  end if;
                  Dot := Dot_Black;
                  Run := Run - 1;
               else
                  --  "extra" black after all dots placed. Move last run to include this
                  --  black. Check to see if we can place crosses between black runs.
                  pragma Assert (Clue_Index > Max_Clue_Index);
                  Clue_Index := Max_Clue_Index;
                  Current_Run_Start := Pos - Clues (Clue_Index);
                  declare
                     Old_Pos : Cell_Index := Last_Run_Start;
                     New_Pos : Cell_Index := Current_Run_Start;
                  begin
                     if Current_Run_Start > Last_Run_End then
                        --  No overlap
                        for I in 1 .. Clues (Clue_Index) loop
                           case Get_Cell (Old_Pos) is
                              when White => raise Sal.Programmer_Error;
                              when Black => null;
                              when Dots => Set_Cell (Line, Old_Pos, White);
                           end case;
                           Set_Cell (Line, New_Pos, Dot_White);
                        end loop;
                     else
                        raise Sal.Not_Implemented with "extra black
               end if;

         end case;
      end if;
   end loop;

end Apply_Clues;
