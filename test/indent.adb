with Ada.Text_IO;
package body Indent is
   -- 1.  The indentation following a for loop that has used a multiple line
   --     range descriptor is incorrect.
   --
   -- Workaround:  It will indent reasonably if "loop" is moved to its own line

   package body IO_CPU_Control_State_S_Pkg
   is
      procedure Step_1 is null;
      procedure Step_2 is null;
      procedure Step_3 is null;
      procedure Step_4 is null;
      type Week_Day is (Monday, Tuesday, Wednesday, Thursday, Friday);
      procedure Test is
      begin
         Step_1;
         for Day in Week_Day range Monday .. Wednesday loop
            Step_2;
            Step_3;
         end loop;
         Step_4;  --  Correctly indented on "end loop"

         for Day in Monday ..
           Wednesday loop
            Step_2;  --  Correctly indented on "for" + 3
            Step_3;
         end loop;
         Step_4;

         for Day in IO_CPU_Control_State_S_Pkg.Week_Day range
           IO_CPU_Control_State_S_Pkg.Monday .. IO_CPU_Control_State_S_Pkg.Wednesday loop
            Step_2;
            Step_3;
         end loop;

         Step_4;  --  Correctly indented on "for"

         for Day in IO_CPU_Control_State_S_Pkg.Week_Day range
           IO_CPU_Control_State_S_Pkg.Monday .. IO_CPU_Control_State_S_Pkg.Wednesday
         loop
            Step_2;
            Step_3;
         end loop;

         Step_4;  --  Correctly indented on "for"
      end Test;
   end IO_CPU_Control_State_S_Pkg;


   --  ada-mode should ignore strings and comments when
   --  looking for keywords such a "record" and "private".

   procedure Toplevel is
   begin
      Ada.Text_IO.Put_Line ("Record" &
                              "Record a new macro");
      null;  --  Was uncorrectly indented
   end Toplevel;

end Indent;
