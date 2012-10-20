--  This packages tests some bad indentation problems showing up
--  with ada-mode up to 3.1.
--  Reported by:steinmark_daniel@si.com
--  Date: Fri, 12 Mar 1999 16:01:41 -0500
--
--  First one : indentation problems with "record .. end record"
--
--  adamode3.4a: problems with indenting for .. use when there is no type
--  definition above

package IO_CPU_Control_State_S_Pkg
is
   --    type CPU1_Fault_Status_Type is
   --       record
   --          Unused                    : Unused_Type;
   --          Bogus_Entry               : Local_Boolean_Type;
   --          Invalid_Address_Access    : Local_Boolean_Type;
   --       end record;
   for CPU1_Fault_Status_Type use   --  Used to be aligned on "end record"
      record
         Unused                    at 0 range 20 .. 20;
         Bogus_Entry               at 0 range 21 .. 21;
         Invalid_Address_Access    at 0 range 22 .. 22;
      end record;
   for CPU1_Fault_Status_Type'Size use 32;

   type CPU2_Fault_Status_Type is  --  User to be aligned on "for"
      record
         Unused2  : Unused_Type;
         B_P_Fail : Local_Boolean_Type;
         B_L_I    : Local_Boolean_Type;
      end record;

   for CPU2_Fault_Status_Type use
      record
         Unused2   at 0 range  0 ..  6;
         B_P_Fail  at 0 range  7 ..  7;
         B_L_I     at 0 range  8 ..  8;
      end record;
   for CPU2_Fault_Status_Type'Size use 32;

end IO_CPU_Control_State_S_Pkg;

--
--  Second one : indentation problems with type qualifications
--

package B_C_Parameters
is
   type CSCL_Type is array
     (CSC_I_Type) of Very_Big_Lengthy_Named_Type;

   C_S_Controls : constant
     CSCL_Type :=
     CSCL_Type'
     (
      1 =>   --  Used to be aligned on "CSCL_Type'"
             --  aligned with previous comment.
        Very_Big_Lengthy_Named_Type'
        (Test         => B_Types.I_I_S_TEST,  -- Used to be aligned on "1 =>"
         F_Threshold  => 0,
         F_F_Action   => B_Types.N_ACTION),

      2 =>
        Very_Big_Lengthy_Named_Type'
        (Test         => B_Types.A1_A_TEST,
         F_Threshold  => 0,
         F_F_Action   => B_Types.N_ACTION),

      --  Comment to disturbe the aggregate indentation

      3 =>
        Very_Big_Lengthy_Named_Type'
        (Test         => B_Types.W_S_T_COMPLETION,
         F_Threshold  => 0,
         F_F_Action   => B_Types.N_ACTION));
end B_C_Parameters;

--
--  Third case :
--
-- 1.  The indentation following a for loop that has used a multiple line
--     range descriptor is incorrect.
--
-- Workaround:  It will indent reasonably if "loop" is moved to its own line

package body IO_CPU_Control_State_S_Pkg
is
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
      Step4;

      for Day in Some_Other_Types_Pkg.Week_Day range
        Some_Other_Types_Pkg.Monday .. Some_Other_Types_Pkg.Wednesday loop
         Step_2;
         Step_3;
      end loop;

      Step_4;  --  Correctly indented on "for"

      for Day in Some_Other_Types_Pkg.Week_Day range
        Some_Other_Types_Pkg.Monday .. Some_Other_Types_Pkg.Wednesday
      loop
         Step_2;
         Step_3;
      end loop;

      Step_4;  --  Correctly indented on "for"
   end Test;
end IO_CPU_Control_State_S_Pkg;


--  Top level indentation: ada-mode should ignore strings and comments when
--  looking for keywords such a "record" and "private".
--  Fixed on 12/14/1999

procedure body Toplevel is
begin
   Append_Item (Tool,
                Text         => "Record",
                Tooltip_Text => "Record a new macro",
                Icon         => Pixmapid);
   null;  --  Was uncorrectly indented
end Toplevel;
