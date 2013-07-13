--EMACSCMD:(setq skip-recase-test t)
--  indentation problems with "record .. end record"
--
--  adamode3.4a: problems with indenting for .. use when there is no type
--  definition above

package Indent is
   package IO_CPU_Control_State_S_Pkg
   is
      type CPU1_Fault_Status_Type is
         record
            Unused                    : Boolean;
            Bogus_Entry               : Boolean;
            Invalid_Address_Access    : Boolean;
         end record;
      for CPU1_Fault_Status_Type use   --  Used to be aligned on "end record"
         record
            Unused                    at 0 range 20 .. 20;
            Bogus_Entry               at 0 range 21 .. 21;
            Invalid_Address_Access    at 0 range 22 .. 22;
         end record;
      for CPU1_Fault_Status_Type'Size use 32;

      type Unsigned_7 is mod 2**7;

      type CPU2_Fault_Status_Type is  --  User to be aligned on "for"
         record
            Unused2  : Unsigned_7;
            B_P_Fail : Boolean;
            B_L_I    : Boolean;
         end record;

      for CPU2_Fault_Status_Type use
         record
            Unused2   at 0 range  0 ..  6;
            B_P_Fail  at 0 range  7 ..  7;
            B_L_I     at 0 range  8 ..  8;
         end record;
      for CPU2_Fault_Status_Type'Size use 32;

      procedure Test;

   end IO_CPU_Control_State_S_Pkg;

   --
   --  indentation problems with type qualifications
   --

   package B_C_Parameters
   is
      type CSC_I_Type is range 1 .. 3;

      type CSCL_Type is array
        (CSC_I_Type) of IO_CPU_Control_State_S_Pkg.CPU2_Fault_Status_Type;

      C_S_Controls : constant
        CSCL_Type :=
          CSCL_Type'
            (
             1 =>  --  Used to be aligned on "CSCL_Type'"
                   --  aligned with previous comment.
               IO_CPU_Control_State_S_Pkg.CPU2_Fault_Status_Type'
                 (Unused2  => 10,  -- Used to be aligned on "1 =>"
                  B_P_Fail => False,
                  B_L_I    => True),

             2 =>
               IO_CPU_Control_State_S_Pkg.CPU2_Fault_Status_Type'
                 (Unused2  => 10,  -- Used to be aligned on "1 =>"
                  B_P_Fail => False,
                  B_L_I    => True),

             --  Comment to (try to) disturb the aggregate indentation

             3 =>
               IO_CPU_Control_State_S_Pkg.CPU2_Fault_Status_Type'
                 (Unused2  => 10,  -- Used to be aligned on "1 =>"
                  B_P_Fail => False,
                  B_L_I    => True));

   end B_C_Parameters;

end Indent;
