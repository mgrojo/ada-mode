--  Other tests for Match_Names (see ada_mode-recover_match_names.adb).
--  These cover Missing or Extra names.

package body Ada_Mode.Recover_Match_Names_02 is

   --  Same as WisiToken test_mckenzie_recover.adb Extra_Name_1.
   procedure Process_Csv_File is
      procedure To_Month is
      begin
         --  Missing 'null; end To_Month;' here.
         begin
      null;
   end Process_Csv_File; -- Extra_Name_Error here.
end Ada_Mode.Recover_Match_Names_02;
