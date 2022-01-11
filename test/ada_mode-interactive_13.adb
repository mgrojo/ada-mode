package body Test_Edit_Source is
   -- No header comment; part of test. From a real editing session.
   --EMACSCMD:(setq skip-recase-test t)

   --EMACSCMD:(progn (goto-char (point-min))(wisi-replay-kbd-macro "-- Comment\r"))
   --EMACSCMD:(progn (wisi-replay-undo 11)(wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT:0

   procedure Non_Ascii (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use Wisi.Parse_Context;
      use WisiToken;

      --  Same as WisiToken test_incremental.adb Non_Ascii

      Initial_Source : constant String :=
        "package A is Theta : Wide_Character := ""Θ""; B : F := ""π_Non""; end A;";
      --      |6  |10       |20       |30       |40

      Expected_Source : constant String :=
        "package A is Theta : Wide_Character := ""ΠΘ""; B : F := ""pi_Non""; end A;";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      Changes.Append ((41, 41, 43, 42, +"Π", 0, 0));
      Changes.Append ((58, 57, 61, 60, +"pi_", 3, 2)); -- delete "π_"

      Expected_KMN_List.Append ((40, 40, 2, 1, 0, 0));
      Expected_KMN_List.Append ((15, 15, 3, 3, 3, 2));
      Expected_KMN_List.Append ((12, 13, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Non_Ascii;

end Test_Edit_Source;
