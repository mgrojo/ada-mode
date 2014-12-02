--EMACSCMD:(setq skip-recase-test t)
-- not covered in ada_mode*
procedure Foruse is
   type Command_Labels_Type is
     (Noop,
      Reset_HK_Counters,
      Enable,
      Disable,
      Enable_Analog,
      Disable_Analog);

   --  Test an earlier bug in wisi-backward-token, that caused
   --  improper indentation.
   for Command_Labels_Type use
     (Noop              => 0,
      Reset_HK_Counters => 10#01#,
      Enable            => 10#02#,
      Disable           => 3,
      Enable_Analog     => 4,
      Disable_Analog    => 5);
   for Command_Labels_Type'Size use 7;

begin

   null;
end Foruse;
