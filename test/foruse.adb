procedure Foruse is
   type Command_Labels_Type is
     (Noop,
      Reset_HK_Counters,
      Enable,
      Disable,
      Enable_Analog,
      Disable_Analog);

   for Command_Labels_Type use
     (Noop              => 0,
      Reset_HK_Counters => 1,
      Enable            => 2,
      Disable           => 3,
      Enable_Analog     => 4,
      Disable_Analog    => 5);
   for Command_Labels_Type'Size use 7;

begin

   null;
end Foruse;
