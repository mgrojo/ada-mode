-- This code doesn't compile; it's cut down from a real GDS file, that
-- exposed a bug in syntax-ppss due to an incorrect setting of
-- font-lock-beginning-of-syntax-function. There must be enough text
-- inside a paren to cause syntax-ppss to try calling
-- syntax-begin-function.

--EMACSCMD:(setq skip-recase-test t)
package body GDS.Hardware.Bus_1553.RWA_Goddard is
   ----------
   --  Private subprograms

   procedure New_Packet
     (Module          : in     GDS.Modules.Hardware.Module_Access_Type;
      RT              : in     Mil_Std_1553.RT_Address_Type)
   is
      Root_Name : constant String := Read (Config_File, Packet_Iterator);
      Monitor   : constant Boolean :=
        Read (Config_File, Packet_Iterator, Default => False, Missing_Key => Ignore);
   begin
      if Monitor then
         Packet := new Packet_Type'
           (RT                            => RT,
            Monitor                       => True,
            RX_Enable                     =>
              (RX_Torque_Subaddress |
                 RX_Control_Subaddress |
                 RX_Sync_Subaddress       => True,
               others                     => False),
            RX_Queued                     => (others => False),
            Monitor_TX_Enable             => (TX_Speed_Subaddress => True, others => False),
            TX_Queued                     => (others => False),
            Symbols                       =>
              (Tor_Cmd_Volt               => Reals.New_Symbol (Module, Root_Name & "_Tor_Cmd_Volt", Output),
               Flag_Motor_Power_On        => Booleans.New_Symbol (Module, Root_Name & "_Flag_Motor_Power_On", Output),
               Motor_Torque               => null,
               Raw_Wheel_Hall_Period      => null,
               Raw_Wheel_Rev_Period       => null,
               Raw_Wheel_Hall_Delay       => null,
               Raw_Wheel_Rev_Delay        => null,
               Wheel_Speed                => Reals.New_Symbol
                 (Module, Root_Name & "_Rate_Tlm", Output,
                  Format                  => GDS.Symbols.Formats.Real_Format),
               Wheel_Momentum             => null,
               Time_Sync                  => Times.New_Symbol (Module, Root_Name & "_Time_Sync_Cmd", Output, TAI),
               Flag_Static_Sequence_Count => null,
               SPDATA_1                   => null,
               SPDATA_2                   => null,
               SPDATA_3                   => null,
               SPDATA_4                   => null,
               SPDATA_5                   => null,
               SPDATA_6                   => null,
               SPDATA_7_0_7               => null,
               SPDATA_7_8                 => null,
               SPDATA_7_9                 => null,
               SPDATA_7_10                => null,
               SPDATA_8                   => null,
               SPDATA_9                   => null),

            Data_Sync_Received => False,
            Time_Sync_Last     => SAL.Time_Conversions.Time_Type'First,
            Sequence_Count     => 0,
            Tor_Cmd_Volt       => 0.0,
            Motor_Power_On     => True);
      else
         Packet := new Packet_Type'
           (RT                       => RT,
            Monitor                  => False,
            RX_Enable                =>
              (RX_Torque_Subaddress |
                 RX_Control_Subaddress |
                 RX_Sync_Subaddress  => True,
               others                => False),
            RX_Queued                => (others => False),
            Monitor_TX_Enable        => (others => False),
            TX_Queued                => (others => False),
            Symbols                  =>
              (Tor_Cmd_Volt          => Reals.New_Symbol (Module, Root_Name & "_Tor_Cmd_Volt", Output),
               Flag_Motor_Power_On   => Booleans.New_Symbol (Module, Root_Name & "_Flag_Motor_Power_On", Output),
               Motor_Torque          => Reals.New_Symbol
                 (Module, Root_Name & "_Tor_Motor_Tlm_Volt", Input,
                  Format             => GDS.Symbols.Formats.Real_Format),
               Raw_Wheel_Hall_Period => Times.New_Symbol
                 (Module, Root_Name & "_Raw_Wheel_Hall_Period", Input, Relative),
               Raw_Wheel_Rev_Period  => Times.New_Symbol (Module, Root_Name & "_Raw_Wheel_Rev_Period", Input, Relative),
               Raw_Wheel_Hall_Delay  => Times.New_Symbol (Module, Root_Name & "_Raw_Wheel_Hall_Delay", Input, Relative),
               Raw_Wheel_Rev_Delay   => Times.New_Symbol (Module, Root_Name & "_Raw_Wheel_Rev_Delay", Input, Relative),
               Wheel_Speed           => Reals.New_Symbol
                 (Module, Root_Name & "_Rate_Tlm", Input,
                  Format             => GDS.Symbols.Formats.Real_Format),
               Wheel_Momentum        => Reals.New_Symbol
                 (Module, Root_Name & "_AngMom_Tlm", Input,
                  Format             => GDS.Symbols.Formats.Real_Format),
               Time_Sync             => Times.New_Symbol (Module, Root_Name & "_Time_Sync_Cmd", Output, TAI),

               Flag_Static_Sequence_Count => Booleans.New_Symbol
                 (Module, Root_Name & "_Flag_Error_Static_Sequence_Count", Input,
                  Rename                  => new String'(Root_Name & "_Flag_Err_Stat_Seq_Cnt")),

               SPDATA_1     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_1", Input),
               SPDATA_2     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_2", Input),
               SPDATA_3     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_3", Input),
               SPDATA_4     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_4", Input),
               SPDATA_5     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_5", Input),
               SPDATA_6     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_6", Input),
               SPDATA_7_0_7 => Unsigned_8s.New_Symbol (Module, Root_Name & "_Word_SPDATA_7_Pos_Code", Input),
               SPDATA_7_8   => Booleans.New_Symbol
                 (Module, Root_Name & "_Flag_SPDATA_7_Hall_Sensor_2_Status", Input,
                  Rename    => new String'(Root_Name & "_Flag_SPDATA_7_Hall_2")),
               SPDATA_7_9   => Booleans.New_Symbol
                 (Module, Root_Name & "_Flag_SPDATA_7_Hall_Sensor_1_Status", Input,
                  Rename    => new String'(Root_Name & "_Flag_SPDATA_7_Hall_1")),
               SPDATA_7_10  => Booleans.New_Symbol
                 (Module, Root_Name & "_Flag_SPDATA_7_Hall_Sensor_0_Status", Input,
                  Rename    => new String'(Root_Name & "_Flag_SPDATA_7_Hall_0")),
               SPDATA_8     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_8", Input),
               SPDATA_9     => Unsigned_16s.New_Symbol (Module, Root_Name & "_Word_SPDATA_9", Input)),

            Data_Sync_Received => False,
            Time_Sync_Last     => SAL.Time_Conversions.Time_Type'First,
            Sequence_Count     => 0,
            Tor_Cmd_Volt       => 0.0,
            Motor_Power_On     => True);
      end if;

      Set_RT_Configuration (Config_1553, RT, RX_Torque_Subaddress, RT_Transmit => False, Monitored => Monitor);
      Set_RT_Configuration (Config_1553, RT, RX_Control_Subaddress, RT_Transmit => False, Monitored => Monitor);
      Set_RT_Configuration (Config_1553, RT, TX_Speed_Subaddress, RT_Transmit => True, Monitored => Monitor);
      Set_RT_Configuration (Config_1553, RT, RX_Sync_Subaddress, RT_Transmit => False, Monitored => Monitor);
      Set_RT_Configuration (Config_1553, RT, TX_Speed_Data_Subaddress, RT_Transmit => True, Monitored => Monitor);

   end New_Packet;

end GDS.Hardware.Bus_1553.RWA_Goddard;
