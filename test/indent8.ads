with Text_Io;
package Indent8 is
   package Dbregulation is
      procedure X;
      I : Integer;
   end Dbregulation;
   type Event_Class_T is
     (Sit_Timeout,
      Inform_Timeout,
      Regulation_Activation,
      Regulation_Deactivation,
      Regulation_Revision);
   type Event_Data_T (Classx : Event_Class_T := Sit_Timeout) is
      record
         case Classx is
            when Sit_Timeout | Inform_Timeout =>
               F_Key : Integer;
            when Regulation_Activation =>
               External_Reference : Integer;
            when Regulation_Deactivation | Regulation_Revision =>
               Reference : Integer;
         end case;
      end record;
end Indent8;
