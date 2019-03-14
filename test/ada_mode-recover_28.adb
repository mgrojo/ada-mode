--  'Pause' is misplaced, and caused the line to be indented wrong.

type Minimal_Action (Verb : Minimal_Verbs := Minimal_Verbs'First) is record
   case Verb is
      when  => Pause


      when Shift =>
         ID    : Token_ID;
         State : State_Index;

      when Reduce =>
         Nonterm     : Token_ID;
         Token_Count : Ada.Containers.Count_Type;
   end case;
end record;
