--  'Pause' is misplaced, and caused the line to be indented wrong.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

type Minimal_Action (Verb : Minimal_Verbs := Minimal_Verbs'First) is record
   case Verb is
      when  => Pause


      when Shift =>
         Id    : Token_Id;
         State : State_Index;

      when Reduce =>
         Nonterm     : Token_Id;
         Token_Count : Ada.Containers.Count_Type;
   end case;
end record;
