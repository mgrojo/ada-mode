--  Raised exception in recover. Fixed now

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

declare
   Min_Rhs := Min ();
   begin
        Possible_Left_Recursive and
          All_Sequences (Id)(Min_Rhs).Left_Recursive;

      All_Sequences (Nonterm)(Rhs).Sequence.Append (Min (All_Sequences (Id), Rhs_Set (Id)).Sequence);
   end if;
end;
end loop;
Rhs_Set (Nonterm)(Rhs) := True;
if Trace_Generate > Extra then
   Ada.Text_Io.Put_Line
     (Trimmed_Image (Production_Id'(Nonterm, Rhs)) & " => " &
        Image (All_Sequences (Nonterm)(Rhs), Descriptor));
