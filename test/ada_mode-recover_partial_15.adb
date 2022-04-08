-- Raised exception in recover. Fixed now

declare
   Min_Rhs := Min (); -- copied assignment to create initialized declaration; recover inserts 'begin'
   begin
      --  Deleted Foo :=.
      Possible_Left_Recursive and
      All_Sequences (Id)(Min_Rhs).Left_Recursive;

      All_Sequences (Nonterm)(Rhs).Sequence.Append (Min (All_Sequences (Id), Rhs_Set (Id)).Sequence);
   end if; -- expecting 'end'. recover has various solutions, can change with small code changes.
   end;
end loop;
Rhs_Set (Nonterm)(Rhs) := True;
if Trace_Generate > Extra then
   Ada.Text_Io.Put_Line
     (Trimmed_Image (Production_Id'(Nonterm, Rhs)) & " => " &
        Image (All_Sequences (Nonterm)(Rhs), Descriptor));
