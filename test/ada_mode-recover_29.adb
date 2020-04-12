-- Converting 'if then' to 'case'; parser used to fail.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Use_Minimal_Complete_Actions
  (Current_Token        : in     Token_ID;
   Next_Token           : in     Token_ID;
   Config               : in     Configuration;
   Use_Complete         :    out Boolean;
   Matching_Begin_Token :    out Token_ID)
is
   pragma Unreferenced (Next_Token);
   use all type SAL.Base_Peek_Type;
   use Java_Expressions_Ch19_Actions;
begin
   if Config.Stack.Depth = 1 and Current_Token = Descriptor.EOI_ID then
      --  Empty input buffer
      Use_Complete         := True;
      Matching_Begin_Token := +Identifier_ID;

   elsif Minimal_Complete_Action_IDs (Current_Token) then
      Use_Complete := True;

      case To_Token_Enum (Current_Token) is
              = +RIGHT_PAREN_ID then
               Matching_Begin_Token := +LEFT_PAREN_ID;
            else
               Matching_Begin_Token := Invalid_Token_ID;
            end if;
   else
      Use_Complete := False;
      Matching_Begin_Token := Invalid_Token_ID;
   end if;
end Use_Minimal_Complete_Actions;
--  Local Variables:
--  wisi-mckenzie-task-count: 1
--  End:
