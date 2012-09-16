--  This file tests the function ada-move-to-end.

--EMACSCMD: (progn (goto-line 21)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 62
--EMACSCMD: (progn (goto-line 39)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 40
--EMACSCMD: (progn (goto-line 40)(end-of-line)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 41
--EMACSCMD: (progn (goto-line 41)(end-of-line)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 42
--EMACSCMD: (progn (goto-line 42)(end-of-line)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 48
--EMACSCMD: (progn (goto-line 48)(end-of-line)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 55
--EMACSCMD: (progn (goto-line 56)(end-of-line)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 62
--EMACSCMD: (progn (goto-line 76)(ada-move-to-end)(count-lines(point-min)(point)))
--EMACSRESULT: 83

declare  --  NL 0   (nested level)
   A : Boolean;  --  Should go to 'end' for the declare from here
   procedure Whatif_For_Route (Route_Id : Route_Identifier.Key_T) is
   begin --  NL 1
      null;
   end Whatif_For_Route;   --  NL 0

   procedure Whatif_For_All is new Identifiers (Whatif_For_Route);

   procedure Do_Actions (Context : Context_T) is
   begin  --  NL 1
      declare
         Valid             : Boolean := True;
         function Delay_Value (For_Flight : Flight.T) return Time.Duration_T is
         begin  --  NL 2
            if Flight.Is_Suspended (For_Flight) then  --  NL 3
               return B;
            else
               if A then  --  NL 4
                  return A;  --  Start from here, and go up levels.
               end if;  --  NL 3
            end if;  --  NL 2
         end Delay_Value;  --  NL 1

      begin  --  NL 2
         if Operation = Try_Pair then --  NL 3
            null;
         end if; --  NL 2
      end;
      --  NL 1
      --  From here, ada-goto-matching-decl-start goes to the 'declare'
      --  which is wrong. This is because of Delay_Value.

      if B then  --  NL 2
         null;
      end if;  --  NL 1 --  Should go to next line from here
   end Do_Actions;  --  NL 0

begin  --  NL 1
   if A then --  NL 2
      null;
   end if;  --  NL 1
end;  --  NL 0


begin
   declare
      A : Integer;
   begin
      null;
   end;
   null;  --  from there, you would go to the line above
          --  but ada-goto-matching-decl-start goes to the 'begin'
end;


procedure A is --  ada-move-to-end should go to 'begin' for A
   procedure B is
   begin
      null;
   end B;
begin
   null;
end A;


Foo;
