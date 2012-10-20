
--  Testing the cross-references to operators.
--  Try doing some cross-references on the definitions below and on the
--  constants defined in the body.
--  FIXME: automate the xref test

package Manual_Xref_Op is

   type A is new Integer;
   function "abs" (B : A) return A;
   function "not" (B : A) return A;
   function "mod" (B, C : A) return A;
   function "rem" (B, C : A) return A;
   function "=" (B, C : A) return Boolean;
   function "<" (B, C : A) return Boolean;
   function "<=" (B, C : A) return Boolean;
   function ">" (B, C : A) return Boolean;
   function ">=" (B, C  : A) return Boolean;
   function "and" (B, C : A) return A;
   function "or" (B, C : A) return A;
   function "xor" (B, C : A) return A;
   function "&" (B, C : A) return A;
   function "+" (B : A) return A;
   function "-" (B : A) return A;
   function "*" (B, C : A) return A;
   function "/" (B, C : A) return A;
   function "+" (B, C : A) return A;
   function "-" (B, C : A) return A;
   function "**" (B, C : A) return A;

end Manual_Xref_Op;
