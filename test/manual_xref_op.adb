package body Manual_Xref_Op is

   function "abs" (B : A) return A is
   begin
      return B;
   end "abs";

   function "not" (B : A) return A is
   begin
      return B;
   end "not";

   function "mod" (B, C : A) return A is
   begin
      return B;
   end "mod";

   function "rem" (B, C : A) return A is
   begin
      return B;
   end "rem";

   function "=" (B, C : A) return Boolean is
   begin
      return True;
   end "=";

   function "<" (B, C : A) return Boolean is
   begin
      return True;
   end "<";

   function "<=" (B, C : A) return Boolean is
   begin
      return True;
   end "<=";

   function ">" (B, C : A) return Boolean is
   begin
      return True;
   end ">";

   function ">=" (B, C : A) return Boolean is
   begin
      return True;
   end ">=";

   function "and" (B, C : A) return A is
   begin
      return B;
   end "and";

   function "or" (B, C : A) return A is
   begin
      return B;
   end "or";

   function "xor" (B, C : A) return A is
   begin
      return B;
   end "xor";

   function "&" (B, C : A) return A is
   begin
      return B;
   end "&";

   function "+" (B : A) return A is
   begin
      return B;
   end "+";

   function "-" (B : A) return A is
   begin
      return B;
   end "-";

   function "*" (B, C : A) return A is
   begin
      return B;
   end "*";

   function "/" (B, C : A) return A is
   begin
      return B;
   end "/";

   function "+" (B, C : A) return A is
   begin
      return B;
   end "+";

   function "-" (B, C : A) return A is
   begin
      return B;
   end "-";

   function "**" (B, C : A) return A is
   begin
      return B;
   end "**";

   B : constant A := 5;
   C : constant A := 5;

   A1 : A := abs (B);
   A2 : A := not (B);
   A3 : A := B mod C;
   A4 : A := B rem C;
   A5 : Boolean := (B = C);
   A6 : Boolean := (B < C);
   A7 : Boolean := (B <= C);
   A8 : Boolean := (B > C);
   A9 : Boolean := (B >= C);
   AA : A := (B and C);
   AB : A := (B or C);
   AC : A := (B xor C);
   AD : A := (B & C);
   AE : A := +B;
   AF : A := -B;
   AG : A := (B * C);
   AH : A := (B / C);
   AI : A := (B + C);
   AJ : A := (B - C);
   AK : A := (B ** C);

end Manual_Xref_Op;
