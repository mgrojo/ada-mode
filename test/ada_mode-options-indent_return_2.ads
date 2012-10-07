--  Similar to ada_mode-options-indent_return_1.ads, except with a
--  different value of ada-indent-return and ada-indent-renames.

--EMACSCMD: (setq ada-indent-return 5)
--  > 0 indents relative to the "function" keyword
--EMACSCMD: (setq ada-indent-renames -2)
--  < 0 indents relative to the open parenthesis
with Ada.Finalization;
package Function2 is

   function A return Integer;
   function B
        return Integer;

   function C (B : Integer) return Integer;
   function D (B : Integer)
        return Integer;

   function E (B : Integer;
               C : Integer) return Integer;
   function F (B : Integer;
               C : Integer)
        return Integer;

   --  In the following example, we test with either one or
   --  multiple arguments

   type Callback_1_Type is access
     function (Widget : access Ada.Finalization.Limited_Controlled'Class)
          return Boolean;

   type Callback_2_Type is access
     function (Widget : access Ada.Finalization.Limited_Controlled'Class;
               User   : in     Integer)
          return Boolean;


   --  Test for renames statements

   function AR return Integer renames A;
   function BR
        return Integer
     renames B;   --  from ada-broken-indent, relative to 'function'

   function CR (B : Integer) return Integer renames C;
   function DR (B : Integer)
        return Integer
                 renames D;  --  from ada-indent-renames, relative to open paren

   function ER (B : Integer;
                C : Integer) return Integer renames E;
   function FR (B : Integer;
                C : Integer)
        return Integer
                 renames F;  --  from ada-indent-renames

end Function2;
