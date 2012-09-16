-- -*- ada-when-indent: 3 -*-

-- Indentation test only

package body PRIME.Volatilities is

   function Get_Volatility_Shift is new Get_Object (Root_Volatility_Shift, Root_Volatility_Shift_Ptr);
   pragma Inline (Get_Volatility_Shift);

   function Get_Volatility is new Get_Object (Root_Volatility, Root_Volatility_Ptr);
   pragma Inline (Get_Volatility);


   function Make_Implicit_BS_Volatility_Shift (Shift : in Shift_BS_Record)
                                              return Volatility_Shift
   is
   begin
      return Make_Ref (new Root_Volatility_Shift'(Root_Object with
                                                    Shift_Type => Shift_BS,
                                                  N => 0,
                                                  S => Shift));
   end Make_Implicit_BS_Volatility_Shift;

   function Apply (S : in Volatility_Shift; V : in Volatility) return Volatility is
      Shift_Object : constant Root_Volatility_Shift_Ptr := Get_Volatility_Shift (Object_Ref (S));
      Volatility_Object : constant Root_Volatility_Ptr := Get_Volatility (Object_Ref (V));
   begin
      case Shift_Object.Shift_Type is
         when Shift_Parameter =>
            return Apply (Shift_Object.all, Volatility_Object.all);
         when Shift_BS =>
            return Make_Ref (new BS_Shifted_Volatility'(Root_Object with
                                                          V => V,
                                                        S => Shift_Object.S));
      end case;
   end Apply;

   function Apply (S : in Root_Volatility_Shift'Class; V : in Root_Volatility) return Volatility is
   begin
      return Make_BS_Volatility (0.0);
   end Apply;

   function Make_BS_Volatility (V    : in Num) return Volatility is
   begin
      return Make_Ref (new BS_Volatility'(Root_Object with
                                            V    => V));
   end Make_BS_Volatility;


end PRIME.Volatilities;
