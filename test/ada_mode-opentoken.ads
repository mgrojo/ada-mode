--  This file does not compile. Test adding special OpenToken indentation rule via Local Variables.

--EMACSCMD: ada-indent-opentoken
--EMACSRESULT: t
--EMACSCMD: (car smie-indent-functions)
--EMACSRESULT: 'ada-smie-opentoken
with GDS.Modules.Models.Integrator_Models.DOF_6;
with SAL.Config_Files;
private package GDS.Commands.Add_Statement is

   type Instance is new Nonterminal.Instance with null record;

   Grammar : constant Production_List.Instance;

   type Create_Module_Type is access procedure
     (Module_Type : in     String;
      Module_Name : in     String;
      Chassis     : in     Chassis_ID_Type;
      Main_Config : in     SAL.Config_Files.Configuration_Type;
      Parent      : in     GDS.Modules.Models.Integrator_Models.DOF_6.Module_Access_Type;
      Before      : in     GDS.Modules.Models.Module_Access_Type;
      Parameters  : in     String;
      Module      :    out GDS.Modules.Module_Access_Type);

   Create_Module_Procedure : Create_Module_Type;

private

   procedure Synthesize_Module
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type);

   --EMACSCMD:(progn (end-of-line 2)(forward-word -1)(downcase-word 1)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "Access")))
   Module_Action : constant Nonterminal.Synthesize := Synthesize_Module'Access;

   Add_Statement : constant Instance :=
     (Nonterminal.Instance (Nonterminal.Get (Add_Statement_ID)) with null record);

   Grammar : constant Production_List.Instance :=
     Tokens.Statement <= Add_Statement and
     Add_Statement <=
       --  add module "type" name;
       Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) & Tokens.String & Tokens.Identifier + Module_Action and

     --  Any module may have parameters
     Add_Statement <=
       --  add module "type" name "parameters";
       Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) &
       Tokens.String & Tokens.Identifier & Tokens.String + Module_Action and

     --  Hardware and Flight_SW modules may be remote, but not have 'parent' or 'before'
     Add_Statement <=
       --  add remote <chassis> module "type" name;
       Master_Token.Get (Add_ID) & Master_Token.Get (Remote_ID) & Tokens.Integer &
       Master_Token.Get (Module_ID) & Tokens.String &
       Tokens.Identifier + Module_Action and

     Add_Statement <=
       --  add remote <chassis> module "type" name "parameters";
       Master_Token.Get (Add_ID) & Master_Token.Get (Remote_ID) & Tokens.Integer & Master_Token.Get (Module_ID) &
       Tokens.String & Tokens.Identifier & Tokens.String + Module_Action and

     --  Model modules may have 'before', but may not be remote
     --  Integrator modules may have 'parent', but may not be remote
     Add_Statement <=
       --  add module "type" child_name parent parent_name;
       Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) & Tokens.String & Tokens.Identifier &
       Master_Token.Get (Parent_ID) & Tokens.Identifier + Module_Action and

     Add_Statement <=
       --  add module "type" child_name parent parent_name "parameters";
       Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) & Tokens.String & Tokens.Identifier &
       Master_Token.Get (Parent_ID) & Tokens.Identifier & Tokens.String + Module_Action and

     --  add module "type" child_name before before_name;
     Add_Statement <= Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) & Tokens.String &
       Tokens.Identifier & Master_Token.Get (Before_ID) & Tokens.Identifier +
       Module_Action and

     Add_Statement <=
       --  add module "type" child_name before before_name "parameters";
       Master_Token.Get (Add_ID) & Master_Token.Get (Module_ID) & Tokens.String & Tokens.Identifier &
       Master_Token.Get (Before_ID) & Tokens.Identifier & Tokens.String +
       Module_Action;

end GDS.Commands.Add_Statement;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
