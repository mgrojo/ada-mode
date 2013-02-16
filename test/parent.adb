--EMACSCMD:(setq skip-recase-test t)
procedure Parent is
begin
   Append_To (Formals,
              Make_Parameter_Specification (Loc,
                                            Defining_Identifier =>
                                              Make_Defining_Identifier (Loc, Name_UTask_Id),
                                            In_Present => True,
                                            Parameter_Type =>
                                              New_Reference_To (RTE (RE_Task_Image_Type), Loc)));
end Parent;
