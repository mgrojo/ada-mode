Rem Make all of the formats for the draft (AA)RM+Corr+Amend
Arm_Form RM HTML New-Only 2 >tt
Arm_Form AARM HTML Show-Changes 2 >>tt

Arm_Form RM Text New-Only 2 >ttt
Arm_Form AARM Text New-Only 2 >>ttt

Rem For Word 97/2000:
Arm_Form RM RTF New-Changes 2 >ttr
Move Output\RM.Rtf Output\RM-W97.rtf
Rem For final printed version:
Arm_Form RM RTF New-Only 2 >>ttr
Move Output\RM.Rtf Output\RM-Final.rtf
Rem For Word XP/2003 (which use ballons for deletes):
Arm_Form RM RTF Show-Changes 2 >>ttr
Rem For final printed version:
Arm_Form AARM RTF New-Only 2 >>ttr
Move Output\AA.Rtf Output\AA-Final.rtf
Arm_Form AARM RTF Show-Changes 2 >>ttr
