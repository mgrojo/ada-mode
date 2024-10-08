-- An Emacs Ada project file, for testing Emacs Ada project files

-- Declare an env var, to test referencing env vars in project variable values
$TEST_ENV_VAR=..

-- ada_project_path could be $ADA_PROJECT_PATH, but we are preserving backward-compatibilty with 4.01
ada_project_path=$TEST_ENV_VAR

gpr_file=ada_mode_parent.gpr

-- This file is also used with gnatxref, which does not use the gpr
-- file. So we need to set obj_dir to find the .ali files.
obj_dir=.
obj_dir=..

gnat_stub_opts=-t -f
gnat_stub_switches=-gnat2012

casing=../case-exceptions-1
casing=../case-exceptions-2

-- end of file
