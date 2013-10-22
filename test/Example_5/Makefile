# build and run hello_5 project

all : build run

.PHONY : force

build : force
	gprbuild -Phello_5.gpr hello_5

run :
	./hello_5

clean :
	gnatclean -r -Phello_5

export GPR_PROJECT_PATH = ../Example_4/Gnat_Project

# Local Variables:
# eval:(ada-parse-prj-file "hello_5.adp")
# eval:(ada-select-prj-file "hello_5.adp")
# End:
