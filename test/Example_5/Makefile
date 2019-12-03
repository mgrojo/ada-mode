# build and run hello_5 project

all : build run

.PHONY : force

build : force
	gprbuild -PExample_5.gpr hello_5

run :
	./hello_5

clean :
	gnatclean -r -PExample_5

# Local Variables:
# eval:(unless (wisi-prj-find-function-set-p)(add-hook 'project-find-functions 'wisi-prj-current-cached))
# eval:(wisi-prj-dtrt-parse-file "hello_5.prj" (ada-prj-default "hello_5 # main") "Makefile")
# End:
