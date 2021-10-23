# common rules for wisi

.PHONY : all force one test test-clean

%.diff : % %.tmp
	-diff -u $< $(*F).tmp > $(*F).diff

%.diff-run : % %.tmp
	-diff -u $< $(*F).tmp

../test_all_harness.exe : ../test_all_harness.adb force
	gprbuild -p -j8 ../wisi_test.gpr $(<F)

elisp-clean :
	rm -f ../autoloads.el
	rm -f ../*.elc

autoloads : force
	emacs -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"../autoloads.el\"))(update-directory-autoloads \"../\"))"

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# (grep-find "find .. -type f -print | xargs grep -n FIXME")

# for recompiling with release options
recursive-clean : force
	gprclean -r -P ../ada_mode_wisi_parse.gpr

clean :: doc-clean elisp-clean exe-clean source-clean test-clean profile-clean

doc-clean ::
	rm -f ../*.info ../*.html ../dir-ada-mode


exe-clean ::
	rm -rf ../obj

profile-clean ::
	rm -rf ../exec_pro ../obj_pro

test-clean ::
	rm -f *.diff *.tmp
	rm -f *.log *.output *.wisi-test *.stamp

source-clean :: test-clean
	-find ../ -name "*~" -print -delete
	-find ../ -name ".#*" -print -delete

# end of file
