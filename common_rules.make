# gnu make rules common to all compilers

# some defaults for user options

DIFF_OPT := -u -w

# gnu make deletes intermediate files created by chained rules;
# label the ones we want to keep precious
.PRECIOUS : %.exe %.out

.PHONY : clean force
clean ::
	rm -f *.diff *.out

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print -delete
	-find $(SOURCE_ROOT) -name ".#*" -print -delete
	-find $(SOURCE_ROOT) -name "*,t" -print -delete
	rm -rf $(SOURCE_ROOT)/_MTN/resolutions

# Separate from clean, since that will often be added to.
test-clean ::
	rm -f *.out *.diff

# Don't include source-clean in release-clean, because
# release-clean is often done from several different build
# directories, and we don't want to repeat source-clean.
release-clean :: clean

maintainer-clean :: release-clean

%.out : %.exe ;	./$(*F).exe $(RUN_ARGS) > $(*F).out

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.hex :
	hexl $* > $@

%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

# end of file
