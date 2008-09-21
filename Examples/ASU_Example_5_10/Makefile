all : asu_example_5_10 asu_example_5_10_rd

asu_example_5_10 : asu_example_5_10-run.adb asu_example_5_10.ads
	gnatmake -I../../ asu_example_5_10-run

asu_example_5_10_rd : asu_example_5_10_rd-run.adb asu_example_5_10_rd.ads
	gnatmake -I../../ asu_example_5_10_rd-run

clean :
	$(RM) *.exe *.o *.ali asu_example_5_10-run asu_example_5_10_rd-run
