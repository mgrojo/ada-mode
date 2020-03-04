# This checks out all the dependencies for building ada-mode and
# running tests (after doing "git clone" to get this file). It assumes
# your upstream repository is named "origin" (which is the default).

git worktree add -b org.wisitoken              ../org.wisitoken              origin/org.wisitoken
git worktree add -b org.stephe_leake.sal       ../org.stephe_leake.sal       origin/org.stephe_leake.sal
git worktree add -b org.stephe_leake.makerules ../org.stephe_leake.makerules origin/org.stephe_leake.makerules
git worktree add -b org.stephe_leake.aunit_ext ../org.stephe_leake.aunit_ext origin/org.stephe_leake.aunit_ext

# end of file.
