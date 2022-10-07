# This checks out all the dependencies and related modes for building
# ada-mode and running tests (after doing "git clone" to get this
# file). It assumes your upstream repository is named "origin" (which
# is the default).

git worktree add -b org.emacs.wisi             ../org.emacs.wisi             origin/org.emacs.wisi
git worktree add -b org.emacs.gpr-mode         ../org.emacs.gpr-mode         origin/org.emacs.gpr-mode
git worktree add -b org.emacs.gnat_compiler    ../org.emacs.gnat_compiler    origin/org.emacs.gnat_compiler
git worktree add -b org.emacs.gpr-query        ../org.emacs.gpr-query        origin/org.emacs.gpr-query
git worktree add -b org.wisitoken              ../org.wisitoken              origin/org.wisitoken
git worktree add -b org.stephe_leake.sal       ../org.stephe_leake.sal       origin/org.stephe_leake.sal
git worktree add -b org.stephe_leake.makerules ../org.stephe_leake.makerules origin/org.stephe_leake.makerules
git worktree add -b org.stephe_leake.aunit_ext ../org.stephe_leake.aunit_ext origin/org.stephe_leake.aunit_ext

# end of file.
