;; ada-project.el - project.el backend for ada-mode projects -*- lexical-binding: t =*-

;; The project backend object is 'ada-project. The actual current
;; project is stored in `ada-prj-current-project'.

(require 'ada-mode)
(require 'project-patches)
(require 'project-menu)

;; methods from master project.el; find out if we need these anywhere
;; (cl-defmethod project-library-roots ((prj (eql ada-project)))
;;   ;; IMPROVEME: we could cache this in an ada-project object, but I
;;   ;; don't anticipate using this much.
;;   (let ((recur-ign (project-flat-to-recursive-ignores (ada-prj-get 'src_dir))))
;;     (car recur-ign)))

;; (cl-defmethod project-roots ((prj (eql ada-project)))
;;   ;; Just use project-library-roots.
;;   nil)

;; (cl-defmethod project-ignores ((prj (eql ada-project)) root)
;;   ;; IMPROVEME: we could cache this in an ada-project object, but I
;;   ;; don't anticipate using this much.
;;   (let ((recur-ign (project-flat-to-recursive-ignores (ada-prj-get 'src_dir)))
;; 	result)
;;     (dolist (dir (cdr recur-ign))
;;       (when (string-prefix-p root dir t)
;; 	(push (concat "./" (file-name-as-directory (file-relative-name dir root))) result)))
;;     result))

;; methods from project-patches.el
;; (cl-defmethod project-ignore-dir ((prj (eql 'ada-project))) should not be needed.

(cl-defmethod project-find-file ((prj (eql ada-project)) filename)
  (let* ((iter (make-path-iterator
		:user-path-non-recursive (ada-prj-get 'src_dir)
		:user-path-recursive nil
		:ignore-function nil))
	 (absfilename (locate-uniquified-file-iter iter nil filename)))

    (if (file-readable-p absfilename)
	(find-file absfilename)
      ;; FIXME: need human-readable name for project
      (error "'%s' not found/readable in project." filename)))
  )

(push
 (make-project-menu-item
  :name "Ada mode"
  :prj 'ada-project)
 project-menu-list)

(provide 'ada-project)

;; end of file
