;; magit-gh-issues

;; Sources
;; magit-gh-pulls
;; http://nullprogram.com/blog/2013/02/06/

;; todo: Guessing code from magit-gh-pulls

(require 'eieio)

(require 'magit)
(require 'gh)
(require 'gh-issues)
(require 'gh-issue-comments)

(defvar magit-gh-issues-collapse-issues t
  "Collapse issues by default in magit-statuslisting.")

(defvar magit-gh-issues-mode-lighter " Issues:")

(defun magit-gh-issues-get-api()
  (gh-issues-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues-purge-cache ()
  (let* ((api (magit-gh-issues-get-api))
         (cache (oref api :cache))
         ;; (repo (magit-gh-issues-guess-repo))
         (repo (list "Banno" "incidents")))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh-issues-cached-p (api user proj)
  "Returns T if the API request to the given USER and PROJ is cached."
  (let ((cache-repo (format "/repos/%s/%s/issues" user proj))
        (cached? nil))
    (pcache-map (oref api :cache)
                (lambda (key _) (when (equal (car key) cache-repo)
                                  (setq cached? t))))
    cached?))

(defun magit-gh-issues-reload()
  (interactive)
  (magit-gh-issues-purge-cache)
  (magit-refresh))

(magit-define-popup magit-gh-issues-popup
  "Show popup buffer featuring Github Issue commands."
  'magit-commands
  :actions  '((?r "Refresh" magit-gh-issues-reload)
              ;; (?c "Create new issue" magit-gh-issues-create-pull-request)
              ;; (?o "Open in browser" magit-gh-issues-open-in-browser)
              )
  :default-action 'magit-gh-issues-reload)

(defun magit-gh-issues-insert-gh-issues()
  (let* ((api (magit-gh-issues-get-api))
         (cached? (magit-gh-issues-cached-p api "Banno" "incidents"))
         (issues (oref
                  (gh-issues-issue-list api "Banno" "incidents")
                  :data)))
    (magit-insert-section
      (issues)
      (magit-insert-heading "Issues:")
      (mapc (lambda (issue)
              (let* ((number (number-to-string (oref issue :number)))
                     (body (oref issue :body))
                     (user (oref (oref issue :user) :login))
                     (title (oref issue :title)))
                (magit-insert-section
                  (issue number)
                  (insert (format "#%s (@%s) %s\n" number user title))
                  (magit-insert-heading)
                  (magit-insert-section
                    (issue number body)
                    'issue-number-body
                    (insert (concat body "\n\n"))
                    magit-gh-issues-collapse-issues))))
            issues)
      (when (not cached?)
        (insert "Press `% r` to update the issue list.\n\n"))
      (when (> (length issues) 0)
        (insert "\n"))
      )))

(defvar magit-gh-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "%") 'magit-gh-issues-popup)
    map))

;;;###autoload
(define-minor-mode magit-gh-issues-mode "Github issue support for Magit"
  :lighter  magit-gh-issues-mode-lighter
  :require 'magit-gh-issues
  :keymap  'magit-gh-issues-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-issues-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-issues-insert-gh-issues
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-issues-insert-gh-issues))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-issues()
  "Unconditionally turn on `magit-gh-issues-mode'."
  (interactive)
  (magit-gh-issues-mode 1))

(provide 'magit-gh-issues)
