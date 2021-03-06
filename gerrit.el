;; gerrit-el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; gerrit-el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gerrit-el.  If not, see <http://www.gnu.org/licenses/>.

(require 's)
(require 'json)


(defun gerrit-line-from-review (review-json)
  "Take a review given as a line of JSON from the gerrit API and
return a line of information about that change ready to be
printed."
  (let ((review (json-read-from-string review-json)))
    (format "%7s %-30s %10s %10s %10s"
            (assoc-default 'number review)
            (s-truncate 30 (assoc-default 'subject review))
            (assoc-default 'status review)
            (s-truncate 20 (assoc-default 'project review))
            (gerrit-lib-format-time (assoc-default 'lastUpdated review)))))

(defun gerrit-list-project (project)
  "Take a project name return a list of changes from the API"
  (mapconcat 'gerrit-line-from-review
             (butlast ;; the last item is just stats about the API query
              (split-string (gerrit-lib-query-project project)
                            "\n" t))
             "\n"))

(defun gerrit-detail-review (review)
  "Take a review given as an alist parsed from the gerrit API and
  open a new buffer with all the information in that review"
  (concat
   (gerrit-display-change-metadata review)
   "\n\n"
   (assoc-default 'commitMessage review)
   "\n\n"
   (gerrit-display-change-reviews review)
   "\n\n"
   (gerrit-display-change-patches review)
   "\n"
   (gerrit-display-change-files review)
   "\n\n\n"
   (mapconcat 'gerrit-display-comment
              (assoc-default 'comments review)
              "\n\n")))

(defun gerrit-display-change-metadata (review)
  "Takes a review alist and returns a string of the change's metadata e.g.

  Change-Id       If70d5ad285a349e163b3d07b6dfe0d2c64072dd8
  Owner           John Doe
  Project         openstack/nova
  Branch          master
  Topic           bp/virt-driver-cpu-pinning
  Created         12:46
  Updated         13:25
  Status          NEW"
  (gerrit-lib-columnize
   "%-15s %s"
   (list "Change-Id" (assoc-default 'id review))
   (list "Owner" (propertize (assoc-default 'name (assoc-default 'owner review))
                             'face 'gerrit-names))
   (list "Project" (assoc-default 'project review))
   (list "Branch" (assoc-default 'branch review))
   (list "Topic" (assoc-default 'topic review))
   (list "Created" (gerrit-lib-format-time
                    (assoc-default 'createdOn review)))
   (list "Updated" (gerrit-lib-format-time
                    (assoc-default 'lastUpdated review)))
   (list "Status" (assoc-default 'status review))))

(defun gerrit-display-change-reviews (review)
  "Takes a review alist and returns a table of the reviews/approvals. e.g.
  Name                 Code-Review  Verified  Workflow
  DB Datasets CI                          +1
  Citrix XenServer CI                     +1
  Microsoft Hyper-V CI                    +1
  Jane Devie                    +2
  Joe Dev                       +2
  Joe Dev                                           +1"
  (apply 'gerrit-lib-columnize "%-20s %11s  %8s  %8s"
          '("Name" "Code-Review" "Verified" "Workflow")
          (mapcar
           (lambda (approval)
             (list (propertize
                    (assoc-default 'name (assoc-default 'by approval))
                    'face 'gerrit-names)
                   (if (equal "Code-Review" (assoc-default 'type approval))
                       (gerrit-lib-format-vote (assoc-default 'value approval))
                     "")
                   (if (equal "Verified" (assoc-default 'type approval))
                       (gerrit-lib-format-vote (assoc-default 'value approval))
                     "")
                   (if (equal "Workflow" (assoc-default 'type approval))
                       (gerrit-lib-format-vote (assoc-default 'value approval))
                     "")))
           (assoc-default 'approvals (assoc-default 'currentPatchSet review)))))

(defun gerrit-display-change-patches (review)
  "Takes a review alist and returns the list of patches as a string. e.g.
  Patch Set  1 - 23a1b9ed539405a9b850ce219c02de5d302038af
  Patch Set  2 - 2f43910e1ec023e695b0087915c8c7fd5600b394"
  (apply 'gerrit-lib-columnize "Patch Set %2s - %s"
         (mapcar
          (lambda (patch-set)
            (list (assoc-default 'number patch-set)
                  (assoc-default 'revision patch-set)))
          (assoc-default 'patchSets review))))

(defun gerrit-display-change-files (review)
  "Takes a review alist and returns a summary as a string. e.g:
  Commit Message                                                  +27,    0
  .gitignore                                                       +1,   -1
  .rubocop.yml                                                     +1,    0
  Gemfile                                                          +1,    0
  Rakefile                                                        +33,    0
  TESTING.md                                                       +9,   -9"
  (apply 'gerrit-lib-columnize "%-69s  %4s, %4s"
         (mapcar
          (lambda (file)
            (list (if (equal "/COMMIT_MSG" (assoc-default 'file file))
                      "Commit Message"
                    (assoc-default 'file file))
                  (propertize (format "+%s" (assoc-default 'insertions file))
                              'face 'gerrit-diff-add)
                  (propertize (number-to-string (assoc-default 'deletions file))
                              'face 'gerrit-diff-del)))
          (assoc-default 'files (assoc-default 'currentPatchSet review)))))

(defun gerrit-display-comment (comment)
  "Format a comment given as an alist; return a string formatted like:

  Commenter Name: First Line of the Comment (relative time)

  Additional comment lines if any."
  (let ((split-message (s-split-up-to "\n" (assoc-default 'message comment) 1)))
    (let ((header (car split-message))
          (body (cadr split-message)))
      (concat (propertize (assoc-default 'name (assoc-default 'reviewer comment))
                          'face 'gerrit-names)
              ": "
              (propertize (format "%s (%s)"
                                  header
                                  (gerrit-lib-format-time
                                   (assoc-default 'timestamp comment)))
                          'face 'gerrit-background-info)
              (when body (concat "\n" body))))))

(defun gerrit-open-change (change-id)
  "Open a change in a new buffer and switch to it"
  (message (concat "gerrit-open-change " change-id))
  (gerrit-lib-with-make-buffer change-id
                               (gerrit-detail-review
                                (json-read-from-string
                                 (gerrit-lib-query-everything change-id)))
                               (setq buffer-read-only t)))

(defun gerrit-open-change-on-line ()
  "Take a line starting with a change number and open the change it references"
  (interactive)
  (let ((projline (thing-at-point 'line)))
    (gerrit-open-change (match-string 0 projline))))

(defun gerrit-open-project (project)
  "Open a project's changes in a new buffer and switch to it"
  (gerrit-lib-with-make-buffer project (gerrit-list-project project)
   (hl-line-mode)
   (use-local-map (let ((map (make-keymap)))
                    (define-key map (kbd "q") 'gerrit-lib-quit-window)
                    (define-key map (kbd "RET") 'gerrit-open-change-on-line)
                    map))))

;; Example code
;; (gerrit-open-change "125030")
;; (gerrit-open-project "openstack/nova")

;; (defvar-local gerrit-credentials "mapleoin@review.openstack.org")

