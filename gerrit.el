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
   (gerrit-lib-columnize "%-15s %s"
              (list "Change-Id" (assoc-default 'id review))
              (list "Owner" (assoc-default 'name (assoc-default 'owner review)))
              (list "Project" (assoc-default 'project review))
              (list "Branch" (assoc-default 'branch review))
              (list "Topic" (assoc-default 'topic review))
              (list "Created" (gerrit-lib-format-time
                               (assoc-default 'createdOn review)))
              (list "Updated" (gerrit-lib-format-time
                               (assoc-default 'lastUpdated review)))
              (list "Status" (assoc-default 'status review)))
   "\n\n"
   (assoc-default 'commitMessage review)
   "\n\n"
   (apply 'gerrit-lib-columnize "%-20s %11s  %8s  %8s"
          '("Name" "Code-Review" "Verified" "Workflow")
          (mapcar
           (lambda (approval)
             (list (assoc-default 'name (assoc-default 'by approval))
                   (if (equal "Code-Review" (assoc-default 'type approval))
                       (gerrit-lib-positivize (assoc-default 'value approval))
                     "")
                   (if (equal "Verified" (assoc-default 'type approval))
                       (gerrit-lib-positivize (assoc-default 'value approval))
                     "")
                   (if (equal "Workflow" (assoc-default 'type approval))
                       (gerrit-lib-positivize (assoc-default 'value approval))
                     "")))
           (assoc-default 'approvals (assoc-default 'currentPatchSet review))))
   "\n\n"
   (apply 'gerrit-lib-columnize "Patch Set %2s - %s"
          (mapcar
           (lambda (patch-set)
             (list (assoc-default 'number patch-set)
                   (assoc-default 'revision patch-set)))
           (assoc-default 'patchSets review)))
   "\n"
   (apply 'gerrit-lib-columnize "%-70s  %4s, %4s"
          (mapcar
           (lambda (file)
             (list (if (equal "/COMMIT_MSG" (assoc-default 'file file))
                       "Commit Message"
                     (assoc-default 'file file)
                     )
                   (format "+%s" (assoc-default 'insertions file))
                   (assoc-default 'deletions file)))
           (assoc-default 'files (assoc-default 'currentPatchSet review))))
   "\n\n\n"
   (mapconcat 'gerrit-display-comment
              (assoc-default 'comments review)
              "\n\n")
   ))

(defun gerrit-display-comment (comment)
  "Format a comment given as an alist; return a string"
  (let ((split-message (s-split-up-to "\n" (assoc-default 'message comment) 1)))
    (let ((header (car split-message))
          (body (cadr split-message)))
      (concat (format "%s: %s (%s)"
                      (assoc-default 'name (assoc-default 'reviewer comment))
                      header
                      (gerrit-lib-format-time
                       (assoc-default 'timestamp comment)))
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

