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

(defvar-local res  (gerrit-query "stackforge/cookbook-openstack-common"))
(let ((review (json-read-from-string res)))
  (line-from-review review))


(defun format-time (seconds-since-epoch)
  "Format timestamp provided as seconds since epoch; returns a string"
  ;; TODO this should be replaced with a proper relative-time function
  ;; like the Web UI has
  (current-time-string (seconds-to-time seconds-since-epoch)))

(defun line-from-review (review)
  "Take a review given as an alist parsed from the gerrit API and
return a line of information about that change ready to be
printed."
  (format "%7s %30s %10s %10s %10s"
          (assoc-default 'number review)
          (s-truncate 30 (assoc-default 'subject review))
          (assoc-default 'status review)
          (s-truncate 20 (assoc-default 'project review))
          ;; TODO make the timestamp shorter (use relative timestamps)
          (format-time (assoc-default 'lastUpdated review))))

(defun detail-review (review)
  "Take a review given as an alist parsed from the gerrit API and
  open a new buffer with all the information in that review"
  (concat
   (columnize "%-15s %s"
              (list "Change-Id" (assoc-default 'id review))
              (list "Owner" (assoc-default 'name (assoc-default 'owner review)))
              (list "Project" (assoc-default 'project review))
              (list "Branch" (assoc-default 'branch review))
              (list "Topic" (assoc-default 'topic review))
              (list "Created" (format-time (assoc-default 'createdOn review)))
              (list "Updated" (format-time (assoc-default 'lastUpdated review)))
              (list "Status" (assoc-default 'status review)))
   "\n\n"
   (assoc-default 'commitMessage review)
   "\n\n"
   (apply 'columnize "%-20s %11s  %8s  %8s"
          '("Name" "Code-Review" "Verified" "Workflow")
          (mapcar
           (lambda (approval)
             (list (assoc-default 'name (assoc-default 'by approval))
                   (if (equal "Code-Review" (assoc-default 'type approval))
                       (assoc-default 'value approval)
                     "")
                   (if (equal "Verified" (assoc-default 'type approval))
                       (assoc-default 'value approval)
                     "")
                   (if (equal "Workflow" (assoc-default 'type approval))
                       (assoc-default 'value approval)
                     "")))
           (assoc-default 'approvals (assoc-default 'currentPatchSet review))))
   "\n\n"
   (apply 'columnize "Patch Set %2s - %s"
          (mapcar
           (lambda (patch-set)
             (list (assoc-default 'number patch-set)
                   (assoc-default 'revision patch-set)))
           (assoc-default 'patchSets review)))
   "\n"
   (apply 'columnize "%-70s  %4s, %4s"
          (mapcar
           (lambda (file)
             (list (if (equal "/COMMIT_MSG" (assoc-default 'file file))
                       "Commit Message"
                     (assoc-default 'file file)
                     )
                   (format "+%s" (assoc-default 'insertions file))
                   (assoc-default 'deletions file)))
           (assoc-default 'files (assoc-default 'currentPatchSet review))))
   ))

(defvar-local change (json-read-from-string (gerrit-query-everything "125030")))
(detail-review change)
