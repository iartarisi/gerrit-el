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
          (cdr (assoc 'number review))
          (s-truncate 30 (cdr (assoc 'subject review)))
          (cdr (assoc 'status review))
          (s-truncate 20 (cdr (assoc 'project review)))
          ;; TODO make the timestamp shorter (use relative timestamps)
          (format-time (cdr (assoc 'lastUpdated review)))))

(defun detail-review (review)
  "Take a review given as an alist parsed from the gerrit API and
  open a new buffer with all the information in that review"
  (concat
   (columnize "%-15s %s"
              (list "Change-Id" (cdr (assoc 'id review)))
              (list "Owner" (cdr (assoc 'name (cdr (assoc 'owner review)))))
              (list "Project" (cdr (assoc 'project review)))
              (list "Branch" (cdr (assoc 'branch review)))
              (list "Topic" (cdr (assoc 'topic review)))
              (list "Created" (format-time (cdr (assoc 'createdOn review))))
              (list "Updated" (format-time (cdr (assoc 'lastUpdated review))))
              (list "Status" (cdr (assoc 'status review))))))
