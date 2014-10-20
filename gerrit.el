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


(defun line-from-review (review)
  "Takes a review given as an alist parsed from the gerrit API
and returns line of information about that change ready to be
printed."
  (format "%7s %30s %10s %10s %10s"
          (cdr (assoc 'number review))
          (s-truncate 30 (cdr (assoc 'subject review)))
          (cdr (assoc 'status review))
          (s-truncate 20 (cdr (assoc 'project review)))
          ;; TODO make the timestamp shorter (use relative timestamps)
          (current-time-string (seconds-to-time (cdr (assoc 'lastUpdated review))))))
