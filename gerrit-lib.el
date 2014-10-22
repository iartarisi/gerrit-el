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

;; Parts of this code originated in the `magit-gerrit` project by `terranpro`

(require 'json)

(defvar-local gerrit-credentials "mapleoin@review.openstack.org")

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
         (executable-find "ssh") nil nil nil
         (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
               "-x -p 29418 -q "
               (or gerrit-credentials
                   (error "`gerrit-credentials' must be set!"))
               " "
               "gerrit "
               cmd
               " "
               (mapconcat 'identity args " "))))
    gcmd))

(defun gerrit-query-project (prj &optional status)
  (gerrit-ssh-cmd "query"
                  "--format=JSON"
                  (concat "project:" prj)
                  (concat "status:" (or status "open"))))

(defun gerrit-query-everything (query)
  "Query gerrit and return all the information used to build a Change page.
`query` should be a Change id or hash"
  (gerrit-ssh-cmd "query"
                  "--format=JSON"
                  "--all-approvals"
                  "--current-patch-set"
                  "--files"
                  query))

(defun gerrit-ssh-cmd (cmd &rest args)
  (shell-command-to-string
   (concat (executable-find "ssh") " "
           (apply #'gerrit-command cmd args))))

(defun columnize (format-s &rest lines)
  "Arrange lines according to `format`. `lines` must be a list of
  lists matching the number of interpolated strings in
  `format`"
  ;; XXX there's got to be a version of mapconcat that doesn't require
  ;; currying with the 'apply function
  (mapconcat (apply-partially 'apply (apply-partially 'format format-s))
             lines
             "\n"))
