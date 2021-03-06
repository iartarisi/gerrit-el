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
(require 'memoize)
(require 'times)


(defun gerrit-lib-ssh-cmd (cmd &rest args)
  (apply #'call-process
         (executable-find "ssh") nil nil nil
         (split-string (apply #'gerrit-lib-command cmd args))))

(defun gerrit-lib-command (cmd &rest args)
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

(defmemoize gerrit-lib-query-project (prj &optional status)
  (gerrit-lib-ssh-cmd "query"
                  "--format=JSON"
                  (concat "project:" prj)
                  (concat "status:" (or status "open"))))

(defmemoize gerrit-lib-query-everything (query)
  "Query gerrit and return all the information used to build a Change page.
`query` should be a Change id or hash"
  (gerrit-lib-ssh-cmd "query"
                  "--format=JSON"
                  "--all-approvals"
                  "--current-patch-set"
                  "--comments"
                  "--files"
                  query))

(defun gerrit-lib-ssh-cmd (cmd &rest args)
  (shell-command-to-string
   (concat (executable-find "ssh") " "
           (apply #'gerrit-lib-command cmd args))))

(defun gerrit-lib-columnize (format-s &rest lines)
  "Arrange lines according to `format`. `lines` must be a list of
  lists matching the number of interpolated strings in
  `format`"
  ;; XXX there's got to be a version of mapconcat that doesn't require
  ;; currying with the 'apply function
  (mapconcat (apply-partially 'apply (apply-partially 'format format-s))
             lines
             "\n"))

(defun gerrit-lib-format-time (seconds-since-epoch)
  "Format timestamp provided as seconds since epoch; returns a string"
  (times-relative (seconds-to-time seconds-since-epoch)))

(defmacro gerrit-lib-with-make-buffer (name content &rest commands)
  "Create or replace buffer with `name` using `content`. Then
  execute any `commands` before making it read-only and bringing
  it to the forefront."
  `(let ((buf (get-buffer-create (format "*gerrit: %s*" ,name))))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert ,content)
       ,@commands
       (setq buffer-read-only t)
       (goto-char 0))
     (switch-to-buffer buf)))

(defun gerrit-lib-format-vote (vote)
  "Add a + sign where there is no -. Useful for +1/+2 reviews"
  (if (string-match (rx-to-string `(: bos "-") t)
                    vote)
      (propertize vote 'face 'gerrit-downvote)
    (propertize (concat "+" vote) 'face 'gerrit-upvote)))

(defun gerrit-lib-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window.  With a prefix argument, kill the
buffer instead."
  (interactive "p")
  (quit-window kill-buffer (selected-window)))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

;; This was copied from the development branch of the s.el project until
;; they release a new version
(defun s-split-up-to (separator s n &optional omit-nulls)
  "Split S up to N times into substrings bounded by matches for regexp SEPARATOR.

If OMIT-NULLS is non-nil, zero-length substrings are omitted.

See also `s-split'."
  (save-match-data
    (let ((op 0)
          (r nil))
      (with-temp-buffer
        (insert s)
        (setq op (goto-char (point-min)))
        (while (and (re-search-forward separator nil t)
                    (< 0 n))
          (let ((sub (buffer-substring-no-properties op (match-beginning 0))))
            (unless (and omit-nulls
                         (equal sub ""))
              (push sub r)))
          (setq op (goto-char (match-end 0)))
          (setq n (1- n)))
        (if (/= (point) (point-max))
            (push (buffer-substring-no-properties op (point-max)) r)
          (unless omit-nulls
            (push "" r))))
      (nreverse r))))
