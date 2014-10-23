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

(defgroup gerrit nil
  "Customize the Gerrit plugin."
  :prefix "gerrit-")

(defgroup gerrit-faces nil
  "Customize the appearance of Gerrit."
  :prefix "gerrit-"
  :group 'gerrit
  :group 'faces)

(defface gerrit-upvote
  '((t :foreground "green"))
  :group 'gerrit-faces)

(defface gerrit-downvote
  '((t :foreground "red"))
  :group 'gerrit-faces)
