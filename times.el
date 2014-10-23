;; times is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; times is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with times.  If not, see <http://www.gnu.org/licenses/>.


(defun times-today ()
  "Return the time of today at 0.00am"
  (apply 'encode-time (append '(0 0 0) (cdddr (decode-time (current-time))))))

(defun times-relative (time)
  "Return a string of the time relative to today. If `time` is
  today, then only the hour and minute is returned. If it is
  before today, then the day and three-letter month is
  returned (e.g. 10:23; 13:37; 3. Oct; 24. Jan)"
  (if (> 0 (car (time-subtract (times-today) time)))
      (format-time-string "%R" time)
    (format-time-string "%d. %b" time)))
