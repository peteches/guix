;;; Allow matugen post_hook and other tooling to eval into a running Nyxt.
(define-configuration browser
  ((remote-execution-p t)))

;;; Call quit when the last Nyxt window is closed via the WM (X button).
;;; Without this, ffi-kill-browser is never called and the SBCL process stays alive.
;;;
;;; Distinguishes WM-close from quit-command-close:
;;;   - quit command: ffi-window-delete :around removes window from hash BEFORE GTK destroy
;;;   - WM close: GTK destroys widget directly; window is still in the hash when this runs
(defmethod nyxt/renderer/gtk::on-signal-destroy :after ((window nyxt:window))
  (when (and nyxt:*browser*
             (slot-value nyxt:*browser* 'nyxt::ready-p)
             (gethash (nyxt::id window) (nyxt::windows nyxt:*browser*)))
    (if (= 1 (hash-table-count (nyxt::windows nyxt:*browser*)))
        (nyxt:quit)
        (remhash (nyxt::id window) (nyxt::windows nyxt:*browser*)))))
