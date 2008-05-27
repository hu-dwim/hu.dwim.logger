(in-package :cl-yalog)

(export '(make-threaded-file-log-appender threaded-file-log-appender) :cl-yalog)

(defclass threaded-file-log-appender (file-log-appender)
  ((lock :initform (bordeaux-threads:make-recursive-lock "threaded-file-log-appender")
         :accessor lock-of
         :initarg :lock)))

(defmethod append-message ((category log-category) (appender threaded-file-log-appender) message level)
  ;; TODO implement buffering and flushing to lower contention. needs a timer.
  (bordeaux-threads:with-recursive-lock-held ((lock-of appender))
    (with-open-file (log-file (merge-pathnames (cl-yalog::log-file-of appender) *log-directory*)
                              :if-exists :append :if-does-not-exist :create :direction :output)
      (let ((*package* #.(find-package :cl-yalog)))
        (format log-file "(~S ~S ~S ~A ~S ~S)~%"
                (machine-instance)
                (sb-thread:thread-name sb-thread:*current-thread*)
                level
                (get-universal-time) ;;TODO ? (local-time:now)
                (cl-yalog::name-of category)
                message)))))

(defun make-threaded-file-log-appender (file-name)
  (make-instance 'threaded-file-log-appender :log-file file-name))
