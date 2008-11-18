(in-package :cl-yalog)

(export '(make-thread-safe-file-log-appender thread-safe-file-log-appender) :cl-yalog)

(defclass thread-safe-file-log-appender (file-log-appender)
  ((lock :initform (bordeaux-threads:make-recursive-lock "thread-safe-file-log-appender")
         :accessor lock-of
         :initarg :lock)))

(defmethod append-message ((category log-category) (appender thread-safe-file-log-appender) message level)
  ;; TODO implement buffering and flushing to lower contention. needs a timer.
  (bordeaux-threads:with-recursive-lock-held ((lock-of appender))
    (with-open-file (log-file (merge-pathnames (log-file-of appender) *log-directory*)
                              :if-exists :append :if-does-not-exist :create :direction :output)
      (let ((*package* #.(find-package :cl-yalog)))
        (format log-file "(~S ~S ~A ~S ~S ~S)~%"
                (machine-instance)
                (sb-thread:thread-name sb-thread:*current-thread*)
                (local-time:now)
                (name-of category)
                level
                message)))))

(defun make-thread-safe-file-log-appender (file-name)
  (make-instance 'thread-safe-file-log-appender :log-file file-name))
