;; Override to provide fewer details
(defun helm-buffer--show-details (buf-name prefix help-echo
                                           size mode dir face1 face2
                                           proc details type)
  (append
   (list
    (concat prefix
            (propertize buf-name 'face face1
                        'help-echo help-echo
                        'type type)))
   (and details
        (list
         (propertize
          (if proc
              (format "(%s %s in `%s')"
                      (process-name proc)
                      (process-status proc) dir)
            (format "%s" (concat  (projectile-project-name)
                                  " / "
                                  (file-name-base (string-trim dir nil "/")))))
          'face face2)))))
