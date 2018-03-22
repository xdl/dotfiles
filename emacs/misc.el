;; For eyebrowse
(defun eyebrowse-print-mode-line-indicator ()
  "Prints the current eyebrowse mode line indicator."
  (interactive)
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face 'eyebrowse-mode-line-delimiters))
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face 'eyebrowse-mode-line-delimiters))
         (separator (propertize eyebrowse-mode-line-separator
                                'face 'eyebrowse-mode-line-separator))
         (current-slot (eyebrowse--get 'current-slot))
         (window-configs (eyebrowse--get 'window-configs)))
    (message (concat
              left-delimiter
              (mapconcat
               (lambda (window-config)
                 (let* ((caption (eyebrowse-format-slot window-config))
                        (slot (car window-config))
                        (display-caption (if (= slot current-slot)
                                             (concat caption "*")
                                           caption)))
                   display-caption))
               window-configs separator)
              right-delimiter))))
