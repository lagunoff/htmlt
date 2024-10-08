((nil
  (eval .
        (setq my-proj-dir
              (file-name-directory
               (let ((d (dir-locals-find-file ".")))
                 (if (stringp d) d (car d))))
        lsp-haskell-server-path (concat my-proj-dir "/bin/haskell-language-server"))))

 (haskell-mode
  (eval .
        (setq my-proj-dir
              (file-name-directory
               (let ((d (dir-locals-find-file ".")))
                 (if (stringp d) d (car d))))
              haskell-hoogle-server-command
              (lambda (port)
                (list (concat my-proj-dir "bin/hoogle") "server"
                      "--local" "-p" (number-to-string port)))))))
