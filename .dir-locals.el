((nil
  (eval .
        (setq my-proj-dir
              (file-name-directory
               (let ((d (dir-locals-find-file ".")))
                 (if (stringp d) d (car d))))
        lsp-haskell-server-path (concat my-proj-dir "/bin/haskell-language-server"))))

 (haskell-mode
  (haskell-process-type . ghci)
  (haskell-process-args-ghci . ("-ferror-spans" "-fdiagnostics-color=never"))
  (eval .
        (setq my-proj-dir
              (file-name-directory
               (let ((d (dir-locals-find-file ".")))
                 (if (stringp d) d (car d))))
              haskell-process-path-ghci (concat my-proj-dir "bin/ghci")
              haskell-hoogle-server-command
              (lambda (port)
                (list (concat my-proj-dir "bin/hoogle") "server"
                      "--local" "-p" (number-to-string port)))))))
