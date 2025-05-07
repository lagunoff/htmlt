((haskell-mode
  (haskell-process-type . ghci)
  (haskell-process-args-ghci . ("-ferror-spans" "-fdiagnostics-color=never"))
  (eval .
        (let ((cwd (locate-dominating-file default-directory ".dir-locals.el")))
          (setq
           haskell-process-path-ghci (concat cwd "bin/ghci")
           haskell-hoogle-server-command
           (lambda (port)
             (list (concat cwd "bin/hoogle") "server"
                   "--local" "-p" (number-to-string port)))
           lsp-haskell-server-path (concat cwd "/bin/haskell-language-server"))
          ))))
