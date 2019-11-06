# flymake-joker

Joker integration for flymake

# Configuration

``` emacs-lisp
(add-to-list 'load-path "~/path/to/flymake-joker")
(require "flymake-joker")

(add-hook 'clojure-mode-hook #'flymake-joker-clj-enable)
(add-hook 'clojurescript-mode-hook #'flymake-joker-cljs-enable)
(add-hook 'clojure-mode-hook #'flymake-mode)
```
