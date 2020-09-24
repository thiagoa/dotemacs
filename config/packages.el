;;; packages.el --- Packages configuration
;;
;;; Commentary:

;;; Code:

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("stable-melpa" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("stable-melpa" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

(unless (>= emacs-major-version 27)
  (package-initialize))

(defvar pivot-package 'magit)

(setq package-selected-packages
      '(;; Emacs extensions
        ace-window
        balanced-windows
        dired-subtree			;
        ibuffer-vc
        persistent-scratch
        which-key
        perspective
        lsp-mode
        use-package

        ;; Elisp
        f
        package-lint

        ;; Movement / Text editing
        avy
        beginend
        change-inner
        crux
        dot-mode
        iy-go-to-char
        move-text
        multiple-cursors
        undo-tree
        wrap-region
        embrace

        ;; Project / Search
        ag
        wgrep-ag
        fzf
        helm
        helm-projectile
        projectile
        smartscan
        swiper
        wgrep

        ;; Git
        git-link
        github-browse-file
        magit
        magithub
        dired-git-info

        ;; Programming utils
        company
        flycheck
        hippie-exp
        rainbow-delimiters
        string-inflection
        yasnippet

        ;; Misc modes / Lang modes
        aggressive-indent
        coffee-mode
        graphql-mode
        polymode
        god-mode
        json-mode
        markdown-mode
        nlinum
        sql-indent
        web-mode
        adoc-mode

        ;; Org mode
        org-journal
        org-download

        ;; Clojure / Common Lisp / Racket / Scheme
        cider
        clj-refactor
        geiser ;; Scheme
        racket-mode
        redshank ;; CL
        slime ;; CL

        ;; JavaScript / JS
        rjsx-mode

        ;; PHP
        php-mode

        ;; Rust
        rust-mode
        flycheck-rust
        cargo
        racer

        ;; Elixir
        alchemist
        elixir-mode

        ;; Ruby / Rails
        bundler
        enh-ruby-mode
        projectile-rails
        rbtagger
        rspec-mode
        rubocop
        seeing-is-believing

        ;; Themes
        doom-themes
        kaolin-themes
        dracula-theme
        nord-theme
        poet-theme
        solarized
        zenburn-theme))

;;; packages.el ends here
