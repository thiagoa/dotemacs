;;; packages.el --- Packages configuration
;;
;;; Commentary:

;;; Code:

(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("stable-melpa" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar pivot-package 'discover)

(setq package-selected-packages
      '(;; Emacs extensions
        dired-subtree
        ibuffer-vc
        persistent-scratch

        ;; Elisp
        f
        package-lint

        ;; Movement / Text editing
        avy
        beginend
        change-inner
        crux
        dot-mode
        goto-last-change
        iy-go-to-char
        move-text
        multiple-cursors
        undo-tree
        wrap-region
        embrace

        ;; Project / Search
        ag
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

        ;; PHP
        php-mode

        ;; Elixir
        alchemist
        elixir-mode

        ;; Ruby / Rails
        bundler
        enh-ruby-mode
        goto-gem
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
        zenburn-theme))

;;; packages.el ends here
