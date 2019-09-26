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
      '(discover
        undo-tree
        ibuffer-vc
        f
        crux
        string-inflection
        aggressive-indent
        ag
        wgrep
        avy
        beginend
        nord-theme
        poet-theme
        rbtagger
        package-lint
        dired-subtree
        enh-ruby-mode
        github-browse-file
        goto-last-change
        swiper
        helm
        helm-projectile
        helm-rg
        ido-vertical-mode
        hippie-exp
        yasnippet
        flycheck
        nlinum
        change-inner
        move-text
        dot-mode
        keyfreq
        iy-go-to-char
        smartscan
        multiple-cursors
        dracula-theme
        zenburn-theme
        doom-themes
        projectile
        magit
        magithub
        git-link
        company
        god-mode
        bundler
        goto-gem
        projectile-rails
        sql-indent
        rubocop
        slime
        rspec-mode
        seeing-is-believing
        elixir-mode
        alchemist
        cider
        clj-refactor
        rainbow-delimiters
        web-mode
        json-mode
        coffee-mode
        markdown-mode
        redshank
        racket-mode
        geiser))

(provide 'packages)
;;; packages.el ends here
