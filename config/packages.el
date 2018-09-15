(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar pivot-package 'discover)

(setq package-selected-packages
      '(discover
        fzf
        undo-tree
        exec-path-from-shell
        ibuffer-vc
        crux
        string-inflection
        aggressive-indent
        ace-jump-mode
        ag
        goto-last-change
        swiper
        ivy-hydra
        ivy-rich
        flx
        counsel
        ido-vertical-mode
        hippie-exp
        yasnippet
        flycheck
        change-inner
        move-text
        dot-mode
        iy-go-to-char
        smartscan
        multiple-cursors
        dracula-theme
        nimbus-theme
        zenburn-theme
        leuven-theme
        doom-themes
        color-theme-sanityinc-tomorrow
        projectile
        counsel-projectile
        magit
        magithub
        git-link
        company
        enh-ruby-mode
        bundler
        goto-gem
        projectile-rails
        ruby-end
        rubocop
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
        redshank))
