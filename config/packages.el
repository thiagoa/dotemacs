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
        swiper
        ivy-hydra
        counsel
        ido-vertical-mode
        hippie-exp
        yasnippet
        change-inner
        dot-mode
        iy-go-to-char
        smartscan
        multiple-cursors
        dracula-theme
        zenburn-theme
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
