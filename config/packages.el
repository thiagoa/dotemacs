(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar pivot-package 'discover)

(setq package-selected-packages
      '(discover
        undo-tree
        exec-path-from-shell
        ibuffer-vc
        crux
        string-inflection
        aggressive-indent
        ag
        swiper
        ivy-hydra
        counsel
        ido-vertical-mode
        hippie-exp
        yasnippet
        change-inner
        iy-go-to-char
        smartscan
        multiple-cursors
        dracula-theme
        zenburn-theme
        projectile
        counsel-projectile
        magit
        company
        enh-ruby-mode
        bundler
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
        json-mode
        coffee-mode
        markdown-mode))
