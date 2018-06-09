(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defconst package-selected-packages
  '(magit
    discover
    exec-path-from-shell
    json-mode
    hippie-exp
    ido-vertical-mode
    dracula-theme
    smartscan
    evil
    clj-refactor
    swiper
    counsel
    counsel-projectile
    crux
    coffee-mode
    ivy-hydra
    elixir-mode
    company
    alchemist
    ruby-end
    zenburn-theme
    ruby-electric
    yasnippet
    smartparens
    bundler
    change-inner
    ibuffer-vc
    iy-go-to-char
    parinfer
    string-inflection
    rubocop
    multiple-cursors
    seeing-is-believing
    rainbow-delimiters
    aggressive-indent)
  "My custom packages")

;; If this package is not present, then no package is present.
;; Trigger an automatic installation of all packages.
(unless (package-installed-p 'discover)
  (pac-install))
