(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; If this package is not present, then no package is present.
;; If so, trigger an automatic installation of all packages.
(defvar pivot-package 'discover)

(defconst package-selected-packages
  '(discover
    exec-path-from-shell
    ibuffer-vc
    crux
    string-inflection

    ;; Useful all-around modes
    aggressive-indent

    ;; Find/auto-complete stuff
    ag
    swiper
    ivy-hydra
    counsel
    ido-vertical-mode

    ;; Text editing helpers
    hippie-exp
    yasnippet
    change-inner
    iy-go-to-char
    smartscan
    multiple-cursors

    ;; Themes
    dracula-theme
    zenburn-theme

    ;; Project management
    projectile
    counsel-projectile

    ;; General programming
    magit
    company

    ;; Ruby
    enh-ruby-mode
    bundler
    ruby-end
    rubocop
    seeing-is-believing

    ;; Elixir
    elixir-mode
    alchemist

    ;; Clojure
    cider
    clj-refactor

    ;; Lisps
    rainbow-delimiters

    ;; Other modes
    json-mode
    coffee-mode)
  "My custom packages")
