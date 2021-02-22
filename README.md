# eww-plus.el
Some helper functions for EWW.

## Dependencies
This package depends on `ivy` and `eww`(Emacs Web Wowser, a built-in Emacs package).

## Install
Clone this repo and add the directory to `load-path`. If you use `use-package`, bellow is an example.

```elisp
(use-package eww-plus
  :demand t
  :load-path "/path/to/eww-plus.el"
  :when (file-exists-p "/path/to/eww-plus.el")
  :bind (:map eww-plus-mode-map
              ("C-c l e w w" . eww-plus-list-buffers)
              ("C-c l e w v" . eww-plus-list-visited-urls))
  :config (progn
            (setq eww-plus-session-file "~/.emacs.d/eww-session.el")
            (setq eww-plus-expire-time 30) ;; after 30 days from the last time you visited an url, its record will be deleted

            (eww-plus-mode t)
            )
  )


```

## Usage
These are the features this package provides.

### Save and restore the position when you killed a eww buffer.
So, you can read from where you left.

### List all of the urls that you have visited.
Use command `eww-plus-list-visited-urls`.

### List all eww buffers.
Use command `eww-plus-list-buffers`.
