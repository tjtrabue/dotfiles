;;; The very first configuration file, read even before `~/.emacs'.
;; Here we set values that must be defined before the first GUI frame is created.

;; * Fullscreen options
;; To change the initial fullscreen behavior of a frame using =initial-frame-alist=
;; or =default-frame-alist=, append one of the following options to one or both of
;; those lists:
;;
;; - `(fullscreen . fullwidth)': Make the frame as wide as possible, but do not
;; adjust vertical size.
;; - `(fullscreen . fullheight)': Make the frame as tall as possible, but do not
;; adjust horizontal size.
;; - `(fullscreen . maximized)': Set height and width to the size of the screen.
;; - `(fullscreen . fullboth)': Like 'maximized, but you cannot readjust the
;; frame size later with the mouse (removed the window manager options, such as
;; the close, minimize, and maximize buttons).
;;
;; ** How to adjust the initial frame's size
;; Use the `initial-frame-alist' to change the size of the first frame Emacs
;; creates on startup.
;;
;; ** How to adjust all frames' sizes
;; To change the size of all frames Emacs creates, set `default-frame-alist', instead.
;; Maximize Emacs' initial frame.
(add-to-list 'initial-frame-alist `(fullscreen . maximized))
