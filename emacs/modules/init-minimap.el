(use-package minimap
  :defer t
  :config
  (setq minimap-update-delay 0.0))
(defun +minimap-set-highlight-line--on ()
  (setq +minimap-highlight-line t)
	(custom-set-faces!
	 '(minimap-current-line-face :background "#51AFEF" :group 'minimap)))

(scroll-bar-mode -1)

(defun +minimap-set-highlight-line--off ()
  (setq +minimap-highlight-line nil)
	(custom-set-faces
		'(minimap-current-line-face
   ((((background dark)) (:background "#7F7F7F"))
    (t (:background "#ABABAB")))
   :group 'minimap)))

(defcustom +minimap-highlight-line nil
  "Whether minimap should highlight the current line more prominent."
  :set (lambda (sym value)
         (set sym value)
         (if (null value) (+minimap-set-highlight-line--off) (+minimap-set-highlight-line--on)))
  :type 'boolean
  :group 'minimap)

(setq
 ;; Configure minimap position
 minimap-window-location 'right ; Minimap on the right side
 minimap-width-fraction 0.0 ; slightly smaller minimap
 minimap-minimum-width 20 ; also slightly smaller minimap

 minimap-dedicated-window t ; seems to work better
 minimap-enlarge-certain-faces nil ; enlarge breaks BlockFont
 )

;; Change colors of minimap
(custom-set-faces
 ;; Change background
 '(minimap-active-region-background
   ((((background dark)) (:background "#494949"))
    (t (:background "#D6D6D6")))
   :group 'minimap))

;; Set Blockfont as minimap font
(defface minimap-font-face
  '((default :family "BlockFont" :height 30))
  "Face used for text in minimap buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the minimap to avoid problems with recentering."
  :group 'minimap)


;; Enable minimap on startup
(minimap-mode 1)

(provide 'init-minimap)
