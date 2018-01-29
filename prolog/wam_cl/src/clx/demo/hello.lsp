
(in-package 'xlib)

(defun hello-world (host &rest args &key (string "Hello World") (font "6x10"))
  ;; CLX demo, says STRING using FONT in its own window on HOST
  (let* ((display (open-display host))
	 (screen (display-default-screen display))
	 (black (screen-black-pixel screen))
	 (white (screen-white-pixel screen))
	 (red 31)
	 (font (open-font display font))
	 (border 1)			; Minimum margin around the text
	 (width (+ (text-width font string) (* 2 border)))
	 (height (+ (max-char-ascent font) (max-char-descent font)
		    (* 2 border)))
	 (x (truncate (- (screen-width screen) width) 2))
	 (y (truncate (- (screen-height screen) height) 2))
	 (window (create-window :parent (screen-root screen)
				:x x :y y :width width :height height
				:background black
				:border white
				:border-width 1
				:colormap (screen-default-colormap screen)
				:bit-gravity :center
				:event-mask (make-event-mask
					     :exposure :key-press)))
	 (keymap (keyboard-mapping display))
	 (gcontext (create-gcontext :drawable window
				    :background black
				    :foreground white
				    :font font)))
    (setf (wm-name window) string
	  (wm-icon-name window) string
	  (wm-command window) (list* 'hello-world host args)
	  (wm-normal-hints window) (make-wm-size-hints :x x :y y :width :width)
	  (wm-hints window) (make-wm-hints :input :off :initial-state :normal))

    (map-window window)
    ;; Handle events
    (unwind-protect
	 (loop
	  (event-case
	   (display :force-output-p t)
	   (exposure;; Come here on exposure events,
	    (window count);; binding WINDOW and COUNT from the event.
	    (when (zerop count);; Ignore all but the last exposure event.
	      (with-state (window)
		(let ((x (truncate (- (drawable-width window) width) 2))
		      (y (truncate (- (+ (drawable-height window)
					 (max-char-ascent font))
				      (max-char-descent font))
				   2)))
		  ;; Draw text centered in widnow
		  (clear-area window)
		  (draw-glyphs window gcontext x y string)))
	      ;; Returning non-nil causes event-case to exit
	      t))
	   (key-press
	    (window state code)
	    (let ((char (int-char (aref keymap code 0))))
	      (terpri)
;		(princ "  char: ") (princ char)
	      (when char
		(return-from hello-world))
	      t))))
      ;; Ensure display is closed when done
      (close-display display))))

#|
To run this program do:
   
     (hello-world "hostname")

To exit press the q key
|#
