;;; ralee-menu 	

; Copyright (c) 2004-2011 Sam Griffiths-Jones
; Author: Sam Griffiths-Jones <sam.griffiths-jones@manchester.ac.uk>
;
; This is part of RALEE -- see
; http://personalpages.manchester.ac.uk/staff/sam.griffiths-jones/software/ralee/
; and the 00README file that should accompany this file.
;
; RALEE is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; RALEE is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with RALEE; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


; GNU emacs and xemacs have different menu setups
(if (featurep 'xemacs)
    (progn
      ;; rewrite xemacs menu from scratch
      (defconst ralee-menubar
	;; note backquote
	`(
	  ("%_File"
	   ["%_Open..." find-file]
	   ["Open in Other %_Window..." find-file-other-window]
	   ["Open in New %_Frame..." find-file-other-frame]
	   ["%_Insert File..." insert-file]
	   ["%_View File..." view-file]
	   "------"
	   ["%_Save" save-buffer
	    :active (buffer-modified-p)
	    :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
	   ["Save %_As..." write-file]
	   ["Save So%_me Buffers" save-some-buffers]
	   "-----"
	   ["Postscript page setup" page-setup]
	   ["Write postcript to file" write-ps]
	   "-----"
	   ,@(if (eq system-type 'windows-nt)
		 '(["Page Set%_up..." generic-page-setup]))
	   ["%_Print" generic-print-buffer
	    :active (or (valid-specifier-tag-p 'msprinter)
			(and (not (eq system-type 'windows-nt))
			     (fboundp 'lpr-region)))
	    :suffix (if (region-active-p) "Selection..."
		      (if put-buffer-names-in-file-menu (concat (buffer-name) "...")
			"..."))]
	   ,@(unless (eq system-type 'windows-nt)
	       '(["Prett%_y-Print" ps-print-buffer-with-faces
		  :active (fboundp 'ps-print-buffer-with-faces)
		  :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]))
	   "-----"
	   ["%_Revert Buffer" revert-buffer
	    :active (or buffer-file-name revert-buffer-function)
	    :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
	   ["Re%_cover File..." recover-file]
	   ["Recover S%_ession..." recover-session]
	   "-----"
	   ["E%_xit XEmacs" save-buffers-kill-emacs]
	   )

	  ("%_Edit"
	   ["%_Undo" advertised-undo
	    :active (and (not (eq buffer-undo-list t))
			 (or buffer-undo-list pending-undo-list))
	    :suffix (if (or (eq last-command 'undo)
			    (eq last-command 'advertised-undo))
			"More" "")]
	   ["%_Redo" redo
	    :included (fboundp 'redo)
	    :active (not (or (eq buffer-undo-list t)
			     (eq last-buffer-undo-list nil)
			     (not (or (eq last-buffer-undo-list buffer-undo-list)
				      (and (null (car-safe buffer-undo-list))
					   (eq last-buffer-undo-list
					       (cdr-safe buffer-undo-list)))))
			     (or (eq buffer-undo-list pending-undo-list)
				 (eq (cdr buffer-undo-list) pending-undo-list))))
	    :suffix (if (eq last-command 'redo) "More" "")]
	   "----"
	   ["Cu%_t" kill-primary-selection
	    :active (selection-owner-p)]
	   ["%_Copy" copy-primary-selection
	    :active (selection-owner-p)]
	   ["%_Paste" yank-clipboard-selection
	    :active (selection-exists-p 'CLIPBOARD)]
	   "----"
	   ["T%_oggle protect alignment mode" protect-mode]
	   "----"
	   ["%_Insert gap" insert-gap]
	   ["%_Delete gap" delete-gap]
	   ["I%_nsert gap column" insert-gap-column]
	   ["D%_elete gap column" delete-gap-column]
	   ["De%_lete all gapped columns" delete-all-gap-columns]
	   "----"
	   ["Shift sequence %_left" shift-sequence-left]
	   ["Shift sequence %_right" shift-sequence-right]
	   ["Throw sequence left" throw-sequence-left]
	   ["Throw sequence right" throw-sequence-right]
	   ["Un%_block alignment" unblock-alignment]
	   ["Make %_flush alignment" make-flush-alignment]
	   ["Trim alignment l%_eft of cursor" trim-left]
	   ["Trim alignment ri%_ght of cursor" trim-right]
	   ["C%_onvert all gaps to ." gap-to-dot]
	   ["Co%_nvert T to U" t-to-u]
	   ["Con%_vert U to T" u-to-t]
	   ["Upper ca%_se alignment" uppercase-alignment]
	   ["Lo%_wer case alignment" lowercase-alignment]
	   )

	  ("%_Colour"
	   ("C%_ustomise colours"
	    ["Customise %_structure markup colours" customize-structure-colors]
	    ["Customise %_conservation markup colours" customize-cons-colors]
	    ["Customise %_base identity markup colours" customize-base-colors]
	    )
	   "---"
	   ["Colour %_buffer by structure consensus" paint-buffer-by-ss]
	   ["Colour buffer by %_conservation" paint-buffer-by-cons-generic]
	   ["Colour buffer by base %_identity" paint-buffer-by-base]
	   ["Colour columns with compensatory %_mutations" paint-buffer-by-compensatory-changes]
	   ["Colour buffer by current structure li%_ne" paint-buffer-by-current-ss]
	   )

	  ("%_Structure"
	   ["Jump to base %_pair" jump-to-pair]
	   ["Jump to base pair in an%_other window" jump-to-pair-in-other-window]
	   ["%_Fold sequence" fold-sequence]
	   ["Fold %_all sequences" fold-all-sequences]
	   ["Fold sequence %_block" fold-sequence-block]
	   ["Show %_structure" show-structure-ps]
	   ["Fold alig%_nment" fold-alignment]
	   "----"
	   ["%_Copy current structure line to SS_cons" copy-current-ss-to-cons]
	   ["%_Remove all #=GR lines" remove-all-gr-lines]
	   )

	  ("%_View"
	   ["%_New Frame" make-frame]
	   ["Frame on Other Displa%_y..." make-frame-on-display
	    :active (fboundp 'make-frame-on-display)]
	   ["%_Delete Frame" delete-frame
	    :active (not (eq (next-frame (selected-frame) 'nomini 'window-system)
			     (selected-frame)))]
	   "-----"
	   ["%_Split Window" split-window-vertically]
	   ["S%_plit Window (Side by Side)" split-window-horizontally]
	   ["%_Un-Split (Keep This)" delete-other-windows
	    :active (not (one-window-p t))]
	   ["Un-Split (Keep %_Others)" delete-window
	    :active (not (one-window-p t))]
	   "----"
	   ("N%_arrow"
	    ["%_Narrow to Region" narrow-to-region :active (region-exists-p)]
	    ["Narrow to %_Page" narrow-to-page]
	    ["Narrow to %_Defun" narrow-to-defun]
	    "----"
	    ["%_Widen" widen :active (or (/= (point-min) 1)
					 (/= (point-max) (1+ (buffer-size))))]
	    )
	   "----"
	   ["Show Message %_Log" show-message-log]
	   "----"
	   ["%_Goto Line..." goto-line]
	   ["%_What Line" what-line]
	   ("%_Bookmarks"
	    :filter bookmark-menu-filter)
	   "----"
	   ["%_Jump to Previous Mark" (set-mark-command t)
	    :active (mark t)]
	   )

	  ("%_Buffers"
	   :filter buffers-menu-filter
	   ["Go To %_Previous Buffer" switch-to-other-buffer]
	   ["Go To %_Buffer..." switch-to-buffer]
	   "----"
	   ["%_List All Buffers" list-buffers]
	   ["%_Delete Buffer" kill-this-buffer
	    :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
	   "----"
	   )

	  nil        ; the partition: menus after this are flushright
          
	  ("%_Help"
	   ["%_About XEmacs..." about-xemacs]
	   ["%_Home Page (www.xemacs.org)" xemacs-www-page
	    :active (fboundp 'browse-url)]
	   "-----"
	   ["What's %_New in XEmacs" view-emacs-news]
	   ["%_Obtaining XEmacs" describe-distribution]
	   "-----"
	   ("%_Info (Online Docs)"
	    ["Info Con%_tents" (Info-goto-node "(dir)")]
	    "-----"
	    ["XEmacs %_User's Manual" (Info-goto-node "(XEmacs)")]
	    ["XEmacs %_Lisp Reference Manual" (Info-goto-node "(Lispref)")]
	    ["All About %_Packages" (Info-goto-node "(xemacs)Packages")]
	    ["%_Getting Started with XEmacs" (Info-goto-node "(New-Users-Guide)")]
	    ["XEmacs In%_ternals Manual" (Info-goto-node "(Internals)")]
	    ["%_How to Use Info" (Info-goto-node "(Info)")]
	    "-----"
	    ["Lookup %_Key Sequence in User's Manual..."
	     Info-goto-emacs-key-command-node]
	    ["Lookup %_Command in User's Manual..." Info-goto-emacs-command-node]
	    ["Lookup %_Function in Lisp Reference..." Info-elisp-ref]
	    "-----"
	    ["Search %_Index in User's Manual/Lispref..."
	     Info-search-index-in-xemacs-and-lispref]
	    ["%_Search Text in User's Manual..." Info-search-text-in-xemacs]
	    ["S%_earch Text in Lisp Reference..."
	     Info-search-text-in-lispref]
	    )
	   
	   ("XEmacs %_FAQ"
	    ["%_FAQ (local)" xemacs-local-faq]
	    ["FAQ via %_WWW" xemacs-www-faq
	     :active (fboundp 'browse-url)])
	   ("%_Tutorials"
	    :filter tutorials-menu-filter)
	   ("%_Samples"
	    ["View Sample %_init.el" view-sample-init-el
	     :active (locate-data-file "sample.init.el")]
	    ["View Sample .%_gtkrc"
	     (Help-find-file (locate-data-file "sample.gtkrc"))
	     :included (featurep 'gtk)
	     :active (locate-data-file "sample.gtkrc")]
	    ["View Sample .%_Xdefaults"
	     (Help-find-file (locate-data-file "sample.Xdefaults"))
	     :included (featurep 'x)
	     :active (locate-data-file "sample.Xdefaults")]
	    ["View Sample %_enriched.doc"
	     (Help-find-file (locate-data-file "enriched.doc"))
	     :active (locate-data-file "enriched.doc")])
	   ("%_Commands, Variables, Keys"
	    ["Describe %_Mode" describe-mode]
	    ["%_Apropos..." hyper-apropos]
	    ["%_Command-Only Apropos..." command-hyper-apropos]
	    ["Apropos %_Docs..." apropos-documentation]
	    "-----"
	    ["Describe %_Key..." describe-key]
	    ["Show %_Bindings" describe-bindings]
	    ["Show M%_ouse Bindings" describe-pointer]
	    ["%_Recent Keys" view-lossage]
	    "-----"
	    ["Describe %_Function..." describe-function]
	    ["Describe %_Variable..." describe-variable]
	    ["%_Locate Command in Keymap..." where-is])
	   ("%_Misc"
	    ["%_Current Installation Info" describe-installation
	     :active (boundp 'Installation-string)]
	    ["%_No Warranty" describe-no-warranty]
	    ["XEmacs %_License" describe-copying]
	    ["Find %_Packages" finder-by-keyword]
	    ["View %_Splash Screen" xemacs-splash-buffer]
	    ["%_Unix Manual..." manual-entry])
	   "-----"
	   ["%_Recent Messages" view-lossage]
	   ["Send %_Bug Report..." report-xemacs-bug
	    :active (fboundp 'report-xemacs-bug)])))
      
      (set-menubar ralee-menubar)
      (set-menubar-dirty-flag)
      
      )

  (progn
    ; GNU emacs menu
    (define-key ralee-mode-map [menu-bar] (make-sparse-keymap))

    ; suppress these two
    (define-key ralee-mode-map [menu-bar options] 'undefined)
    (define-key ralee-mode-map [menu-bar tools] 'undefined)

    ; add these
    (defvar menu-bar-structure-menu (make-sparse-keymap "Structure"))
    (define-key ralee-mode-map [menu-bar structure] (cons "Structure" menu-bar-structure-menu))

    (defvar menu-bar-colour-menu (make-sparse-keymap "Colour"))
    (define-key ralee-mode-map [menu-bar colour] (cons "Colour" menu-bar-colour-menu))

    ; push the buffer menu after our additions
    (setq menu-bar-final-items (cons 'buffer menu-bar-final-items))

    ; structure menu items
    (define-key menu-bar-structure-menu [remove-base-pair]
      '(menu-item "Remove base pair" remove-base-pair
		  :help "Remove base pair involving the current column"))
    (define-key menu-bar-structure-menu [add-base-pair]
      '(menu-item "Add base pair" add-base-pair
		  :help "Add base pair between current column and the marked column"))
    (define-key menu-bar-structure-menu [separator-bp]
      '("--"))
    (define-key menu-bar-structure-menu [remove-all-gr-lines]
      '(menu-item "Remove all #=GR lines" remove-all-gr-lines
		  :help "Remove all #=GR lines"))
    (define-key menu-bar-structure-menu [copy-current-ss-to-cons]
      '(menu-item "Copy structure to SS_cons" copy-current-ss-to-cons
		  :help "Copy current structure line to SS_cons"))
    (define-key menu-bar-structure-menu [separator-gr]
      '("--"))
    (define-key menu-bar-structure-menu [fold-alignment]
      '(menu-item "Fold alignment" fold-alignment
		  :help "Fold the alignment and add a #=GC SS_cons line"))
    (define-key menu-bar-structure-menu [show-structure-ps]
      '(menu-item "Show structure" show-structure-ps
		  :help "Show the structure of the current sequence in gv"))
    (define-key menu-bar-structure-menu [fold-sequence-block]
      '(menu-item "Fold sequence block" fold-sequence-block
		  :help "Fold the subsequences in the current block"))
    (define-key menu-bar-structure-menu [fold-all-sequences]
      '(menu-item "Fold all sequences" fold-all-sequences
		  :help "Fold all sequences and add #=GR SS lines"))
    (define-key menu-bar-structure-menu [fold-sequence]
      '(menu-item "Fold sequence" fold-sequence
		  :help "Fold the current sequence and add a #=GR SS line"))
    (define-key menu-bar-structure-menu [separator-fold]
      '("--"))
    (define-key menu-bar-structure-menu [jump-to-pair-in-other-window]
      '(menu-item "Jump to base pair in other window" jump-to-pair-in-other-window
		  :help "Jump the cursor to the current base's pair in another window"))
    (define-key menu-bar-structure-menu [jump-to-pair]
      '(menu-item "Jump to base pair" jump-to-pair
		  :help "Jump the cursor to the current base's pair"))

    ; colour menu items
    (define-key menu-bar-colour-menu [lowlight-current-line]
      '(menu-item "Un-highlight current line" lowlight-current-line
		  :help "Un-highlight the current line"))
    (define-key menu-bar-colour-menu [highlight-current-line]
      '(menu-item "Highlight current line" highlight-current-line
		  :help "Highlight the current line"))
    (define-key menu-bar-colour-menu [ralee-highlight-line]
      '(menu-item "Highlight line mode" ralee-highlight-line
		     :visible t
		     :enable t
		     :help "Toggle the ability to highlight a line with left click"
		     :button (:toggle . ralee-highlight-line)))
    (define-key menu-bar-colour-menu [separator-highlight]
      '("--"))
    (define-key menu-bar-colour-menu [paint-buffer-by-current-ss-line]
      '(menu-item "Colour buffer by current structure line" paint-buffer-by-current-ss-line
		  :help "Colour buffer by current structure line"))
    (define-key menu-bar-colour-menu [paint-buffer-by-compensatory-changes]
      '(menu-item "Colour compensatory mutations" paint-buffer-by-compensatory-changes
		  :help "Colour columns with compensatory mutations"))
    (define-key menu-bar-colour-menu [paint-buffer-by-base]
      '(menu-item "Colour buffer by base identity" paint-buffer-by-base
		  :help "Colour buffer by base identity"))
    (define-key menu-bar-colour-menu [paint-buffer-by-cons-generic]
      '(menu-item "Colour buffer by conservation" paint-buffer-by-cons-generic
		  :help "Colour buffer by conservation"))
    (define-key menu-bar-colour-menu [paint-buffer-by-ss]
      '(menu-item "Colour buffer by structure consensus" paint-buffer-by-ss
		  :help "Colour buffer by structure consensus"))
    (define-key menu-bar-colour-menu [separator-customise]
      '("--"))
    ;; submenu
    (defvar menu-bar-colour-custom-menu (make-sparse-keymap "Customise colours"))
    (define-key menu-bar-colour-custom-menu [customize-base-colors]
      '(menu-item "Customise base identity markup colors" customize-base-colors
		  :help "Customise the colours used for base identity markup"))
    (define-key menu-bar-colour-custom-menu [customize-cons-colors]
      '(menu-item "Customise conservation markup colors" customize-cons-colors
		  :help "Customise the colours used for conservation markup"))
    (define-key menu-bar-colour-custom-menu [customize-structure-colors]
      '(menu-item "Customise structure markup colors" customize-structure-colors
		  :help "Customise the colours used for structure markup"))
    (define-key menu-bar-colour-menu [customise] (cons "Customise colours" menu-bar-colour-custom-menu))


    ; add to the file menu
    (define-key-after menu-bar-file-menu [separator-write]
      '("--")
      'separator-print)
    (define-key-after menu-bar-file-menu [write-ps]
      '(menu-item "Write postscript to file" write-ps
		  :help "Write a postscript file with coloured markup")
      'separator-print)
    (define-key-after menu-bar-file-menu [page-setup]
      '(menu-item "Postscript page setup" page-setup
		  :help "Setup the page for writing postscript")
      'separator-print)
    (define-key-after menu-bar-file-menu [separator-read]
      '("--")
      'separator-print)
    (define-key-after menu-bar-file-menu [read-scores]
      '(menu-item "Read scores file" read-scores
		  :help "Read a file of scores into #=GS lines")
      'separator-print)
    (define-key-after menu-bar-file-menu [read-stockholm]
      '(menu-item "Read stockholm alignment file" read-stockholm
		  :help "Read a stockholm format alignment file")
      'separator-print)

    ; add to the edit menu
    (define-key-after menu-bar-edit-menu [seperator-search]
      '(menu-item "Search for a sequence motif" ralee-motif-search
		  :help "Search for a sequence motif")
      'separator-search)
    (define-key-after menu-bar-edit-menu [separator-trim]
      '("--")
      'separator-search)
    (define-key-after menu-bar-edit-menu [gap-to-dot]
      '(menu-item "Convert all gaps to ." gap-to-dot
		  :help "Convert all gaps to .")
      'separator-search)
    (define-key-after menu-bar-edit-menu [u-to-t]
      '(menu-item "Convert U to T" u-to-t
		  :help "Convert U to T")
      'separator-search)
    (define-key-after menu-bar-edit-menu [t-to-u]
      '(menu-item "Convert T to U" t-to-u
		  :help "Convert T to U")
      'separator-search)
    (define-key-after menu-bar-edit-menu [lowercase-alignment]
      '(menu-item "Lower case alignment" lowercase-alignment
		  :help "Make the alignment lower case")
      'separator-search)
    (define-key-after menu-bar-edit-menu [uppercase-alignment]
      '(menu-item "Upper case alignment" uppercase-alignment
		  :help "Make the alignment upper case")
      'separator-search)
    (define-key-after menu-bar-edit-menu [trim-right]
      '(menu-item "Trim alignment right of cursor" trim-right
		  :help "Trim alignment right of cursor (inclusive)")
      'separator-search)
    (define-key-after menu-bar-edit-menu [trim-left]
      '(menu-item "Trim alignment left of cursor" trim-left
		  :help "Trim alignment left of cursor (exclusive)")
      'separator-search)

    (define-key-after menu-bar-edit-menu [throw-sequence-left]
      '(menu-item "Throw sequence left" throw-sequence-left
		  :help "Shift sequence (between cursor and previous gap character) to the left as far as it will go")
      'separator-search)
    (define-key-after menu-bar-edit-menu [throw-sequence-right]
      '(menu-item "Throw sequence right" throw-sequence-right
		  :help "Shift sequence (between cursor and previous gap character) to the right as far as it will go")
      'separator-search)
    (define-key-after menu-bar-edit-menu [shift-sequence-left]
      '(menu-item "Shift sequence left" shift-sequence-left
		  :help "Shift sequence (between cursor and previous gap character) to the left")
      'separator-search)
    (define-key-after menu-bar-edit-menu [shift-sequence-right]
      '(menu-item "Shift sequence right" shift-sequence-right
		  :help "Shift sequence (between cursor and previous gap character) to the right")
      'separator-search)

    (define-key-after menu-bar-edit-menu [separator-block]
      '("--")
      'separator-search)

    (define-key-after menu-bar-edit-menu [realign-block]
      '(menu-item "Re-align the current block" realign-block
		  :help "Re-align the current alignment block (using clustalw)")
      'separator-search)

    ;; submenu
    (defvar menu-bar-edit-unalign-menu (make-sparse-keymap "Unalign the current block"))
    (define-key-after menu-bar-edit-unalign-menu [unalign-block-left]
      '(menu-item "... to the left" unalign-block-left
		  :help "Unalign the current block to the left"))
    (define-key-after menu-bar-edit-unalign-menu [unalign-block-right]
      '(menu-item "... to the right" unalign-block-right
		  :help "Unalign the current block to the right"))
    (define-key-after menu-bar-edit-unalign-menu [unalign-block-center]
      '(menu-item "... to the center" unalign-block-center
		  :help "Unalign the current block to the center"))
    (define-key-after menu-bar-edit-unalign-menu [unalign-block-out]
      '(menu-item "... to the outside" unalign-block-out
		  :help "Unalign the current block to the outside"))
    (define-key-after menu-bar-edit-menu [unalign] 
      (cons "Unalign the current block" menu-bar-edit-unalign-menu)
      'separator-search)
    ;;;;

    (define-key-after menu-bar-edit-menu [undefine-block]
      '(menu-item "Undefine an alignment block" undefine-block
		  :help "Undefine an alignment block")
      'separator-search)
    (define-key-after menu-bar-edit-menu [define-block]
      '(menu-item "Define an alignment block" define-block
		  :help "Define an alignment block")
      'separator-search)

    (define-key-after menu-bar-edit-menu [separator-gap]
      '("--")
      'separator-search)
    (define-key-after menu-bar-edit-menu [make-flush-alignment]
      '(menu-item "Make the alignment flush" make-flush-alignment
		  :help "Add gap chars to the end of the alignment to make it flush")
      'separator-search)
    (define-key-after menu-bar-edit-menu [unblock-alignment]
      '(menu-item "Unblock alignment" unblock-alignment
		  :help "Convert a blocked Stockholm alignment into an unblocked one")
      'separator-search)
    (define-key-after menu-bar-edit-menu [delete-all-gap-columns]
      '(menu-item "Delete all gapped columns" delete-all-gap-columns
		  :help "Delete all columns that contain only gaps")
      'separator-search)
    (define-key-after menu-bar-edit-menu [delete-gap-column]
      '(menu-item "Delete gap column" delete-gap-column
		  :help "Delete the current column if it contains only gaps")
      'separator-search)
    (define-key-after menu-bar-edit-menu [insert-gap-column]
      '(menu-item "Insert gap column" insert-gap-column
		  :help "Insert a column of gap characters")
      'separator-search)
    (define-key-after menu-bar-edit-menu [delete-gap]
      '(menu-item "Delete gap" delete-gap
		  :help "Delete a gap")
      'separator-search)
    (define-key-after menu-bar-edit-menu [insert-gap]
      '(menu-item "Insert gap" insert-gap
		  :help "Insert a gap")
      'separator-search)
    (define-key-after menu-bar-edit-menu [separator-protect]
      '("--")
      'separator-search)
    (define-key-after menu-bar-edit-menu [protect-mode]
      '(menu-item "Protect alignment" protect-mode
		     :visible t
		     :enable t
		     :help "Protect the alignment from inadvertant insertion and deletion"
		     :button (:toggle . protect-mode))
      'separator-search)
    )
  )


(provide 'ralee-menu)
