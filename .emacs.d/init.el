;;; init.el

;; 主要参考文献
;;   - https://emacs-jp.github.io/tips/emacs-in-2020
;;       - 大部分はこの設定を真似している
;;   - Emacs実践入門
;;
;; メモ
;;   - C-S-sは Ctrl+大文字のS, つまりCtrl + Shift + sの意味
;;   - emacs -batch -f batch-byte-compile init.el でバイトコンパイル
;;
;; キー操作
;;   - C-u 5 C-n: 5行下に移動
;;   - C-u 5 C-k: 改行含めて5行分消去
;;   - M-;(comment-dwim) : コメントアウト、コメント解除 (https://tomoya.hatenadiary.org/entry/20091015/1255593575)
;;     - C-c;, C-c: は捨てること
;;   - C-qで特殊文字。改行文字を置換するとき便利
;;   - C-M-SPC: リージョン選択範囲を一気に増やせる
;;   - C-x h: バッファ全体を選択
;;   - M-h: 段落を選択
;;   - C-x C--: フォントサイズを小さく
;;   - C-x C-+: フォントサイズを大きく
;;
;; キー操作 (ヘルプ系)
;;   - M-x info: infoを参照
;;   - F1: ヘルプ
;;     - F1 F1: ヘルプコマンドの一覧
;;     - F1 b (M-x describe-bindings): キーの割当と実行されるコマンドの対応表
;;     - F1 k キーバインド RET (M-x describe-key): キーがどのコマンドに対応するか
;;     - F1 w コマンド名 RET (M-x where-is): コマンドがどのキーに対応するか
;;     - F1 f 関数名 RET (M-x describe-function): 関数の説明
;;     - F1 v 変数名 RET (M-x describe-variable): 変数の説明

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;;; leafの初期化 ;;;
;;;;;;;;;;;;;;;;;;;;

;; https://emacs-jp.github.io/tips/emacs-in-2020
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;;; cus-edit.c
;; leafの :customで設定するとinit.elにcustomが勝手に設定を追記する
;; この状況になると、変数の二重管理になってしまうので、customがinit.elに追記しないように設定
;; 詳細: https://qiita.com/conao3/items/347d7e472afd0c58fbd7
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; cus-start.c
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            ;; インデントにタブを使用しない
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  ; C-hでバックスペース
  (keyboard-translate ?\C-h ?\C-?))

;; Emacsの外でファイルが書き変わったときに自動的に読み直すマイナーモード
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;; Cやそれに似た構文を持つファイルに関する設定
(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  :mode-hook
  (c-mode-hook . ((c-set-style "bsd")
                  (setq c-basic-offset 4)))
  (c++-mode-hook . ((c-set-style "bsd")
                    (setq c-basic-offset 4))))

;; 選択している状態で入力したときに、regionを削除して挿入するマイナーモード
(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; 対応するカッコを強調表示するマイナーモード
(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0))
  :global-minor-mode show-paren-mode)

;; kill-ringの数を制御したり、kill-lineの挙動を変更したり
(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)  ; 行頭でのC-kにより行末の改行も含めて行全体がkill
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

;; バックアップファイルを一箇所に集める
;; NOTE: 事前に~/.emacs.d/backupの作成が必要
(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))


;; 自動保存されたファイルのリスト。~/.emacs.d/backup以下にまとめて保存
(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; leafによる個別設定 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ivy + swiper + counsel
;; ミニバッファの補完を強化
;; https://emacs-jp.github.io/tips/emacs-in-2020
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)  ; imenuによりインデキシングされた項目にジャンプ
           ("C-x C-r" . counsel-recentf))  ; 履歴からファイルを開く
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)


;;; flycheck
;; リアルタイムにソースのエラーやワーニングを表示するマイナーモード
;; C-c ! l でエラーリストを表示
;;
;; NOTE:
;;   - LANG=ja_JP.UTF-8 に設定していると、C++のエラーメッセージが日本語になるためパースに失敗。
;;     回避するためにはexport LC_MESSAGES=Cを設定 (https://github.com/flycheck/flycheck/issues/1578)
;;   - 特定言語でチェックが働かないときはC-c ! vで原因究明可能。
;;     例えばPythonならflake8, pylint, mypyなどを予めインストールする必要あり
;;     参考: https://stackoverflow.com/questions/37720869/emacs-how-do-i-set-flycheck-to-python-3
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;;; company
;; 入力補完のためのパッケージ
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))


;;; magit
;; C-x gでmagitを開く
;; ?でヘルプ
(leaf magit
  :bind (("C-x g" . magit-status))
  :ensure t
)

;;; yasnippet
;; https://mako-note.com/python-emacs-ide/#yasnippet
;; TODO: yasnippet-snippets によるスニペットダウンロードに失敗する
;; ようなので、https://github.com/AndreaCrotti/yasnippet-snippets
;; から取得したファイルを~/.emacs.d/snippetsに手動で配置
;; TODO: company-mode/backend-with-yas の箇所でバイトコンパイルに失敗
(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :custom ((yas-indent-line . 'fixed)
           (yas-global-mode . t)
                                 )
  :bind ((yas-keymap
          ("<tab>" . nil))            ; conflict with company
         (yas-minor-mode-map
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all)))
  :config
  (leaf yasnippet-snippets :ensure t)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    )
  :hook
  ((company-mode-hook . set-yas-as-company-backend))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-packageによる個別設定 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; anaconda mode
;; https://gist.github.com/yiufung/d8216038252f0488198e8b6af1e2ece4
;; 事前にuse-packageのインストールが必要
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )
(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )
(use-package company-quickhelp
  ;; Quickhelp may incorrectly place tooltip towards end of buffer
  ;; See: https://github.com/expez/company-quickhelp/issues/72
  :ensure t
  :config
  (company-quickhelp-mode)
  )


;;; autoinsert
;; 事前作成したテンプレートの挿入
;; 以前は
;; https://higepon.hatenablog.com/entry/20080731/1217491155
;; に従っていたが、より低機能な以下に変更
;; http://howardism.org/Technical/Emacs/templates-tutorial.html
(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (define-auto-insert "\\.cpp$" "template.cpp")
  (define-auto-insert "\\.py$" "template.py"))


;;; markdown mode
;; https://jblevins.org/projects/markdown-mode/
;; C-c C-c pでプレビューを開く (pandoc必要)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; pandocの場所を指定
;; https://stackoverflow.com/questions/14231043/emacs-markdown-mode-error-on-preview-bin-bash-markdown-command-not-found/19589576
(custom-set-variables '
(markdown-command "/cygdrive/c/ProgramData/Anaconda3/Scripts/pandoc"))


;;;;;;;;;;;;
;;; 表示 ;;;
;;;;;;;;;;;;

;;; font
;; 「Emacs実践入門」より
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Myrica M"))
;; 半角:全角 = 1:2に
(add-to-list 'face-font-rescale-alist '(".*Myrica.*" . 1.2))

;; 列番号を表示
(column-number-mode 1)
;; shellモードでパスワードを伏字に
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 左端に行番号を表示
(global-linum-mode t)
;; タブを空白スペース4個分として表示（デフォルトは8）
(setq-default tab-width 4)
;; カーソル行を目立たせる
;; http://ergoemacs.org/emacs/emacs_high_light_current_line.html
;; (global-hl-line-mode 1)
;; テーマを設定
(load-theme 'misterioso t)
;; あらかじめpackageからzenburn-themeのインストールが必要
;; (load-theme 'zenburn t)


;;;;;;;;;;;;
;;; misc ;;;
;;;;;;;;;;;;

;; C-x C-bでlist-buffersのリッチ版が開くようにする
;; (global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key (kbd "C-x C-b") 'bs-show)
;; goto. M-g gでも可能
(global-set-key (kbd "C-c g") 'goto-line)
;; 行の折り返しを切り替え
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
;; C-zを押してもアイコン化しない
(cond (window-system
        (global-unset-key "\C-z")
        ))

;; スクリプトを保存する時，自動的に chmod +x
;; https://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; カーソル位置にあるElisp関数や変数の情報をエコーエリアへ表示
;; 「Emacs実践入門」より
(defun elisp-mode-hooks ()
  "Lisp-mode-hooks."
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; 日本語の言語設定をしたあと、UTF-8の優先順位を最大にする
;; 現在の設定はM-x describe-current-coding-systemで確認
;; 「Emacs実践入門」より
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
