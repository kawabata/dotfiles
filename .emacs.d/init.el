;;; init.el --- My Emacs Initialization/Customization file  -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: My Emacs Initialization/Customization file
;; Package-Requires: ((emacs "24.4"))
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: around 1995 (Since my first Emacs experience...)
;; Modified: 2015-12-06
;; Version: 14
;; Keywords: internal, local
;; Human-Keywords: Emacs Initialization
;; Namespace: tkw-
;; URL: https://github.com/kawabata/dotfiles/.emacs.d/init.el

;;; Commentary:

;;;; init.el 設定ポリシー
;;
;; - 初期設定は "init.el" 単独で行い、複数のファイルでの分割設定は避ける。
;; - パッケージの管理は、Caskを用いる。
;;   Cask ファイルは、init.el から自動生成する。 (tkw-generate-cask)
;;   Windows では cask が動かないため、use-package の :ensure でインストールする。
;; - 本 init.el はバッチ処理では使用しない。
;;   + init.el 内で noninteractive 変数チェックをしない。
;; - 変数の設定は、set-variable で行う。
;; - 変数の内容の変更は、 defvar で変数を宣言した後に、変数を操作し、
;;   その結果を set-variable する。pushnew 等で直接操作するのは避ける。
;; - 非組み込み関数を使う場合は、declare-function で関数を宣言する。
;;   + use-package を使う場合は、:defines, :functions で変数と関数を定義する。

;;;; Emacsおよび関連ソフトのインストール方法
;;;;; Ubuntu
;;    % sudo add-apt-repository ppa:cassou/emacs
;;    % sudo apt-get update
;;    % sudo apt-get install emacs-snapshot
;;    % emacs-snapshot

;;;;; Ubuntu Source Build
;;    % sudo apt-get install build-essential
;;    % sudo apt-get build-dep emacs23
;;    % sudo apt-get install git ccache
;;    % export PATH=/usr/lib/ccache:$PATH
;;    % git clone git://git.savannah.gnu.org/emacs.git
;;    % cd emacs
;;    % ./configure
;;    % make
;;    % sudo make install

;;;;; Macintosh (Yamamoto Patch)
;; http://www.math.s.chiba-u.ac.jp/~mituharu/emacs-mac.git
;; -  更新履歴
;;    * emacs-24.5-mac-5.13 (2015-10-31)
;;    * emacs-24.5-mac-5.12 (2015-10-30)
;;    * emacs-24.5-mac-5.11 (2015-09-27)
;;
;; - Brew
;;   日本語インラインパッチ付き
;;   % brew install --cocoa --srgb --with-gnutls --japanese emacs -v
;;
;; - git
;;   % git clone http://www.math.s.chiba-u.ac.jp/~mituharu/emacs-mac.git/
;;   % cd emacs-mac
;;   % ./autogen.sh
;;   % ./configure --enable-mac-app --with-mac CC="/usr/bin/gcc"
;;   % make
;;   % sudo make install
;;
;; - パッチを使う場合
;;   % wget ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-24.5-rc3.tar.xz
;;   % wget ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-rc3-mac-5.6.tar.gz
;;   % tar xfz emacs-24.5-rc3-mac-5.6.tar.gz
;;   % tar xfJ emacs-24.5-rc3.tar.xz
;;   （アイコンを変更したい場合）
;;   % wget ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-hires-icons-1.0.tar.gz
;;   % tar xfz emacs-hires-icons-1.0.tar.gz
;;   % cd emacs-24.5
;;   （ここまで）
;;   % cat ../emacs-24.5-rc3-mac-5.6/patch-mac | patch -p1   # （注意。場合によっては -p0）
;;   % rsync -av ../emacs-24.5-rc3-mac-5.6/mac/ ./mac/
;;   % rsync -av ../emacs-24.5-rc3-mac-5.6/src/ ./src/
;;   % rsync -av ../emacs-24.5-rc3-mac-5.6/lisp/ ./lisp/
;;   （アイコンを変更したい場合）
;;   % cp ./nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns ./mac/Emacs.app/Contents/Resources/Emacs.icns
;;   % cp -pr ../emacs-hires-icons-1.0/etc/images/* ./etc/images
;;   （ここまで）
;;   % ./configure --enable-mac-app --with-mac CC="/usr/bin/gcc"
;;   ※ 注意 :: --enable-profiling は絶対に指定してはいけない。（速度が極端に遅くなる。）
;;   ※ 注意 :: 'unrecognized command line option "-fconstant-cfstrings" ならば、/usr/bin/gcc の代わりに /usr/local/bin/gcc が使われている。
;;   % make # -j CC="/usr/bin/gcc -std=gnu99" CFLAGS=-g3 bootstrap
;;   % sudo make install

;;;;; Macintosh (HEAD / NS)
;;
;; - ccache による再コンパイルの高速化
;;   : brew install ccache
;;   : export PATH=/usr/local/opt/ccache/libexec:$PATH
;;   : ./configure --with-ns
;;   : make bootstrap
;;   : make install
;;   : open ./nextstep/Emacs.app
;;   : EMACS=/Users/kawabata/cvs/emacs/src/emacs cask update
;;
;; + トラブルシューティング
;;   tputs がない、というエラーが出たら、config.log のエラー出力場所をよく見る。
;;   librsvg 等、思わぬライブラリが悪さをしていることがある。関係ライブラリを一度、
;;   アンインストールして再度インストールする。
;;   freetype がないと、フォントはまともに表示できない。
;;   ./configure 時に、freetype=no と出る場合は、以下を試すとうまくいく場合がある。
;;  % ln -s /usr/local/opt/freetype/include/freetype2 /usr/local/include/freetype

;;;;; Macintosh (HEAD / X-Windows)
;; - Troubleshooting
;;  : unexec: not enough room for load commands for new __DATA segments
;;  : make[2]: *** [bootstrap-emacs] Error 1
;;  : make[1]: *** [src] Error 2
;;  : make[1]: *** Waiting for unfinished jobs....
;;  : make: *** [bootstrap] Error 2
;;
;;  上記のエラーがでたら、{src,lib}/Makefile の
;;  LD_SWITCH_SYSTEM_TEMACS の headerpad_extra の値に注意する。
;;  変更：690→6C8
;;  configure.ac のif文がミスしている場合あり。

;;;;; Windows
;; - GnuPack (13.00-2015.05.05)
;;   + URL :: http://sourceforge.jp/projects/gnupack/releases/p10360
;;   + GnuPack の機能をフルに活かすには、Cygwin のgpgなどのコマンドが必要。
;;   + ホームディレクトリに .emacs.d のシンボリックリンクを作成する。
;;     mklink を使ってもよいが、CYGWIN で、export CYGWIN="winsymlinks:nativestrict"
;;     を宣言すれば、ln コマンドも利用できる。
;;
;;   + コマンドプロンプトで emacs.exe を --debug-init で起動可。
;;   + Windows では Cask は動かないので、use-package の :ensure を利用。
;;
;;   + Cygwin と Emacs ではパスは以下のように異なるので注意。
;;     - "c:/Windows/system32" → "/cygdrive/c/windows/system32"
;;     - "path1;path2;path3"   → "path1:path2:path3"
;;     parse-colon-path 関数が自動的に環境に応じて処理してくれる。
;;     なお、cygwin の rootディレクトリは
;;     HKEY_LOCAL_MACHINE\SOFTWARE\Cygwin\setup\rootdir = C:\cygwin64
;;     となっているが、これは当然、Emacsからは見えないので、
;;     (getenv "SHELL") に /bin/sh があってもアクセスできない。
;;
;; - 個別設定
;;   [SetEnv]
;;   CYGWIN_DIR     = C:\cygwin64
;;     + HOME を自分のディレクトリにする。
;;     + color_theme をコメントアウトする。

;;;;; Cask
;; - マニュアル
;;   http://cask.github.io/dsl.html
;; - 開発ガイド
;;   http://www.kaichan.info/blog/2014-02-23-emacs-cask.html
;; - Linux
;;   : $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
;; - Mac
;;   : $ brew install cask
;;   : % cask upgrade # upgrade-cask でもOK

;;;;; 個別パッケージインストール
;; - m17n-lib (https://savannah.nongnu.org/projects/m17n)
;;   - コンパイルには libintl.h が必要なため、
;;     : brew install gettext
;;     : brew link gettext (gettext はkeg onlyなため)
;;     を実行する。
;; - TeX (MacTeX, W32TeX, TexLive) (/usr/texbin/)
;; - Go Language (/usr/local/go)
;; - Digital Mars D language
;; - GnuPG (Agent)
;;   MacGPGの場合はシステム環境設定のGPGPreferencesにおいて
;;   "Store Password in KeyChain" を設定
;; - R (http://cran.r-project.org/bin/macosx/)
;; - X Windows (XQuartz)
;; - csharp
;;   MDK (http://www.mono-project.com/download/)
;;   自動的に /usr/bin に mcs がインストールされる。
;; - パッケージマネージャ (MacPorts/HomeBrew) でインストール
;;   + Haskell / Cabal (Pandoc via Cabal)
;;     - インストール
;;       : % export PATH=$HOME/Library/Haskell/bin:$PATH
;;       : % cabal update
;;       : % cabal install cabal-install
;;     - トラブルシューティング
;;       : % ghc-pkg check
;;       : % sudo ghc-pkg recache
;;       : % cabal install --reinstall --force-reinstall <pkg-name>
;;     - 全削除
;;       : % rm ~/.ghc ~/.cabal
;;   + Maxima

;;;; Emacsの公式ドキュメント一覧
;; - [[info:emacs#Top][Emacs]]
;; - [[info:eintr#Top][An Introduction to Programming in Emacs Lisp]]
;; - [[info:elisp#Top][Emacs Lisp]]
;; - Reference Cards (/etc/refcards)
;;   - refcard.pdf :: Emacs
;;   - calccard.pdf :: Calc
;;   - dired-ref.pdf :: Dired
;;   - gnus-booklet.pdf :: Gnus
;;   - gnus-refcard.pdf :: Gnus
;;   - orgcard.pdf :: Org

;;; Code:

(eval-when-compile (require 'cl))
(setq init-file-debug t)
(cd "~/") ; ホームディレクトリにcd
(setq force-load-messages t)
(message (format "Startup time: %s" (format-time-string "%Y/%m/%d %H:%M:%S")))

;;; マクロ・関数
;; 本ファイルで使用するマクロと関数の定義
;;;; ライブラリ遅延読み込み
;; hinted by http://lunaryorn.com/blog/2013/05/31_byte-compiling-eval-after-load.html
;; * with-eval-after-load (Emacs 24.4 から)
;;   バイトコンパイル時は、未定義マクロは関数とみなすので注意。
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;;;; 関数のコマンド化
(defmacro command (&rest body)
  `(lambda () (interactive) ,@body))

;;;; major-mode のモードライン名を変更
;; - minor-mode は diminishを使用する。
(defun tkw-major-mode-lighter (mode string &optional face)
  "Change the mode-line MODE lighter to STRING with FACE."
  (let ((string (if face (propertize string 'face face) string)))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              `(lambda () (setq mode-name ,string)))))

;;;; 大きすぎるファイルでは適用しないモードの判定
(defun tkw-large-file-p (&optional char-size)
  "現在のバッファの文字数が CHAR-SIZE を越えるか判定する.
デフォルト値は 100,000."
  (< (or char-size 100000) (point-max)))

;;;; そのほかの関数
;; (defun add-to-auto-mode-alist (pair)
;;   (pushnew pair auto-mode-alist :test 'equal))
;; (defun remove-from-auto-mode-alist (pair)
;;   (callf2 delete pair auto-mode-alist))
;; (defun tkw-assoc-delete-all (key alist)
;;   (callf2 'remove-if (lambda (equal key (car item))) alist))

;;;; cask
;; Windows ではCaskは caskxy
(when (and (not (equal window-system 'w32))
	   (or (require 'cask nil t) ;; MacOS X (homebrew)
	       (require 'cask "~/.cask/cask.el" t))) ;; Linux (install by curl)
  (cask-initialize))

;;;; 初期化
;;   Emacsは init.el 読み込み後に各パッケージへのload-path設定を行い
;;   XXX-autoloads.el を読み込む。このままでは init の段階では
;;   require/locate-library ができないため、(package-initialize) を事前
;;   に実行する。

;;(require 'autoinsert) ; abc-mode-autoloads.el 対策。
(package-initialize)
(setq package-enable-at-startup nil) ; 初期化済みなので自動初期化は停止。

;; パッケージの情報は、~/.emacs.d/elpa/archives/ に格納される。自分で
;; パッケージを作る場合は、 package-x.el の、
;; `package-upload-{buffer,file}' を利用する。archive-contents ファイ
;; ルが自動生成される。

;; パッケージ読み込み後は、読み込みライブラリを表示する。
;; （繁雑な XXXX-autoload は表示させない。）

;; ただし、package-menu-execute 時のみ、(XXX-autoload.elを) 表示させない。
(defadvice package-menu-execute (around tkw-package-menu-execute-suppress-load-messages)
  "Suppress displaying load file messages."
  (let ((force-load-messages nil))
    ad-do-it))
(ad-activate 'package-menu-execute)

;;;; Package Archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ;; sunrise-commander
        ;; ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
        ;; org-mode
        ("org"   . "http://orgmode.org/elpa/")
        ))
;; ローカルレポジトリを追加
(when (file-exists-p "~/.emacs.d/local-packages/archive-contents")
  (pushnew '("local" . "~/.emacs.d/local-packages/")
              package-archives :test 'equal))

;;;;; Marmalade について
;;
;; MELPAとMarmalade の違い
;; | archives   | MELPA                   | Marmalade            |
;; |------------+-------------------------+----------------------|
;; | source     | Public Repository       | Upload manually      |
;; | update     | Automatic               | Manual               |
;; | XXX-pkg.el | automatically generated | prepare by oneself   |
;; | version    | year-date-revision      | prepaed by oneself   |
;; | curaton    | relatively safe         | random fork possible |
;;
;; marmalade は危険なファイルが入る可能性があるので、専用の関数で処理する。
;; 利用後は M-x init-package-archives して、もとに戻す。
;;(defun list-packages-marmalade ()
;;  (interactive)
;;  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
;;  (list-packages))

;;;; Emacs 25 対策
;; 2015年1月時点では、use-package の前にこれを読まないと Emacs 25 ではエラーになる。
(require 'xref nil t)

;;;; use-package
;; - url :: https://github.com/jwiegley/use-package
;; 非標準パッケージは use-package で管理する。
;; （標準ライブラリは use-package では管理しない）
;; これを利用できない環境では、パッケージ管理のソフトはインストールしない。
;;;;; 起動時の use-package の抑止
;; - init.el を外部に持ちだした時など、use-package を抑止したいときは
;;   Emacs を、オプション "--qq" で起動する。
;; - use-package が未インストールか、抑止されている場合は空マクロにする。
(when (or (member "--qq" command-line-args)
          (null (require 'use-package nil t)))
  (warn "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
  (defmacro use-package (&rest _args)))
;; 後の startup.el におけるオプション認識エラーを防止
(add-to-list 'command-switch-alist '("--qq" . (lambda (switch) nil)))

;;;;; 形式的宣言
(use-package use-package :no-require t :ensure t :defer t)

;;;; pallet
;; 自動的に ~/.emacs.d/Cask ファイル と Packaging を同期する。
;; Network に繋がっていないとエラーになる。要対策。
(use-package pallet :no-require t :ensure t
  :commands (pallet-init)
  :config
  (condition-case nil
        (pallet-init)
      (error nil)))

;;;; 要チェックライブラリ
;; - logito
;; - org-journal
;; - org-magit
;; - org-toc
;; - polymode
;; - prodigy
;; - sourcemap
;; - strie
;; - sync-env
;; - tern
;; - theme-changer
;; - ucs-cmds
;; - ucs-utils
;; - unicode-fonts
;; - wisi
;; - xmlgen

;;; キーボード設定
;;;; bind-key

;; bind-key* は、emulation-mode-map-alists を利用することにより、
;; minor-mode よりも優先させたいキーのキーマップを定義できる。
;; bind-key.el がない場合は普通のbind-key として振る舞う。
(use-package bind-key :no-require t :defer t :ensure t)
(unless (require 'bind-key nil t)
  (defun bind-key (key cmd &optional keymap)
    (define-key (or keymap global-map) (kbd key) cmd))
  (defun bind-key* (key cmd) (global-set-key (kbd key) cmd)))

(defun tkw-this-command-char ()
  "Last character of `this-command'."
  (string-to-char (substring (format-kbd-macro (this-command-keys)) -1)))

;;;; 回転系コマンド
;; 回転系コマンドは、doremi.el や、smartrep を使って実装する。
;; | key | function               |
;; |-----+------------------------|
;; | f   | next-multiframe-window |
;; | h   | rotate-han             |
;; | k   | rotate-kana            |
;; | l   | rotate-latin           |
;; | s   | rotate-font-size       |
;; | t   | rotate-theme           |
(defvar tkw-rotate-map (make-sparse-keymap))

(bind-key "M-r" tkw-rotate-map) ;; move-to-window-line-top-bottom を上書き
(bind-key "M-R" tkw-rotate-map) ;; M-r が別の目的に使われていた場合

;;;; emacs-24.4-mac-5.2 の新機能
;; 詳細は macport.texi と macfns.c を参照。
;; - US keyboard :: com.apple.keylayout.US
;; - GoogleIME :: com.google.inputmethod.Japanese.base
(declare-function mac-input-source "macterm.c")
(declare-function mac-osa-script "macterm.c")
(when (fboundp 'mac-select-input-source)
  (defun tkw-mac-selected-keyboard-input-source-chage-function ()
    (let ((mac-input-source (mac-input-source)))
      (if (string-match "\\.US$" mac-input-source)
          ;; (string-match "\\.Roman$" mac-input-source)
          (set-cursor-color "Yellow")
        (set-cursor-color "Red"))))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'tkw-mac-selected-keyboard-input-source-chage-function))

;;; 標準設定
;;;; (emacs) 7 Basic
;;;;; (emacs) 7.1 Inserting Text
;; `C-q 数字' で文字を入力する際の進数
(setq read-quoted-char-radix 16)

;;;;; (emacs) 7.2 Changing the Location of Point
(bind-key "C-x :" 'goto-line) ; M-g g に加えて追加

;;;; (emacs) 8 Minibuffers
;; ミニバッファの変化が激しいと思うときは、'grow-onlyに。
(setq resize-mini-windows 'grow-only)

;;;; (emacs) 11.4 The Mark Ring
;; C-u C-SPC C-SPC... で繰返しマークをpopできるようにする.
(setq set-mark-command-repeat-pop t)

;;;; (emacs) 11.7 Disabling Transient Mark Mode
(transient-mark-mode t)

;;;; (emacs) 12 Killing and Moving Text
;;;;; (emacs) 12.1.2 Killing by Lines
(setq kill-whole-line t)

;;;;; (emacs) 12.3.2 Cut and Paste with Other Window Applications
;; VNC等で動きが遅くならないための工夫
(setq select-active-regions nil)

;;;;; (emacs) 12.5 Rectangles (rect.el)
(bind-key "C-x r K" 'tkw-kill-rectangle-save)
(bind-key "C-x r t" 'string-rectangle)

;;;; (emacs) 14 Controlling the Display
;;;;; (emacs) 14.16 Useless Whitespace
;; 行末の空白を表示
(setq-default show-trailing-whitespace nil)
(defun turn-on-show-trailing-whitespace  () (interactive) (setq show-trailing-whitespace t))
(defun turn-off-show-trailing-whitespace () (interactive) (setq show-trailing-whitespace nil))
(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-show-trailing-whitespace)
;; フレームの横幅が一定以下になれば自動的に truncate-window-mode にする。
;; nil の場合はこの機能を無効にする。（デフォルトは50）

;;;;; (emacs) 14.18 Optional Mode Line Features
(setq line-number-display-limit 10000000)
(setq line-number-display-limit-width 1000)

(line-number-mode t)
(column-number-mode t)

;; time.el
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '(month "/" day
        " " 24-hours ":" minutes ;; ":" seconds
        (if mail " Mail" "")))
(display-time)

;;;;; (emacs) 14.20 Displaying the Cursor
(blink-cursor-mode 0)

;;;;; (emacs) 14.21 Line Truncation
(bind-key "C-c t" 'toggle-truncate-lines)

;;;;; (emacs) 14.22 Visual Line Mode
;;(global-visual-line-mode t)

;;;; (emacs) 15.1 Incremental Search
(setq lazy-highlight-initial-delay 0) ; isearch のハイライト反応を良くする。
(bind-key "C-k" 'isearch-edit-string isearch-mode-map)

;;;; (emacs) 18.2 Visiting Files
(when (require 'openwith nil t)
  ;; openwith-file-handler でデバッガに落ちないようにする。
  (defadvice openwith-file-handler (around tkw-openwith-handler activate)
    (let (debug-on-error)
      ad-do-it))
  ;; ディレクトリを開こうとする場合は、OSのファイルブラウザで開けるようにする。
  (defadvice find-file (around tkw-find-file activate)
    "Open directory by external application if its name ends with /."
    (if (and (file-directory-p (ad-get-arg 0))
             (string-match "/$" (ad-get-arg 0))
             (y-or-n-p (concat "Open " (ad-get-arg 0)
                               " in File Browser? ")))
        ;; openwith は、強制エラーを発生させるので、 debug-on-error を
        ;; nil にして、デバッガに入るのを抑止する。
        (cond ((eq system-type 'windows-nt)
               (openwith-open-windows (list (ad-get-arg 0))))
              ((eq system-type 'gnu/linux)
               (openwith-open-unix "nautilus" (list (ad-get-arg 0))))
              (t
               (openwith-open-unix "open" (list (ad-get-arg 0)))))
      ad-do-it)))
;; warn only if file size is more than 100Mbyte.
(setq large-file-warning-threshold 100000000)

;;;; (emacs) 20 Windows
(bind-key "A-M-n" 'enlarge-window)
(bind-key "A-M-p" 'shrink-window)
(bind-key "A-M-f" 'enlarge-window-horizontally)
(bind-key "A-M-b" 'shrink-window-horizontally)
;;(bind-key "<C-right>" (command (scroll-left 8)))
;;(bind-key "<C-left>" (command (scroll-right 8)))
;;(bind-key "<M-up>" (command (scroll-up 1)))
;;(bind-key "<M-down>" (command (scroll-down 1)))
;;(bind-key "<C-right>" (command (scroll-left 1)))
;;(bind-key "<C-left>" (command (scroll-right 1)))
;; 現在のウィンドウを垂直方向に伸ばす。まず、下にウィンドウがあれば、
;; それを消して、無ければ、上を消して、上もなければ、
;; delete-other-windowsする。
(defun tkw-enlarge-window-vertically ()
 (command
   (let ((current-tl  (car (window-edges (selected-window))))
         (next-tl     (car (window-edges (next-window))))
         (previous-tl (car (window-edges (previous-window)))))
     (cond ((= current-tl next-tl)
            (other-window 1) (delete-window)
            (other-window -1))
           ((= current-tl previous-tl)
            (other-window -1) (delete-window))
           (t (delete-other-windows))))))
(bind-key "C-x 9" 'tkw-enlarge-window-vertically)
;; 上下スクロールする際に、カーソルが追随するかどうかを切替える。

;;;; (emacs) 21 Frames and Graphical Displays
;;;;; (emacs) 21.7 Frame Commands
(bind-key "A-M-o" 'other-frame)

;;;;; (emacs) 21.15 Tool Bars
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))

;;;; (emacs) 22 International
;;;;; (emacs) 22.2 Language Environments
;; coding関係の設定の前にこれを行う。
;;(set-language-environment "Japanese")

;;;;; (emacs) 22.6 Recognizing Coding Systems
(prefer-coding-system
 (cond ((equal system-type 'windows-nt) 'utf-8-dos)
       (t 'utf-8)))

;;;; (emacs) 25.1 Words
(bind-key "C-M-h" 'backward-kill-word) ; 単語をまとめてBS。

;;;; (emacs) 25.8 Outline Mode

;;;; (emacs) 26.5 Comments
(bind-key "C-c ;" 'comment-region)

;;;; (emacs) 32.7 Mail-Composition Methods
(setq mail-user-agent 'gnus-user-agent) ; Gnusをメールに使用する。

;;;; (emacs) 36 Running shell commands from Emacs
;; SHELL環境変数が使用される。
;;(defvar explicit-shell-file-name)
;; CYGWIN 環境では、SHELL環境変数が "/bin/bash" などだと、Emacsでは解釈できない。
(if (equal window-system 'w32)
    (if (executable-find "zsh")
        (setq shell-file-name "zsh"
              explicit-shell-file-name "zsh")
      (message "Warning! zsh not found!")))

;;;; (emacs) 47.1 The Package Menu Buffer
;;;;; emacs-lisp/packages.el
;; (bind-key "C-c P" 'list-packages)

;;;;; emacs-lisp/package-x.el
;; package-upload-buffer は tarファイルでも指定可能。
;; 自作ツールはパッケージ化すると、autoloadなどが自動化される。

;; +-----------------+  tar command で生成      package-upload-file
;; | hogehoge.el     |  +------------------+   +--------------------------+
;; | hogehoge-pkg.el +->| hogehoge-VER.tar +-->| package-base-directory/  |
;; | 関連ファイル    |  +---------+--------+   |   hogehoge-VER.tar       |
;; | README (descr.) |            |            +------------+-------------+
;; +-----------------+            |         package-install | autoload 生成
;;                                |                         v バイトコンパイル
;;                                |            +--------------------------+
;;                                |            | package-base-directory/  |
;;                                +----------->|   hogehoge-autoloades.el |
;;                      package-install-file   |   hogehoge.elc           |
;;                                             |   misc files...          |
;;                                             +--------------------------+
;(use-package package-x
;  :commands (package-upload-file package-upload-buffer)
;  :config
;  (setq package-archive-upload-base (locate-user-emacs-file "local-packages")))

;;;; (emacs) 48 Customization
;;;;; (emacs) 48.1.4 Saving Customizations
(setq custom-file (locate-user-emacs-file "custom.el"))
;;;;; (emacs) 48.1.7 Custom Themes
;; テーマを読み込む際にいちいち聞かない。
(setq custom-safe-themes t)

(defun tkw-delete-theme ()
  "現在のthemeを全て削除する。"
  (interactive)
  (mapc 'disable-theme custom-enabled-themes))

(defvar doremi-custom-themes) ; from doremi-cmds.el

(defun tkw-rotate-theme-to (target)
  (require 'doremi-cmd)
  (if (null target) (tkw-delete-theme)
    (when (member target doremi-custom-themes)
      ;;(setq doremi-custom-themes (tkw-rotate-to doremi-custom-themes target))
      (callf tkw-rotate-to doremi-custom-themes target)
      (tkw-delete-theme)
      (let ((custom-safe-themes t))
        (load-theme (car doremi-custom-themes))))))

;; My favorite themes
;; C-x M-?

;; 利用するthemeの条件
;; org-mode の bullet がきちんと表示されること
;; - alect-light*, ample

(defvar tkw-theme-disabled
  '(alect-light
    alect-light-alt))

(defvar tkw-theme-shortcut
  '((?a . adwaita)
    (?b . base16-railscasts)
    (?c . colorsarenice-light)
    (?d . dichromacy)
    (?e . espresso)
    (?f . fogus)
    (?g . gandalf)
    (?h . hemisu-light)
    (?i . inkpot)
    (?j . jonadabian)
    (?k . kingsajz)
    (?l . light-blue)
    (?m . moe-light)
    (?n . nzenburn)
    (?o . occidental)
    (?p . pastels-on-dark)
    (?q . qsimpleq)
    (?r . reverse)
    (?s . smyx)
    (?t . tango)
    (?u . underwater)
    (?v . vim-colors)
    (?w . wombat)
    (?x . xemacs)
    (?y . xp)
    (?z . zenburn)))

;; M-r M-<key> で一発テーマ選択。
(dolist (pair tkw-theme-shortcut)
  (eval `(bind-key ,(format "M-%c" (car pair)) 'tkw-select-theme tkw-rotate-map)))

(defun tkw-select-theme ()
  "Select Theme by shortcut key."
  (interactive)
  (let* ((last-char (tkw-this-command-char))
         (target (cdr (assoc last-char tkw-theme-shortcut))))
    (tkw-rotate-theme-to target)))

;;;; (emacs) 50.1 If <DEL> Fails to Delete
(normal-erase-is-backspace-mode 1)

;;;; 8.1 Symbol Components
;; Symbol Cells
;; | symbol      | define | check   | set      | get             | remove       |
;; |-------------+--------+---------+----------+-----------------+--------------|
;; | name        |        |         | intern   | symbol-name     | unintern     |
;; | dynamic val | devar  | boundp  | set      | symbol-value    | makunbound   |
;; | lexical val |        |         | setq     | eval            |              |
;; | function    | defun  | fboundp | fset     | symbol-function | fmakeunbound |
;; | plist       |        |         | setplist | symbol-plist    |              |
;; | property    |        |         | put      | get             | cl-remf      |
;; |             |        |         |          | function-get    |              |
;; ※ defvar の代わりに defconst, defcustom もありうる。
;; ※ defun の代わりに defsubst や defalias もありうる。

;;;; 9 Evaluation
(bind-key "M-;" 'eval-region)
(setq max-lisp-eval-depth 40000) ;; 600

;;;; 11 Variables
;;;;; 11.3 Local Variables
(setq max-specpdl-size 100000) ;; 1300

;;;;; 11.11 File Local Variables
;; ファイルローカル変数を許可するか。
;; (setq enable-local-variables t)

;;;; 15.3 Library Search
;; load-path の設定
(defun add-to-load-path (dir)
  "新しい DIR を load-path に追加する。"
  (let ((default-directory dir))
    (load (expand-file-name "subdirs.el") t t t))
  (pushnew dir load-path :test 'equal))

;; パッケージの load-pathの前に 個人のsite-lisp のpathを設定する。
(add-to-load-path (locate-user-emacs-file "site-lisp"))
;; 本来なら上記で、load-pathの先頭に site-lisp 以下のディレクトリが入る
;; はずだが、なぜか入らないので以下のように無理やり順序を入れ替える。
(let (site-lisp non-site-lisp)
  (dolist (path load-path)
    (if (string-match "/.emacs.d/site-lisp/" path) (push path site-lisp)
      (push path non-site-lisp)))
  (setq load-path (nconc (nreverse site-lisp) (nreverse non-site-lisp))))
;; load-path の理想的な順番
;; ( <site-lisp 以下> <elpa 関係> <標準elisp> )...


;;;; 18 Debugging
;;;;; 18.1.1 Entering the Debugger on an Error
(setq debug-on-error t)
(bind-key "C-c E" 'toggle-debug-on-error)

;;;;; 18.1.2 Debugging Infinite Loops
;;(setq debug-on-quit t)
(bind-key "C-c Q" 'toggle-debug-on-quit)

;;;;; 18.1.3 Entering the Debugger on a Function Call (emacs-lisp/debug.el)
(bind-key "C-c D" 'debug-on-entry)
(bind-key "C-c C" 'cancel-debug-on-entry)
;; C-M-c は exit-recursive-edit

;;;;; 18.2 Edebug (emacs-lisp/edebug.el)
;; デバッグ時は以下をtにすると、C-M-x で評価した関数にデバッガが入る。
;; 個別にデバッグ設定するなら、C-u C-M-x でも良い。
(setq edebug-all-defs nil)

;;;;; 18.5 Profiling
;; cf. profile.el
(bind-key "C-c M-P s" 'profiler-start)
(bind-key "C-c M-P t" 'profiler-stop)
(bind-key "C-c M-P r" 'profiler-report)

;;;; 19.6 Variables Affecting Output
(setq print-circle t)
(setq eval-expression-print-length nil) ; default 12.
(setq eval-expression-print-level nil)  ; default 4

;;;; 20 Minibuffers
;;;;; 20.6.1 Basic Completion Functions
;; completion の際に 大文字・小文字の違いは無視する。
(setq completion-ignore-case t)

;;;;; 20.7 Yes-or-No Queries
(fset 'yes-or-no-p 'y-or-n-p)

;;;;; 20.13 Recursive Minibuffers
;; ミニバッファ内で再帰的編集を許す
(setq enable-recursive-minibuffers t)

;;;; 21 Command Loop
;; キー入力の処理順番
;; イベント入力 (read-event)
;; → キー入力 (read-key-sequence)
;; Translation of Input Events [[info:elisp#Event Mod]]
;; - keyboard-translate-table
;; - input-method-function
;; Translation of Keyboard Input [[info:elisp#Translation Keymaps]]
;; - input-decode-map
;; - local-function-key-map (parent: function-key-map)
;; - key-translation-map (iso-transl.el)
;; Searching the Active Keymaps :: [[info:elisp#Searching Keymaps]]
;;   キー入力を、コマンドに変換する
;; - overriding-terminal-local-map :: 滅多に使われない。
;; - overriding-local-map :: これより下位のkeymapを全て無効にするので実用性はない。
;; - (keymap char property at point)
;; - emulation-mode-map-alists :: 最優先キーにはこれを利用する
;; - minor-mode-overriding-map-alist :: Major Mode が Minor Mode をオーバーライドするのに使用。
;; - minor-mode-map-alist
;; - (Keymap text property at point)
;; - (current local-map) :: Major Mode
;; - current global-map :: Global Keymap

;; Altキーの代替として、M-a をprefixとして使用する。
;; 旧 M-a (backward-sentence は M-A に割り当てる)
(bind-key "M-a" 'event-apply-alt-modifier function-key-map)
(bind-key "M-A" 'backward-sentence)

;;;;; 21.8.3 Modifying and Translating Input Events
;; C-h を DEL にして、C-z を C-h にする。
(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
(keyboard-translate ?\C-z ?\C-h)  ; translate `C-z' to `C-h'.

;;;; 23 Major and Minor Modes
;; *scratch* は text-mode にするが、一旦、lisp-interactive-mode になったらそちらをデフォルトにする。
(setq initial-major-mode 'fundamental-mode)
;;(add-hook 'lisp-interaction-mode-hook
;;          (lambda () (setq initial-major-mode 'lisp-interaction-mode)))

;;;;; 23.6 Font Lock Mode
;; * font-lock-defaults の書式
;;   (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD
;;           [SYNTAX-ALIST [SYNTAX-BEGIN OTHER-VARS…]]]])
;;   KEYWORDS ::= SYMBOL(font-lock-keywords) | SYMBOL(function) | LIST (font-lock-keywords)

;;;;; 23.7 Automatic Indentation of Code
;; インデントエンジン
;; - 参照 :: emacs-lisp/smie.el
;; - 参考 :: progmodes/modula2, progmodes/octave-mod, progmodes/prolog,
;;           progmodes/ruby-mode, progmodes/sh-script, textmodes/css-mode,
;;           tuareg.el
;;           https://github.com/deactivated/sql-smie-mode/blob/master/sql-smie-mode.el
;; * 基本構成
;;   文法解析・字句解析・インデント計算関数（引数：字句と文脈）
;;   (smie-setup XYZ-smie-grammar #'XYZ-smie-rules
;;               :forward-token   #'XYZ-smie--forward-token
;;               :backward-token  #'XYZ-smie--backward-token)
;; |------------------------+----------------------------|
;; | 設定パラメータ         | デフォルト値               |
;; |------------------------+----------------------------|
;; | forward-sexp-function  | smie-forward-sexp-command  |
;; | backward-sexp-function | smie-backward-sexp-command |
;; |------------------------+----------------------------|
;; * 演算子順位構文解析（ドラゴンブック4.6参照）
;;
;;        smie-bnf->prec2            smie-prec2->grammar
;;   BNF (BNF文法) ----> Prec2 ---+---> Prec2 ----> Grammar
;;                                |
;;                                | smie-merge-prec2s
;;                                |
;;   Prec (順位) ------> Prec2 ---+
;;         smie-precs->prec2
;;
;; ** 文法からの優先順位の決定方法
;;    a → bXc
;;    b → dYe の場合は、X⋗Y が自動的に決まる。
;;    または
;;    a -> bXc or dYe として、別に X⋗Y を指定する。
;;    ※ Precは、smie-bnf->prec2の第二以降の引数としても指定可能。
;;      その場合は、必要な優先順位を１引数に入れると、複数の引数間の優先順は
;;      SMIE が自動的に決定する。
;;      e.g. a=b+c*d^e;f=g-h/i の場合、`;' ⋗ `=' ⋗ `^' ⋗ `*' ≐ `/' ⋗ `+' ≐ `-' の優先度。
;; - 終端記号は opener / closer / neither になり、closer に対しては最外殻を除き、
;;   (:before . opener) がコールバック関数に渡され、それでインデント量を計算する。
;; - syntax-tableのコメントや括弧は自動的に文法として認識される。
;; - :list-intro がきた場合は、arg の後ろに　expression のリストがくる場合は t を返す。

;; 特定のemacsにおいて下記変数の未定義エラーが出るので念の為。
;;(with-eval-after-load 'smie
;;  (defvar smie--token)
;;  (defvar smie--after)
;;  (defvar smie--parent))

;;;; 25 Files
(setq delete-by-moving-to-trash nil) ; <undocumented>
(defun tkw-toggle-delete-by-moving-to-trash ()
  "Toggle 'delete-by-moving-to-trash'."
  (interactive)
  (message "delete-by-moving-to-trash=%s"
           (setq delete-by-moving-to-trash
                 (null delete-by-moving-to-trash))))

;;;; 26 Backups and Auto-Saving
(setq auto-save-timeout 30
      auto-save-interval 500)

;;;; 27 Buffers
(bind-key "M-l" 'bury-buffer)

;;;; 28 Windows
;;;;; 28.11 Switching to a Buffer in a Window
(bind-key "C-x M-B" 'pop-to-buffer)

;;;;; 28.14 Additional Options for Displaying Buffers
(setq split-window-preferred-function 'split-window-sensibly)
(setq split-height-threshold 80)
(setq split-width-threshold 160)

;;;;; 28.20 Textual Scrolling
(setq next-screen-context-lines 3
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum ; 4
      scroll-margin 4
      scroll-step 1)
(setq scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0)

;;;;; 28.9 Cyclic Ordering of Windows
(defun tkw-other-window ()
  "ウィンドウが1つしかない場合は、過去のウィンドウ配置に戻るか、左
右・上下のいずれかに分割する。"
  (interactive)
  (when (one-window-p)
    (if (functionp 'winhist-backward) (call-interactively 'winhist-backward)
      (if (< (window-width) 140)
          (split-window-vertically)
        (split-window-horizontally))))
  (other-window 1))

;; "M-o" → "M-O" へ入れ替え。
;; (bind-key "M-O" (lookup-key global-map (kbd "M-o")))
(bind-key* "M-o" 'tkw-other-window) ; ggtags.el の navigation/facemenu-keymap と衝突。
(bind-key "M-O" 'facemenu-keymap)

;;;; 29 Frames
;;;;; 29.3.2 Initial Frame Parameters
(setq default-frame-alist
      '(;; Colors
        (background-color . "#102020")
        (foreground-color . "Wheat")   ; "#d0e0f0"
        (cursor-color . "#e0e0e0")
        (mouse-color . "Orchid")
        (pointer-color . "Orchid")
        (background-mode . dark)
        ;; Frae Position and Size
        (left . 0)
        (top . 0)
        (height . 45)
        (width . 175)
        ;; Misc
        (line-spacing . 0)
        (scroll-bar-width . 12)
        (vertical-scroll-bars . left)
        (internal-border-width . 0)
        ;;
        ;; (font . "Inconsolata-12")
        ))

;; イニシアルフレーム設定　（デフォルトフレームと異なる場合に設定）
;; (setq initial-frame-alist default-frame-alist)

;;;;; 29.3 Access to Frame Parameters
(modify-frame-parameters nil default-frame-alist)

;;;;; 29.5 Frame Titles
(setq frame-title-format "%b")

;;;; 30 Positions
(defun tkw-next-line-5 ()     (interactive) (forward-line 5))
(bind-key "C-S-n" 'tkw-next-line-5)
(defun tkw-previous-line-5 () (interactive) (forward-line -5))
(bind-key "C-S-p" 'tkw-previous-line-5)

;;;; 32 Text
;;;;; 32.6 Deleting Text
(bind-key "C-c e" 'erase-buffer)

;;;;; 32.4 Inserting Text
;; ショートカットで文字入力
(defvar tkw-char-keys
  '((?z . ?从)))

(dolist (pair tkw-char-keys)
  (eval `(bind-key ,(format "C-c %c" (car pair)) 'tkw-insert-char)))

(defun tkw-insert-char ()
  "Insert IDS char."
  (interactive)
  (let ((last-char (tkw-this-command-char)))
    (insert (cdr (assoc last-char tkw-char-keys)))))

;;;;; 32.5 User-Level Insertion Commands
;; overwriteは危険なので警告を出す。
(put 'overwrite-mode 'disabled t)

;;;;; 32.8.5 Low-Level Kill Ring
;; (setq interprogram-cut-function x-select-text)

;;;;; 32.11 Filling
(bind-key "M-Q" (command (fill-paragraph 1))) ; fill with justification

;;;;; 32.12 Margins for Filling
(setq-default fill-column 70) ; 標準は70だが、やや少なめが見やすい。

;;;;; 32.19.6 Stickiness of Text Properties
;; (add-to-list 'text-property-default-nonsticky '(read-only . t))

;;;; 33 Non-ASCII Characters
;;;;; 33.6 Character Properties
;; East Asian Ambiguous
;; - Adobe-Japan1 Width Information
;;   Proportional 1-230, 9354-9737, 15449-15975, 20317-20426
;;   Half-width 231-632, 8718, 8719, 12063-12087
;;   Third-width 9758-9778
;;   Quarter-width 9738-9757
;;   Others :: Full-width
(defun set-east-asian-ambiguous-width (width)
  (while (char-table-parent char-width-table)
    ;;(setq char-width-table (char-table-parent char-width-table))
    (callf char-table-parent char-width-table))
  (let ((table (make-char-table nil)))
    (dolist
        (range
         '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
           (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
           #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
           (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
           (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
           #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
           (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
           (#x0148 . #x014B) #x014D (#x0152 . #x0153)
           (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
           #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
           (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
           #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
           (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
           (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
           (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
           (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
           #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
           #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
           #x212B (#x2153 . #x2154) (#x215B . #x215E)
           (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
           (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
           (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
           #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
           (#x2227 . #x222C) #x222E (#x2234 . #x2237)
           (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
           (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
           (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
           #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
           (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
           (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
           (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
           (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
           (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
           (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
           #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
           (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
           (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
           #xFFFD))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))
(set-east-asian-ambiguous-width 2)

;;;;; 33.7 Character Sets
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
          'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)

;;;;; 33.10.5 Default Coding Systems
(setq default-process-coding-system
      (case system-type ('windows-nt '(cp932 . cp932))
                        ('darwin (require 'ucs-normalize) '(utf-8-hfs . utf-8))
                        (t '(undecided . utf-8))))
(setq selection-coding-system
      (case system-type ('windows-nt 'cp932)
                        ('darwin 'utf-8)
                        (t nil)))
;; decode-translation-table の設定

;;;;; 33.10.6 Specifying a Coding System for One Operation
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;;;;; 33.12 Locales
(setq system-time-locale "C")

;;;;; 33.10.8 Terminal I/O Encoding
;; PuTTY 用の terminal-coding-system の設定
;; charset と coding-system の優先度設定
(apply 'define-coding-system 'utf-8-for-putty
   "UTF-8 (translate jis to cp932)"
   :encode-translation-table
   (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
   (coding-system-plist 'utf-8))
(set-terminal-coding-system 'utf-8-for-putty)

;;;; 35 Syntax Tables
;;;;; 35.2 Syntax Descriptors
;; - 1 :: syntax class designator
;; - 2 :: matching character (space if no such character)
;; - 3-4 :: syntax flags (35.2.2 Syntax Flags)

;;;; 36 Abbrevs and Abbrev Expansion
;; abbrev.el
;; - C-x a g :: add-global-abbrev
;; - C-x a l :: add-local-abbrev
;; - define-mode-abbrev
;; - define-global-abbrev
;; - expand-abbrev
(bind-key "A-'" 'expand-abbrev)

;;;; 37 Processes
;; PATHがない環境から起動された場合に備えて、exec-path に "gnupg" 等の
;; 必要なソフトがインストールされているパスを追加しておく。
;; → sync-env で PATH環境変数を一元管理するので不要。
;;(loop for directory in
;;      '(
;;        "/usr/bin"
;;        "/opt/local/bin"
;;        "/usr/local/bin"
;;        ;; Windows用。
;;        "c:/Program Files/Haskell Platform/2014.2.0.0/bin"
;;        "c:/Program Files/Haskell Platform/2014.2.0.0/lib/extralibs/bin"
;;        "c:/cygwin64/bin/"
;;        "c:/w32tex/texinst2010/bin"
;;        "c:/Program Files/Ruby-1.9.2/bin"
;;        "d:/Users/taichi/AppData/Roaming/cabal/bin"
;;        )
;;      if (file-directory-p directory)
;;      do (add-to-list 'exec-path directory))

;;;; 38 Emacs Display
;;;;; 38.3 Truncation
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; 長い行の文字数をキャッシュする。
;; cache-long-scans (Emacs 24.4) cache-long-line-scans (Emacs 24.3)
(defvar cache-long-scans t)
(defvar cache-long-line-scans t)

;;;;; 38.4 The Echo Area
;; 38.4.3 Logging Messages in ‘*Messages*’
(setq message-log-max 8000)             ; default 50

;; 38.4.4 Echo Area Customization
(setq echo-keystrokes 0.01)

;;;;; 38.11 Line Height
(setq-default line-spacing 1)           ; 行間を2ピクセルあける

;;;;; 38.12 Faces
;;;;; 38.12.11 Fontsets
;; フォントセットの設定
;; SPECS = ((target . fonts) (target . fonts) ...)
;; TARGET = t ← default for all, nil ← all
;; 最初から最後に向かってつなげていく。
;; TARGETのスクリプト名は、international/characters.el を参照。
;; fonts の最初の要素のフォントを利用する。

;;;;; 38.12.12 Low-Level Font Representation

;; FAMILY-NAME
;; OPENED-NAME ... XLFD Name
;; FULL-NAME
;;                                     +-------+
;;  font-info                          |bufffer|
;;  font-family-list                   +---+---+
;;      |                           font-at|    composition-get-gstring
;;      v                                  v    font-shape-gstring
;; +---------+     +-----------+      +-----------+      +-------+
;; |font-spec+---->|font-entity+----->|font-object+----->|GSTRING|
;; +---------+     +-----------+      +----+------+      +-------+
;;         find-font           open-font   |     font-get-glyphs
;;         list-fonts                      |             +------+
;;                                         +------------>|glyphs|
;;                                         |             +------+
;;                                         |             +---------+
;;                                         +------------>+font-info|
;;                                         | query-font  +---------+
;;                                         |             +----------+
;;                                         +------------>|variations|
;;                                 font-variation-glyphs +----------+

;; - font-spec parameters
;; |---------+------------------------------------------------------------|
;; | :family |                                                            |
;; | :width  | `ultra-condensed', `extra-condensed', `condensed',         |
;; |         | `semi-condensed', `normal', `semi-expanded', `expanded',   |
;; |         | `extra-expanded', or `ultra-expanded'                      |
;; | :weight | `ultra-bold', `extra-bold', `bold', `semi-bold', `normal', |
;; |         | `semi-light', `light', `extra-light', `ultra-light'        |
;; | :slant  | `italic', `oblique', `normal', `reverse-italic',           |
;; |         | `reverse-oblique'.                                         |
;; |---------+------------------------------------------------------------|

;; - Functions with font-spec/entity/object as arguments

;;   fontp/font-info/font-get/font-face-attributes/font-put/font-xlfd-name
;;   font-match-p

;; - For debugging

;;   font-drive-otf/font-otf-alternates/draw-string

;; - Font Shaping by font Backend

;;   font-shape-gstring

;; - Examples

;;   (font-info (font-spec :family "Hiragino Kaku Gothic ProN"))
;;   (setq entity (find-font (font-spec :family "Hiragino Kaku Gothic ProN")))
;;   (setq object (open-font entity))
;;   (font-face-attributes object)
;;   (font-get-glyphs object 0 6 "漢字と日本語")
;;   (font-otf-alternates object ?漢 'aalt)
;;   (font-variation-glyphs object ?漢)
;;   (font-shape-gstring)
;;   (font-get-glyphs object 0 3 [?a ?漢 #x800])

(defun tkw-font-object-coverage (font-object)
  "指定された font-object でカバーされる文字の一覧を取得する。SIPまでをチェック。"
  (unless (fontp font-object 'font-object)
    (error "not font object! %s" font-object))
  (let* ((char-list (make-vector 196608 nil))
         glyph-list coverage)
    (dotimes (i 196608) (aset char-list i i))
    (setq glyph-list (font-get-glyphs font-object 0 196608 char-list))
    (dotimes (i 196608)
      (when (aref glyph-list i)
        (callf2 cons i coverage)))
    (nreverse coverage)))
;; (setq coverage (tkw-font-object-coverage object))


;;;;; 38.13 Fringes
;; 38.13.2 Fringe Indicators
;; 空行をフリンジに表示
(setq-default indicate-empty-lines t)

;;;;; 38.20 Character Display
(setq-default tab-width 8)              ; set tab width
;; (setq tab-stop-list (cons 24 (number-sequence 32 120 8)))

;;;;; 38.21 Beeping
(setq visible-bell t)

;;;;; 38.23 Bidirectional Display
(setq-default bidi-display-reordering t
              bidi-paragraph-direction (quote left-to-right))

;;;; 39 Operating System Interface
;;;;; 39.1.1 Summary: Sequence of Actions at Startup
;;(setq inhibit-default-init t)
(setq inhibit-startup-message t)
;;(setq inhibit-startup-echo-area-message -1)

;;;;; 39.1.2 The Init File
;; `default.el' があっても読み込まない。（site-start.elは読み込む。）

;;;;; 39.3 Operating System Environment
(when (= (length user-mail-address) 0)
  (setq user-mail-address (or (getenv "EMAIL") (rot13-string "xnjnongn.gnvpuv@tznvy.pbz"))))
(setq mail-host-address (replace-regexp-in-string "^.+@" "" user-mail-address))

;;;;; 39.4 User Identification
(setq user-full-name (string #x5ddd #x5e61 #x20 #x592a #x4e00))
(defvar user-latin-name (rot13-string "Gnvpuv XNJNONGN"))

;;;; E.3. Garbage Collection
;; (setq gc-cons-threshold 8000000)

;;;; (undocumented) fontset.c
;;  Font Vertical Centering
(when (equal system-type 'windows-nt)
  (setq vertical-centering-font-regexp ".*"))

;;;; (undocumented) composite.c
;; Character Composition

;; | composite.c                        |    | composite.el                  |
;; |------------------------------------+----+-------------------------------|
;; | auto-composition-mode (v)          | <- | auto-composition-mode (f)     |
;; |------------------------------------+----+-------------------------------|
;; | <compose-chars-after-function>     | -> | compose-chars-after           |
;; |                                    |    | -> composition-function-table |
;; |                                    |    | 　-> (funcall func)           |
;; |------------------------------------+----+-------------------------------|
;; | <find_automatic_composition>       |    |                               |
;; | <composition_reseat_it>            |    |                               |
;; | 　-> composition-function-table    |    |                               |
;; | 　　-> autocmp_chars               |    |                               |
;; | 　　　-> auto-composition-function | -> | auto-compose-chars            |
;; | 　　composition-get-gstring        | <- |                               |
;; | 　　font-shape-gstring (font.c)    | <- |                               |
;; |------------------------------------+----+-------------------------------|
;; | find-composition-internal          | <- | <find-composition>            |
;; | -> find_automatic_composition      |    |                               |
;; | 　-> composition-function-table    |    |                               |
;; | 　.....                            |    |                               |

;;       composition-get-gstring       font-shape-gstring
;; +------------+       +-----------------+       +-----------------+
;; |STRING      +------>|GSTRING (glyphs) +------>|GSTRING (shaped) |
;; +------------+       +-----------------+       +-----------------+
;;                              ^
;;                              |
;;                       +------+------+
;;                       | font-object |
;;                       +-------------+

;; - Example
;;   (setq object (open-font (find-font (font-spec :family "Devanagari Sangam MN"))))
;;   (query-font object)
;;   (font-get-glyphs object 0 6 "हिन्दी")
;;   (font-shape-gstring (composition-get-gstring 0 6 object "हिन्दी"))
;;   (font-get object :otf)

;; - manual composition
;;   (insert (compose-string "." 0 1 '(?+ (tc . bc) ?o)))
;;   (insert (compose-string "." 0 1 '(?+ (cc . cc) ?o)))


;;;; (undocumented) coding.c
;; cp932エンコード時の表示を「P」とする (from GnuPack)
;; 全角チルダ/波ダッシュをWindowsスタイルにする
(let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
  (mapc
   (lambda (coding-system)
     (coding-system-put coding-system :decode-translation-table table)
     (coding-system-put coding-system :encode-translation-table table))
   '(utf-8 cp932 utf-16le)))
(coding-system-put 'euc-jp :decode-translation-table ; debug
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'iso-2022-jp :decode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'utf-8 :decode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
;; encode-translation-table の設定
;; 原因は不明だが、これを設定すると Mac でのみ、eblook が動かなくなる？
;; （ただし、init.el 読み込み後に設定すると動作する。）
;;(coding-system-put 'euc-jp :encode-translation-table ;
;;           (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp :encode-translation-table
           (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'cp932 :encode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'utf-8 :encode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

(setq default-file-name-coding-system
      (cond ((eq system-type 'darwin) 'utf-8-hfs)
            ((eq window-system 'w32)  'cp932)
            (t 'utf-8)))

;;;;; * database architecture *

;; | kind        | key     | create            | get          | set                  |
;; |-------------+---------+-------------------+--------------+----------------------|
;; | cons        | -       | cons              | car/cdr      | setcar/setcdr        |
;; | list        | integer | list              | elt          | setf elt             |
;; | vector      | integer | vector            | aref         | aset                 |
;; | string      | integer | string            | aref         | aset                 |
;; | char-table  | char    | make-char-table   | aref         | aset                 |
;; |             |         |                   |              | set-char-table-range |
;; | bool-vector | integer | make-bool-vector  | aref         | aset                 |
;; | ring        | integer | make-ring         | ring-ref     | ring-insert          |
;; | keymap      | keystr  | make-keymap       | lookup-key   | define-key           |
;; | fontset     | char    | new-fontset       | fontset-font | set-fontset-font     |
;; | buffer      | int     | get-buffer-create | char-after   | insert               |
;; | plist       | symbol  | (key val...)      | plist-get    | plist-put            |
;; | alist       | any     | ((key . val) ...) | alist-get*   | setf alist-get       |
;; | obarry      | string  | make-vector 1511  | intern-soft  | set intern           |
;; | hash        | any     | make-hash-table   | gethash      | puthash              |
;; | struct XYZ  | YYY     | make-XXX          | XXX-YYY      | setf XXX-YYY         |
;; | class XYZ   | YYY     | make-instance     | oref         | oset                 |
;; | avl-tree    | any     | avl-tree-create   |              | avl-tree-enter       |
;; | heap        | integer | make-heap         | heap-add     | heap-modify          |
;; | trie        | string  | make-trie         | trie-lookup  | trie-insert          |
;; | dict-tree   | string  | dictree-create    | dictree-look | dictree-insert       |
;; | queue | ? | make-queue | queue-dequeue | queue-enqueue |
;;
;; 特殊 :  tNFA



;; | kind        | enumerate      | type-check    | member-check      | delete            |
;; |-------------+----------------+---------------+-------------------+-------------------|
;; | cons        |                | consp         |                   |                   |
;; | list        | mapcar         | listp         | member            | remove/remq       |
;; | vector      | mapcar         | vectorp       |                   | X                 |
;; | string      | mapcar         | stringp       | string-match      | X                 |
;; | char-table  | map-char-table | char-table-p  |                   | X                 |
;; | bool-vector | mapcar         | bool-vector-p |                   | X                 |
;; | ring        | ring-elements  | ring-p        | ring-member       | ring-remove       |
;; | keymap      | map-keymap     | keymapp       |                   |                   |
;; | fontset     | fontset-list   |               | query-fontset     |                   |
;; | buffer      | while search-* | bufferp       |                   |                   |
;; | plist       | while cddr     |               | plist-member      |                   |
;; | alist       | dolist         |               | assoc             | assoc-delete-all* |
;; |             |                |               | assq              | assq-delete-all   |
;; |             |                |               |                   | rassq-delete-all  |
;; | obarry      | mapatoms       |               |                   |                   |
;; | hash        | maphash        | hash-table-p  |                   | remhash           |
;; | struct XYZ  |                | XXXX-p        |                   |                   |
;; | class XYZ   |                | object-p      |                   | ???               |
;; | avl-tree    | avl-tree-map   | avl-tree-p    | avl-tree-member   | avl-tree-delete   |
;; |             | avl-tree-mapf  |               | avl-tree-member-p |                   |
;; | heap        |                | heap-p        |                   |                   |
;; | trie        |                | trie-p        | trie-complete     |                   |
;; | dict-tree   | dictree-mapcar |               | dictree-complete  |                   |


;; | kind        | size              | value | order    | setf | misc
;; |-------------+-------------------+-------+----------+------|-
;; | cons        | 2                 | any   | o(1)     | O    |
;; | list        | length (variable) | any   | o(n)     | O    |
;; | vector      | length (fixed)    | any   | o(1)     | O    |
;; | string      | length (variable) | char  | o(n)     | O    |
;; | char-table  | length            | any   | o(log n) | O    |
;; | bool-vector | length            | t/nil | o(1)     | X    |
;; | ring        | ring-size         | any   | o(n)     | X    | ring-copy
;; | keymap      |                   |       |          |      |
;; | fontset     |                   |       |          |      |
;; | buffer      | (point-max)       |       |          |      |
;; | plist       | (/ length 2)      | any   | o(n)     | O    |
;; | alist       | length            | any   | o(n)     | O    |
;; | obarry      |                   | any   | o(1)     |      |
;; | hash        | hash-table-size   | any   | o(1)     |      |
;; | struct XYZ  | (fixed)           | any   |          |      |
;; | class XYZ   | fixed             | any   |          |      |
;; | avl-tree    | avl-tree-size     | any   | o(log n) | O    |
;; | heap        | length (variable) | any   | o(log n) |      |
;; | trie        | length (variable) | any   | o(log n) |      |
;; | dict-tree   |                   |       |          |      |

;; - `char-table' and `class' can have parent.
;; - `char-table' can be used with `get-char-code-property' function.
;; - `obarray' can be used by a symbol that is `intern'ed to specific obarray.
;; - `dict-tree' can output file with `dictree-save/write'.
;; - `avl-tree' is very useful when sorting a large amount of randomly inserted data.

;; char-code-property は、まず unicode-property-table-internal を見た後で、
;; char-code-property-table の各文字のplist を確認する。


;;;; (undocumented) lread.c
;; Lisp Reader syntax (in order of 'read1()')
;; - List :: e.g. (1 2 3)
;; - Vector :: e.g. [a b c]
;; - Hashtable :: e.g. #s(hash-table data (k1 v1 k2 v2 ...))
;; - Chartable :: e.g. #^[....], #^^[....]
;; - Bool Vectors :: e.g. #&"abc"
;; - Compiled Functions :: e.g. #[..]
;; - String with Text Properties :: e.g. #("aaa" 0 1 (k1 v1 k2 v2 ...))
;; - Skip Number of Chars :: e.g. #@234
;; - Skip Line (Unix Executables) :: #! comments ... (can appear anywhere in text.)
;; - Filename :: #$
;; - (cons func (cons charfun nil)) :: #'
;; - Uninterend Symbol :: :..   ("';()[]#`, は除く)
;; - Empty Symbol :: ##
;; - Radix Forms :: #12r100 → 12進数で100なので144
;; - Reader Forms :: e.g. '(#1="a" #1#)
;; - Number (#b010, #o234, #xaf)
;;   |-------------+--------|
;;   | binary      | #b1111 |
;;   | octal       | #o77   |
;;   | decimal     | 99     |
;;   | hexadecimal | #xff   |
;;   |-------------+--------|
;; - Comment :: e.g. ;.....
;; - Quote
;;   |-----------+-----+-----------|
;;   | quote     | 'a  | '(1 2 3)  |
;;   | backquote | `a  | `(1 2 3)  |
;;   | comma     | ,a  | ,(1 2 3)  |
;;   | comma-at  | ,@a | ,@(1 2 3) |
;;   | comma-dot | ,.a | ,.(1 2 3) |
;;   |-----------+-----+-----------|
;; - Character :: e.g. ?\a
;;   |-----+---------+----------------|
;;   | a   | #x07    |                |
;;   | b   | #x08    |                |
;;   | d   | #x7f    |                |
;;   | e   | #x27    |                |
;;   | f   | #x0c    |                |
;;   | n   | #x0a    |                |
;;   | r   | #x0d    |                |
;;   | t   | #x09    |                |
;;   | v   | #x0b    |                |
;;   | ' ' | #x20    | only Character |
;;   | M-  | Meta-   |   or 0x8000000 |
;;   | C-  | Control |   or 0x4000000 |
;;   | S-  | Shift-  |   or 0x2000000 |
;;   | H-  | Hyper-  |   or 0x1000000 |
;;   | s-  | super-  |   or 0x0800000 |
;;   | A-  | Alt-    |   or 0x0400000 |
;;   | u   | Unicode |        4-digit |
;;   | U   | Unicode |        8-digit |
;;   | ^   | control |                |
;;   |-----+---------+----------------|
;; - String :: e.g. "\a"
;; - Cons (a . b)
(setq load-prefer-newer t)

;;;; (undocumented) macterm.c
;; Macのみの設定。また、山本パッチのMacでは不要。
(defvar mac-option-modifier nil)
(defvar mac-option-key-is-meta nil)
(defvar mac-command-key-is-meta nil)
(defvar mac-command-modifier nil)

(when (equal (window-system) 'mac)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  ;;(setq mac-option-modifier nil)
  )

;;;; (undocumented) term.c
;; shell において、4m .. 等の ANSI Esc Seq が見える場合。
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
;; (setq system-uses-terminfo nil)

;;;; 各種警告の非表示
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)


;;; 標準ライブラリ
;; loadup.el に記述され、デフォルトでEmacsに読み込まれる
;; ライブラリに関する設定は、上の「標準設定」で行なう。
;;;; align.el
(with-eval-after-load 'align
  ;;(push 'jde-mode align-c++-modes)
  (set-variable 'align-indent-before-aligning t))

;;;; ansi-color.el (extension to comint.el)
;; SGR エスケープシーケンスをEmacsのFaceに解釈する。
(with-eval-after-load 'comint
  (ansi-color-for-comint-mode-on))
(setq ansi-color-names-vector
      ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2"
       "#333366" "#ccaa8f" "#f6f3e8"])

;;;; arc-mode.el (minor-mode)
(dolist (x '("\\.kmz\\'" "\\.odp\\'" "\\.otp\\'"))
  (pushnew (cons x 'no-conversion) auto-coding-alist :test 'equal)
  (pushnew (cons x 'archive-mode) auto-mode-alist :test 'equal))
(with-eval-after-load 'arc-mode
  (defun set-archive-file-name-coding-system (cs)
    "日本語ファイルの時は適宜この関数を呼び出してください。"
    (interactive "zCoding system for archived file name: ")
    (check-coding-system cs)
    ;; プログラム中で、強制的に変更されてしまうので、
    ;; file-name-coding-system を変更するしかない。あとで気をつけること。
    (set-variable 'archive-member-coding-system cs)))

;;;; autoinsert.el (global-minor-mode)
(eval-when-compile (require 'autoinsert))
;;(auto-insert-mode) ; モード型はライブラリを初期化時に読み込むので避ける。
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory (locate-user-emacs-file "insert"))
(setq auto-insert-query nil)
;; すでに設定済のテンプレより望ましいものがあればそれに置き換える。
;;(dolist (elem
;;         '(
;;           ;;("\\.html" . "template.html")))
;;           (latex-mode . "template.tex")))
;;  (push elem auto-insert-alist))
;;(defun tkw-auto-insert-yas-expand ()
;;  "`auto-insert' するテンプレートを `yasnippet' のスニペットと見做して展開する."
;;  (when (require 'yasnippet nil :no-error)
;;    (yas-expand-snippet (buffer-string) (point-min) (point-max))))
;; ~/.emacs.d/insert/emacs-lisp-mode.el 等にスニペットを入れておく。
;;(defadvice auto-insert (before auto-insert-update-template activate)
;;  "`auto-insert' 前にテンプレート一覧を更新する."
;;  (dolist (template (directory-files auto-insert-directory nil "-mode\\.[^~]+$"))
;;    (let ((mode (intern (file-name-sans-extension template))))
;;      (setq auto-insert-alist (assq-delete-all mode auto-insert-alist)
;;            auto-insert-alist (cons (cons mode
;;                                          (vector
;;                                           template 'tkw-auto-insert-yas-expand))
;;                                    auto-insert-alist)))))

;;;; autorevert.el (global-minor-mode)
;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;;;; avoid.el (minor-mode)
;; (if (eq window-system 'x) (mouse-avoidance-mode 'banish))

;;;; bookmark.el
;; 参照 - [[info:emacs#Bookmarks]]
(setq bookmark-use-annotations t)
(setq bookmark-automatically-show-annotations t)
;; 複数のブックマークを使うが、起動時はbookmarks.bmk で統一する。
(setq bookmark-default-file
      (locate-user-emacs-file "bookmarks/bookmarks.bmk"))

;;;; bs.el
;(bind-key (kbd "M-C-,") 'bs-cycle-previous)
;(bind-key (kbd "M-C-.") 'bs-cycle-next)

;;;; calc/calc.el
;; 初期設定ファイルは、 ~/.emacs.d/calc.el に入れる。
;; プログラムからの利用方法は以下を参照。
;; [[info:calc#Calling%20Calc%20from%20Your%20Programs]]
;; (setq calc-settings-file (locate-user-emacs-file "calc.el"))

;;;; calendar/calendar.el (application)
;; 休日を表示する
(with-eval-after-load 'calendar
  (set-variable 'calendar-mark-holidays-flag t)
  ;; ダイヤリーを表示する。 (~/.diary がないとエラーに)
  (set-variable 'diary-file (locate-user-emacs-file "diary"))
  (set-variable 'calendar-mark-diary-entries-flag t)
  (set-variable 'calendar-view-diary-initially-flag t)
  ;; 場所・タイムゾーンの設定
  ;; S (calendar-sunrise-sunset) … 日出・日没時間の計算
  ;; M (calendar-lunar-phases) … 月齢の計算
  (set-variable 'calendar-latitude 35.35)
  (set-variable 'calendar-longitude 139.44)
  (set-variable 'calendar-location-name "Tokyo, JP")
  (set-variable 'calendar-time-zone +540)
  (set-variable 'calendar-standard-time-zone-name "JST")
  (set-variable 'calendar-daylight-time-zone-name "JST")
  ;; 日付表示方法の設定
  ;; (calendar-european-date-display-form, calendar-american-date-display-form)
  (defvar calendar-iso-date-display-form)
  (set-variable 'calendar-date-display-form
                calendar-iso-date-display-form))

;;;; calendar/holidays.el
;; 祝日の管理
;; h (calendar-cursor-holidays) … 祝日の内容を確認
;; M-x list-holidays … 画面の祝日を表示

;;;; calendar/lunar.el
(with-eval-after-load 'lunar
  (set-variable 'lunar-phase-names
                '("新月" "上弦の半月" "満月" "下弦の半月")))

;;;; cedet/ede.el
;;(with-eval-after-load 'cc-mode
;;  (global-ede-mode 1))

;;;; cedet/semantic.el
;; - 参考 :: https://earthserver.com/Setting_up_a_C%2B%2B11_development_environment_on_Linux_with_Clang_and_Emacs
;;(with-eval-after-load 'cc-mode
;;  (semantic-mode 1))

(declare-function semanticdb-enable-gnu-global-databases "semantic" (mode &optional noerror))
(declare-function semantic-add-system-include "dep" (dir &optional mode))
(with-eval-after-load 'semantic
  ;; semantic では senator は必須だが require されていない場合があるため補完。
  (require 'semantic/senator)
  ;; submode の設定
  (setq
   semantic-default-submodes
   '(global-semanticdb-minor-mode      ; Maintain tag database.
     ;;
     ;; global-semantic-idle-scheduler-mode   ; Reparse buffer when idle.
     ;; global-semantic-idle-summary-mode     ; Show summary of tag at point.
     ;; global-semantic-idle-completions-mode ; Show completions when idle.
     global-semantic-decoration-mode       ; Additional tag decorations.
     global-semantic-highlight-func-mode   ; Highlight the current tag.
     global-semantic-stickyfunc-mode       ; Show current fun in header line.
     global-semantic-mru-bookmark-mode     ; Provide `switch-to-buffer'-like
                                           ; keybinding for tag names.
     global-semantic-idle-local-symbol-highlight-mode ; Highlight references
                                           ; of the symbol under point.
     ;; The following modes are more targeted at people who want to see
     ;; some internal information of the semantic parser in action:
     global-semantic-highlight-edits-mode ; Visualize incremental parser by
                                          ;  highlighting not-yet parsed changes.
     ;; 以下のモードはマクロで関数宣言しているとエラーと誤認して鬱陶しい
     ;; ので必要なときのみ on にする。
     ;; global-semantic-show-unmatched-syntax-mode ; Highlight unmatched lexical
                                          ;syntax tokens.
     global-semantic-show-parser-state-mode ; Display the parser cache state."
     ))

  ;; cc-mode では原則として GNU global を利用する。
  (semanticdb-enable-gnu-global-databases 'cc-mode)
  (semantic-add-system-include "/usr/include/GL" 'c++-mode)
  (semantic-add-system-include "/opt/local/include" 'c++-mode)
  (semantic-add-system-include "/usr/local/include" 'c++-mode)
  (semantic-add-system-include "/usr/include" 'c++-mode)
  (semantic-add-system-include "/usr/include/google" 'c++-mode))

;;(lazyload () "cc-mode"
;;  (add-hook
;;   'c-mode-common-hook
;;   (lambda ()
;;     (semantic-idle-scheduler-mode 1)
;;     (semantic-idle-completion-mode 1)
;;     (semantic-decoration-mode 1)
;;     (semantic-highlight-func-mode 1)
;;     (semantic-stickyfunc-mode 1)
;;     (semantic-mru-bookmark-mode 1)
;;     (semantic-idle-local-symbol-highlight-mode 1)
;;     (semantic-highlight-func-mode 1)
;;     (semantic-show-parser-state-mode 1)
;;     )))


;;;; cedet/semantic/complete.el
(with-eval-after-load 'semantic/complete
  ;; Try to make completions when not typing
  :config
  (set-variable 'semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-tooltip)
  (set-variable 'semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-tooltip))

;;;; comint.el
;; Emacsが、UIで外部プロセスと通信するときの基本モード
;; - 参照 :: http://www.masteringemacs.org/articles/2013/07/31/comint-writing-command-interpreter/
(with-eval-after-load 'comint
  ;; [WinNT] Ctrl-M を除去する。
  (when (equal system-type 'windows-nt)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m))
  (setq ;;comint-scroll-show-maximum-output t ; カーソルは常に画面の最も下に。
        comint-input-ignoredups t ; 履歴に重複は残さない
        comint-completion-autolist t ; タブで候補を自動補完
        comint-completion-addsuffix t ; ファイル名の後にスペースを追加
        comint-scroll-to-bottom-on-input 'this ; テキスト入力時に下にスクロール
        ;;comint-scroll-to-bottom-on-output nil ; tなら出力時に一番下にスクロール
        ;; zsh のヒストリのタイムスタンプを無視する。
        comint-input-ring-separator "\n\\(: [0-9]+:[0-9];\\)?"
        ;; ヒストリサイズは500の10倍にする。
        comint-input-ring-size 5000))

;;;; cmuscheme.el
;; C-x C-e → scheme-send-last-sexp

;;;; dabbrev.el
(with-eval-after-load 'dabbrev
  (set-variable 'dabbrev-abbrev-char-regexp "\\w\\|\\s_")
  (set-variable 'dabbrev-case-replace nil))

;;;; delsel.el
;; リージョンを文字入力で同時に削除。
;; 事故でうっかり削除がたまにあるため、Offにする。
;;(delete-selection-mode 1)

;;;; descr-text.el
(with-eval-after-load 'descr-text
  (set-variable 'describe-char-unidata-list
                '(name general-category canonical-combining-class
                  bidi-class decomposition decimal-digit-value digit-value
                  numeric-value mirrored old-name iso-10646-comment uppercase
                  lowercase titlecase)))

;;;; desktop.el
;; → session.el （自動的に前回終了時のファイルを開かない）に移行。
;; [[info:emacs#Saving Emacs Sessions]]
;; inhibit-default-init 変数を t にしてしまうので使用禁止。
(desktop-save-mode 1)
;;(add-hook 'kill-emacs-hook
;;          (lambda ()
;;            (desktop-truncate search-ring 3)
;;            (desktop-truncate regexp-search-ring 3)))

;;;; dired.el
(defvar dired-mode-map)
(defvar dired-actual-switches)
(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hook 'dired-mode-hook (lambda () (setenv "LANG" "C")))
  ;; diredバッファの自動更新
  (set-variable 'dired-auto-revert-buffer t)
  ;; diredのサイズ表示に Kbyte, Mbyte 等の単位を使う。
  ;; -h :: Kbyte, Mbyte 単位の表示
  (set-variable 'dired-listing-switches "-alh")
  ;; Diredでのコピー先ディレクトリの自動推定（２窓ファイラ的動作）
  (set-variable 'dired-dwim-target t)
  ;; 再帰的にコピー・削除
  (set-variable 'dired-recursive-copies 'always)
  (set-variable 'dired-recursive-deletes 'always)
  ;; Drag&Drop
  ;; (setq dired-dnd-protocol-alist nil)
  ;; GNU ls は、Mac では、
  ;; - sudo port install coreutils (gls)
  ;; - sudo port install coreutils +with_default_names (ls)
  ;; でインストール可能
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  ;; dired の sort を拡張する。
  ;; sorter.el のバグ修正・整理版。
  (defvar dired-sort-order '("" "t" "S" "X")
    "-t (時間) -X (拡張子) -S (サイズ) なし (アルファベット順) を切り替える。")
  (defvar dired-sort-order-position 0)
  (declare-function dired-sort-other "dired")
  (defun dired-rotate-sort ()
    "Rotate dired toggle sorting order by `dired-sort-order'"
    (interactive)
    (setq dired-sort-order-position
          (% (1+ dired-sort-order-position) (length dired-sort-order)))
    (setq dired-actual-switches
          (concat dired-listing-switches (elt dired-sort-order
                                            dired-sort-order-position)))
    (dired-sort-other dired-actual-switches))
  (bind-key "s" 'dired-rotate-sort dired-mode-map))

;; dired のバッファが氾濫しないように，ディレクトリを移動するだけなら
;; バッファを作らないようにする．
;;(defvar tkw-dired-before-buffer nil)
;;(defadvice dired-advertised-find-file
;;  (before kill-dired-buffer activate)
;;  (setq tkw-dired-before-buffer (current-buffer)))
;;(defadvice dired-advertised-find-file
;;  (after kill-dired-buffer-after activate)
;;  (when
;;      (and
;;       (eq major-mode 'dired-mode)
;;       (not (string= (buffer-name (current-buffer))
;;                     (buffer-name tkw-dired-before-buffer))))
;;    (kill-buffer tkw-dired-before-buffer)))
;;(defadvice dired-up-directory
;;  (before kill-up-dired-buffer activate)
;;  (setq tkw-dired-before-buffer (current-buffer)))
;;(defadvice dired-up-directory
;;  (after kill-up-dired-buffer-after activate)
;;  (when
;;      (and
;;       (eq major-mode 'dired-mode)
;;       (not (string= (buffer-name (current-buffer))
;;                     (buffer-name tkw-dired-before-buffer))))
;;    ;;(not (string-match "^[a-z]+:[/]$" (buffer-name tkw-dired-before-buffer))))
;;    (kill-buffer tkw-dired-before-buffer)))

;; Cygwin 環境では、diredのファイル名はutf-8のため、fopenと整合しない。
;;(when (file-executable-p "c:/cygwin/bin/ls.exe")
;;  (setq ls-lisp-use-insert-directory-program t)
;;  (setq insert-directory-program "c:/cygwin/bin/ls.exe"))

;;;; dired-aux.el
(defvar dired-compress-file-suffixes)
(with-eval-after-load 'dired-aux
  ;; atool を使い、多数の圧縮ファイルを閲覧可能にする。
  (when (executable-find "aunpack")
    (let ((dired-additional-compression-suffixes
           '(".7z" ".Z" ".a" ".ace" ".alz" ".arc" ".arj" ".bz" ".bz2" ".cab"
             ".cpio" ".deb" ".gz" ".jar" ".lha" ".lrz" ".lz" ".lzh" ".lzma"
             ".lzo" ".rar" ".rpm" ".rz" ".t7z" ".tZ" ".tar" ".tbz" ".tbz2"
             ".tgz" ".tlz" ".txz" ".tzo" ".war" ".xz" ".zip" ".epub")))
      (loop for suffix in dired-additional-compression-suffixes
            do (pushnew `(,(concat "\\" suffix "\\'") "" "aunpack")
                        dired-compress-file-suffixes)))))

;;;; dired-x.el
;; dired 拡張機能 :: dired-jump, dired-omit-mode
(autoload 'dired-omit-mode "dired-x")
;; (bind-key "C-x C-j" 'dired-jump)
(with-eval-after-load 'dired-x
  ;; dired-aux 機能の omit の一部キーバインドの無効化と代替キーバインドの設定
  (bind-key "C-M-o" 'dired-omit-mode dired-mode-map)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  ;; dired-omit-mode :: LaTeX等の作業ファイルを表示しない。
  ;; Office のワークドキュメント（~で始まる）を表示しない。
  (set-variable 'dired-omit-files ; dired-omit-mode で隠すファイル
                (concat "^~\\|^\\.?#\\|^\\.\\|^\\.\\.?$"
                        "\\|\\.aux$\\|\\.log$")))

;; shell-command-guesssing … "!" を押した時のシェルコマンドを予想
;; M-x dired-mark-extension … 特定の拡張子をマーク

;;;; doc-view.el
;; PDFビューア。ページをpng化してキャッシュするため、大量の画像を
;; 高速に見るときに便利。外部ツールを使うかを、適宜判断する。
(declare-function doc-view-next-page "doc-view" (&optional arg))
(declare-function doc-view-previous-page "doc-view" (&optional arg))
(defvar doc-view-mode-map)
(with-eval-after-load 'doc-view
  ;; 画面サイズを変更する度に rescale する。
  (set-variable 'doc-view-scale-internally nil)
  ;; 高速ページ移動
  (bind-key "M-n" (command (doc-view-next-page 5))      doc-view-mode-map)
  (bind-key "M-N" (command (doc-view-next-page 25))     doc-view-mode-map)
  (bind-key "M-p" (command (doc-view-previous-page 5))  doc-view-mode-map)
  (bind-key "M-P" (command (doc-view-previous-page 25)) doc-view-mode-map))

;;;; edmacro.el
;; | key | char |
;; |-----+------|
;; | NUL | \0   |
;; | RET | \r   |
;; | LFD | \n   |
;; | TAB | \t   |
;; | ESC | \e   |
;; | SPC | " "  |
;; | DEL | \177 |

;;;; electric.el
;; 改行と同時にインデントする。
;; Emacs 24.4 からデフォルトでオン。C-j で普通の改行。
;; (electric-indent-mode 1)

;;;; emacs-lisp/checkdoc.el
;; DocString の内容チェックを行う。
(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)
(with-eval-after-load 'checkdoc
  (bind-key "C-c k" 'checkdoc-minor-mode emacs-lisp-mode-map))

;;;; emacs-lisp/easy-mmode
;; define-minor-mode

;;;; emacs-lisp/eldoc.el
;; 重いので、明示的にOnにする以外はオフにしておく？
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook
          #'eldoc-mode) ;; eval-expression 中に eldocを表示。
(when (equal system-type 'darwin)
  (put 'eldoc-minor-mode-string 'risky-local-variable t)
  (setq eldoc-minor-mode-string
        (if (eq window-system 'mac) " 📚"
          (propertize "ed" 'face '(:foreground "purple" :background "yellow")))))

;;;; emacs-lisp/ert.el
;; [[info:ert#Top]]
;; プロジェクトとテストの書き方
;; - travis-ci :: Emacs 23 等他バージョンの環境の試験を仮想環境で実現
;; - cask

;;;; emacs-lisp/find-func.el
;; find-function-setup-keys でキーをセットアップするため、
;; 遅延ロードができない。
(find-function-setup-keys) ; C-x (4/5/null) (F/K/V)
(bind-key "C-x >" 'find-library)
(with-eval-after-load 'find-func
  (setq find-function-regexp
        (concat
         "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
\[^cgv\W]\\w+\\*?\\)\\|define-minor-mode\
\\|easy-mmode-define-global-mode\\|luna-define-generic\\)"
         find-function-space-re
         "\\('\\|\(quote \\)?%s\\(\\s-\\|$\\|\(\\|\)\\)")))

;;;; emacs-lisp/lisp.el
;; デフォルトファイルなので lazyload しない。
(setq emacs-lisp-docstring-fill-column 72) ; default = 65
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq lexical-binding t)
            (message "lexical-binding is set to `t'.")))
(tkw-major-mode-lighter 'emacs-lisp-mode "el")

;;;; emacs-lisp/macroexp.el
;; マクロ展開で厄介な、let 文のシンボルとマクロ引数のシンボルの衝突を回避する
;; macroexp-let*
;; macroexp-let2

;;;; emacs-lisp/nadvice.el
;; Emacs 24.4 からの機能
;; defadvice よりは add-function を使う方が管理は簡単。

;;;; emacs-lisp/trace.el
(bind-key "C-c r" 'trace-function)
(bind-key "C-c u" 'untrace-function)
(bind-key "C-c U" 'untrace-all)

;;;; emulation/cua-base.el
;; C-RET で矩形選択、RETで挿入
;;(lazyload () "cua-base"
;;  (setq cua-enable-cua-keys nil))
;;(cua-mode t)

;;;; env.el
;; シェル環境変数をEmacsの環境変数に反映させる。
;; Emacs配下で動作するプロセスに引き継ぐ環境変数で重要なもの：
;; PATH, JAVA_TOOL_OPTIONS, HTTP_PROXY, BIBINPUTS, etc.

;; 参考 Emacs で利用する環境変数
;; （特定言語目的のものは除く）
;; |--------------------------+------------------------------------------------|
;; | BIBINPUTS                | filesets.el, textmodes/bibtex.el               |
;; | CDPATH                   | files.el                                       |
;; | CLASSPATH                | progmodes/gud.el                               |
;; | COLORFGBG                | term/rxvt.el                                   |
;; | COLORTERM                | term/xterm.el                                  |
;; | COLUMNS                  | calendar/calendar.el, man.el                   |
;; | COMSPEC                  | net/tramp.el, w32-fns.el                       |
;; | CVSREAD                  | vc/vc-cvs.el                                   |
;; | CVSROOT                  | vc/pcvs.el                                     |
;; | DESKTOP_SESSION          | net/browse-url.el                              |
;; | DISPLAY                  | calc/calc-graph.el, gnus/mailcap.el,           |
;; |                          | mail/emacsbug.el, net/browse-url.el            |
;; |                          | term/x-win.el                                  |
;; | EMACS                    | comint.el, progmodes/compile.el                |
;; | EMAIL                    | startup.el                                     |
;; | EPROLOG                  | progmodes/prolog.el                            |
;; | ESHELL                   | shell.el, term.el, terminal.el,                |
;; |                          | textmodes/tex-mode.el, w32-fns.el              |
;; | GDBHISTFILE              | progmodes/gdb-mi.el                            |
;; | GNOME_DESKTOP_SESSION_ID | mail/emacsbug.el, net/browse-url.el            |
;; | GPG_AGENT_INFO           | epg.el, obsolete/pgg-gpg.el                    |
;; | GREP_OPTIONS             | progmodes/grep.el                              |
;; | HFY_INITFILE             | htmlfontify.el                                 |
;; | HISTFILE                 | eshell/em-hist.el, shell.el                    |
;; | HISTSIZE                 | eshell/em-hist.el, progmodes/gdb-mi.el         |
;; |                          | shell.el                                       |
;; | HOME                     | gnus/nnir.el, vc/emerge.el                     |
;; | HOST                     | net/zeroconf.el                                |
;; | IDLWAVE_HELP_LOCATION    | progmodes/idlw-help.el                         |
;; | IDL_DIR                  | progmodes/idlwave.el                           |
;; | INCLUDE                  | progmodes/flymake.el                           |
;; | INCPATH                  | obsolete/complete.el                           |
;; | INFOPATH                 | info.el                                        |
;; | INITIALS                 | calendar/todo-mode.el                          |
;; | IRCNAME                  | erc/erc.el                                     |
;; | IRCNICK                  | erc/erc.el                                     |
;; | IRCSERVER                | erc/erc.el                                     |
;; | KDE_FULL_SESSION         | mail/emacsbug.el, net/browse-url.el            |
;; | LC_ALL                   | cedet/semantic/bovine/gcc.el                   |
;; | LC_MESSAGES              | international/mule-cmds.el                     |
;; | LINTER_MBX               | progmodes/sql.el                               |
;; | LOGNAME                  | gnus/pop3.el                                   |
;; | LPDEST                   | printing.el                                    |
;; | MAIL                     | gnus/mail-source.el, mail/mspools.el,          |
;; |                          | mail/rmail.el, time.el                         |
;; | MAILCAPS                 | gnus/mailcap.el                                |
;; | MAILDIR                  | gnus/mail-source.el                            |
;; | MAILHOST                 | gnus/mail-source.el, gnus/pop3.el              |
;; | MANPATH                  | woman.el                                       |
;; | MH                       | mh-e/mh-utils.el                               |
;; | MIMETYPES                | gnus/mailcap.el                                |
;; | MPD_HOST                 | mpc.el                                         |
;; | MPD_PORT                 | mpc.el                                         |
;; | MY_BIBINPUTS             | filesets.el                                    |
;; | MY_TEXINPUTS             | filesets.el                                    |
;; | NNTPSERVER               | gnus/gnus.el                                   |
;; | NO_PROXY                 | url/url.el                                     |
;; | ORGANIZATION             | autoinsert.el, emacs-lisp/copyright.el,        |
;; |                          | gnus/message.el,                               |
;; | OSTYPE                   | printing.el                                    |
;; | PATH                     | eshell/esh-cmd.el, eshell/esh-ext.el,          |
;; |                          | eshell/esh-util.el, net/tramp-sh.el,           |
;; |                          | printing.el, progmodes/python.el, woman.el     |
;; | PROJECTDIR               | ldefs-boot.el, loaddefs.el, vc/vc-sccs.el      |
;; | PWD                      | startup.el                                     |
;; | PYTHONPATH               | progmodes/python.el                            |
;; | REPLYTO                  | mail/sendmail.el                               |
;; | SAVEDIR                  | gnus/gnus.el                                   |
;; | SHELL                    | progmodes/sh-script.el, term.el, terminal.el   |
;; |                          | w32-fns.el                                     |
;; | SMTPSERVER               | mail/smtpmail.el                               |
;; | SSH_AGENT_PID            | net/tramp.el                                   |
;; | SSH_AUTH_SOCK            | net/tramp.el                                   |
;; | SVN_ASP_DOT_NET_HACK     | ldefs-boot.el, loaddefs.el, vc/vc-svn.el       |
;; | SYSTEMP                  | progmodes/prolog.el                            |
;; | SystemRoot               | w32-common-fns.el                              |
;; | TEMP                     | cus-start.el, net/tramp-compat.el,             |
;; |                          | printing.el, vc/ediff-init.el                  |
;; | TERM                     | emulation/edt-mapper.el, emulation/edt.el,     |
;; |                          | flow-ctrl.el, international/mule-diag.el       |
;; | TERM_PROGRAM             | international/mule-cmds.el                     |
;; | TEXINPUTS                | filesets.el                                    |
;; | TMP                      | cus-start.el, net/tramp-compat.el,             |
;; |                          | progmodes/prolog.el, vc/ediff-init.el          |
;; | TMPDIR                   | dos-w32.el, files.el, net/tramp-compat.el,     |
;; |                          | progmodes/prolog.el, server.el, url/url-vars.el|
;; | TZ                       | org/org-icalendar.el, time-stamp.el, time.el,  |
;; |                          | vc/add-log.el                                  |
;; | UNIX95                   | net/tramp-compat.el                            |
;; | USER                     | gnus/mail-source.el, gnus/pop3.el              |
;; | VERSION_CONTROL          | startup.el, vc/ediff-ptch.el                   |
;; | XDG_CURRENT_DESKTOP      | net/browse-url.el                              |
;; | XDG_DATA_HOME            | files.el                                       |
;; | no_PROXY                 | url/url.el, url/url.el                         |
;; | winbootdir               | dos-w32.el                                     |

;;;; epa.el・epg.el
;; [[info:epa]]
;; gnupg のインストールと起動
;; - Windows :: http://gpg4win.org/
;; - Linux ::  eval `gpg-agent --daemon` を session 単位で。
;; - Macintosh :: MacGPG2 （MacPortsは不推奨）
;;   https://gpgtools.org/macgpg2/index.html
;;   自動的に launhchd で gpg-agent が起動する。
;; - symmetric passphrase を使う場合、ファイル先頭に以下を付加。
;;   -*- epa-file-encrypt-to: nil; epa-file-select-keys: silent -*-
;; M-x epa-list-keys
;; 同じファイルに対して、パスワードを何度も打ち込まないようにする。
;; （GnuPG2 を使う場合は GPG-AGENTを起動する必要がある。）
(with-eval-after-load 'epa-file
  ;;(setq epa-file-cache-passphrase-for-symmetric-encryption t)
  )

;;;; erc
;; 注意！ISO-2022-JPが利用できないため、日本語チャットには riece を使うこと！
;; Rieceが使いにくい場合・UTF-8の場合はこちらを使用する。
;;;;; erc/erc.el
;; IRCへの接続は M-x erc-server-select が推奨。
;; 各チャネルで、メッセージ受信時に個別にバッファが作成される。
;; IRCサーバコマンド
;; - /list :: チャンネルリスト一覧
;; - /join ::
;; IRCチャネルコマンド
;; - /part :: チャンネル離脱
;; - /topic
;; - /names #ch
;; - /nick
;; - /whois <nick> （OCNだとログイン名でバレバレ…）
(with-eval-after-load 'erc
  ;; デフォルトIRC設定
  (set-variable 'erc-server "irc.freenode.net")
  (set-variable 'erc-port 6667)
  (set-variable 'erc-nick '("teufelsdrockh" "dotabata" "bata"))
  (set-variable 'erc-password nil)
  ;; ログイン
  (set-variable 'erc-anonymous-login t)
  (set-variable 'erc-prompt-for-password nil)
  (set-variable 'erc-command-indicator "CMD")
  (erc-autojoin-mode 1)
  ;; メッセージ送出
  (set-variable 'erc-send-whitespace-lines nil)
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  ;; /join
  (set-variable 'erc-join-buffer 'bury) ; 裏バッファ
  ;; /msg
  (set-variable 'erc-auto-query 'window-noselect) ; 裏バッファ
  ;; /quit
  ;;(set-variable 'erc-quit-reason-various-alist
  ;;              '(("brb"    "I'll be right back.")
  ;;                ("lunch"  "Having lunch.")
  ;;                ("dinner" "Having dinner.")
  ;;                ("food"   "Getting food.")
  ;;                ("sleep"  "Sleeping.")
  ;;                ("work"   "Getting work done.")
  ;;                ))
  (set-variable 'erc-quit-reason 'erc-quit-reason-various)
  ;; /part
  ;;(set-variable 'erc-part-reason-various-alist
  ;;              '(("brb"    "I'll be right back.")
  ;;                ("lunch"  "Having lunch.")
  ;;                ("dinner" "Having dinner.")
  ;;                ("food"   "Getting food.")
  ;;                ("sleep"  "Sleeping.")
  ;;                ("work"   "Getting work done.")
  ;;                ))
  (set-variable 'erc-part-reason 'erc-part-reason-various)
  ;; モジュール erc-XXXX-enable によって起動
  (set-variable 'erc-modules
                `(
                  autoaway         ; erc-autoaway.el
                  autojoin         ; erc-join.el
                  button           ; erc-button.el
                  ;;capab          ; erc-capab.el
                  completion       ; erc-pcomplete.el
                  dcc              ; erc-dcc.el
                  fill             ; erc-fill.el
                  ;;identd         ; erc-identd.el
                  irccontrols      ; erc-goodies.el
                  ;;keep-place     ; erc-goodies.el
                  list             ; erc-list.el :: /list コマンド処理
                  log              ; erc-log.el
                  match            ; erc-match.el :: 知り合いのハイライト・通知
                  menu             ; erc-menu.el
                  move-to-prompt   ; erc-goodies.el
                  ;;netsplit         ; erc-netsplit.el （再帰読み込みエラーになる）
                  networks         ; erc-networks.el
                  noncommands      ; erc-goodies.el
                  notify           ; erc-notify.el
                  page             ; erc-page.el
                  readonly         ; erc-goodies.el
                  ;; replace       ; erc-replace.el
                  ring             ; erc-ring.el
                  scrolltobottom   ; erc-goodies.el
                  ;;services       ; erc-services.el (NickServ)
                  smiley           ; erc-goodies.el
                  sound            ; erc-sound.el
                  stamp            ; erc-stamp.el
                  ;; spelling      ; erc-spelling.el
                  track            ; erc-track.el
                  truncate         ; erc-truncate.el
                  ;; unmorse       ; erc-goodies.el
                  ;; xdcc          ; erc-xdcc.el
                  ))
  (erc-log-mode 1))

;; erc でパスワードを設定しない場合、auth-source から取得する。
(defun tkw-erc-w3 () (interactive)
  (erc :server "irc.w3.org" :port "6665" :nick "kawabata"))
(defun tkw-erc-freenode () (interactive)
  (erc :server "irc.freenode.net" :port "6667" :nick "teufelsdrockh"))
(defun tkw-erc-2ch ()
  (interactive)
  (set-variable 'erc-server-coding-system '(iso-2022-jp . iso-2022-jp))
  (erc :server "irc.2ch.net" :port "6667" :nick "batta"))

;;;;; erc/erc-backend.el
(with-eval-after-load 'erc-backend
  (set-variable 'erc-encoding-coding-alist
                '(("2ch.net" . iso-2022-jp)))
  (set-variable 'erc-server-auto-reconnect t)
  (set-variable 'erc-server-reconnect-attempts t)
  (set-variable 'erc-server-reconnet-timeout 10))

;;;;; erc/erc-ibuffer.el
;; ibuffer の "/ C-e" を、ercサーバリストのフィルタにする。
(defvar ibuffer-mode-map)
(with-eval-after-load 'erc-ibuffer
  (bind-key "/ \C-e" 'ibuffer-filter-by-erc-server ibuffer-mode-map))

;;;;; erc/erc-list.el
;; リスト処理（自動読み込みされる）

;;;;; erc/erc-log.el
(defvar erc-log-channels-directory)
(with-eval-after-load 'erc-log
  (set-variable 'erc-log-channels-directory "~/.irclog/")
  (set-variable 'erc-save-buffer-on-part t)
  (set-variable 'erc-log-file-coding-system 'utf-8)
  (set-variable 'erc-log-write-after-send t)
  (set-variable 'erc-log-write-after-insert t)
  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t)))

;;;;; erc/erc-join.el
;; チャネル接続時にパスワードが必要な場合は、auth-sources に書いておけばOK。
(with-eval-after-load 'erc-join
  (set-variable 'erc-autojoin-channels-alist
                `(("freenode.net" "#emacs" "#emacs-lisp-ja" "#emacs-ja"
                   "#wikipedia-ja" "##japanese")
                  ("w3.org" "#css")
                  ("2ch.net" "#IRC航空宇宙局" ;; ,(encode-coding-string "#IRC航空宇宙局" 'iso-2022-jp)
                   ,(encode-coding-string "#おもしろネタ速報" 'iso-2022-jp)
                   "#japanese" "#yaruo"))))

;;;;; erc/erc-match.el
(with-eval-after-load 'erc-match
  (set-variable 'erc-pals '("rms"
                            "hober"
                            "alan"
                            "fantasai")))

;;;;; erc/erc-networks.el
(defvar erc-server-alist)
(with-eval-after-load 'erc-networks
  (pushnew '("W3C: " w3c "irc.w3.org" ((6665 6667)))
           erc-server-alist)
  (set-variable 'erc-networks-alist
                '((freenode "freenode.net")
                  (w3c "w3.org"))))

;;;;; erc/erc-notify.el
;; erc-notify-list に nickname を入れる。

;;;;; erc/erc-services.el
;; NickServ 管理
;;(lazyload () "erc-services"
;;  (let ((secret (plist-get (nth 0 (auth-source-search :host "freenode.net"))
;;                           :secret)))
;;    (when (functionp secret) (setq secret (funcall secret)))
;;    (when secret
;;      (setq erc-nickserv-passwords (list secret)
;;            erc-nick "batta"))))

;;;;; erc/erc-truncate.el
;; メッセージ送出時に長すぎるメッセージをtruncateする。
(with-eval-after-load 'erc-truncate
  (set-variable 'erc-truncate-buffer-on-save t)
  (set-variable 'erc-max-buffer-size 40000))

;;;;; erc/erc-track.el
(with-eval-after-load 'erc-track
  (set-variable 'erc-track-exclude-types
                '("JOIN" "NICK" "PART" "QUIT" "MODE"
                  "324" "329" "332" "333" "353" "477")))

;;;; eshell
;; 参考 :: http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/
;; * zsh との比較と注意点
;; |            | zsh        | eshell     |
;; |------------+------------+------------|
;; | script     | zsh-script | emacs-lisp |
;; | alias      | ~/.alias   | elisp      |
;; | globbing   | OK         | OK         |
;; | Signal     | OK         | no         |
;; | MS Windows | cygwin     | OK         |
;; | ssh        | OK         | no         |
;; - 環境変数設定は、Emacs の getenv で設定する。
;; - zsh の複雑な globbing はできない。（必要な場面はあまり多くない）

;;;; eshell/eshell.el
;; (setq eshell-directory-name (locate-user-emacs-file "eshell/"))

;;;; eshell/esh-modules.el
(with-eval-after-load 'esh-module
  (set-variable 'eshell-modules-list
                '(eshell-alias   ; em-alias
                  eshell-banner  ; em-banner
                  eshell-basic   ; em-basic (echo, umask, and version)
                  eshell-cmpl    ; em-cmpl
                  eshell-dirs    ; em-dirs
                  eshell-glob    ; em-glob
                  eshell-hist    ; em-hist
                  eshell-ls      ; em-ls
                  eshell-pred    ; em-pred
                  eshell-prompt  ; em-prompt
                  eshell-rebind  ; em-rebind
                  eshell-script  ; em-script
                  ;;eshell-smart ; em-smart
                  eshell-term    ; em-term
                  eshell-unix    ; em-unix em-xtra
                  )))

;;;; eshell/em-cmpl.el
(with-eval-after-load 'em-cmpl
  ;; 補完時にサイクルする
  (set-variable 'eshell-cmpl-cycle-completions t)
  ;;補完候補がこの数値以下だとサイクルせずに候補表示
  (set-variable 'eshell-cmpl-cycle-cutoff-length 5)
  ;; 補完時に大文字小文字を区別しない
  (set-variable 'eshell-cmpl-ignore-case t)
  ;; CVSを除外するのはなし。
  (set-variable 'eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\)/\\'"))

;;;; eshell/em-hist.el
;; ヒストリファイルの保存
(with-eval-after-load 'em-hist
  ;; 履歴で重複を無視する
  (set-variable 'eshell-hist-ignoredups t))

;;;; eshell/em-glob.el
;; - globbing :: http://www.emacswiki.org/emacs/EshellGlobbing

;;;; eshell/em-rebind.el
;; C-a をプロンプト後ろに移動する、などを shell と同じ動作にする。

;;;; eshell/esh-opt.el
;; cf. [[info:eshell]]
;; http://www.masteringemacs.org/articles/2010/11/01/running-shells-in-emacs-overview/

(with-eval-after-load 'esh-opt
  (setenv "PAGER" "cat"))

;;;; faces.el
;; Emacsの色管理 (ns/x/w32/tty)
;; - defined-colors :: フレームの種類に応じてサポートされている色の種類を返す。
;; - color-values :: 色のRGB値を返す (xw-color-values/tty-color-values)
;; tty は原則8色だが、tty-color-standard-values で rgb.txt の類似色を返す。
(bind-key "M-f" 'describe-face help-map)
(bind-key "C-x C-f" 'find-file-at-point)
(bind-key "C-x 4 f" 'ffap-other-window)
(bind-key "C-x d" 'dired-at-point)
(defvar ffap-alist)
(with-eval-after-load 'ffap
  (ffap-bindings)
  (set-variable 'ffap-machine-p-known 'accept)
  (set-variable 'ffap-newfile-prompt t)
  (set-variable 'ffap-rfc-path "http://www.ietf.org/rfc/rfc%s.txt")
  ;;(set-variable 'ffap-dired-wildcards "*") ; TODO CHECK
  (set-variable 'ffap-ftp-regexp nil)
  (set-variable 'ffap-url-unwrap-remote nil) ; domain を ftp にwrapするのを抑止。
  )

;; ftp 時に ping をしないで，いきなり ange-ftp で開く
;;(setq ffap-machine-p-known 'accept)
;; ffap-kpathsea-expand-path で展開するパスの深さ
;;(setq ffap-kpathsea-depth 5)
;; ttp のように不完全な URL を修正する
;;(defadvice ffap-url-at-point (after support-omitted-h activate)
;;  (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
;;    (setq ad-return-value (concat "h" ad-return-value))))

;;;; filecache.el
;; find-fileなどでファイル名を途中まで打って、C-TABで全体を補完。
;; 補完範囲のディレクトリは別途指定するが、これを使うよりはlocateで探し
;; た方が早いと思われる。
;;(when (executable-find "locate")
;;  (eval-after-load
;;   "filecache"
;;   '(progn
;;      (message "Loading file cache...")
;;      ;;(file-cache-add-directory-using-locate "~/projects")
;;      ;;(file-cache-add-directory-using-find "~/projects")
;;      ;;(file-cache-add-directory-list load-path)
;;      ;;(file-cache-add-directory "~/")
;;      ;;(file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
;;     ))
;;  )

;;;; files.el
(setq kept-old-versions 2) ; バックアップファイルの古いバージョン
(setq kept-new-versions 2) ; バックアップファイルの新しいバージョン
(setq delete-old-versions t)
;; zip-mode などで、本来のキーを食うことがあるので、offにする。
(setq view-read-only nil)
;; 新しいキー
(bind-key "C-x C-v" 'revert-buffer)
(bind-key "C-x M-l" 'load-library)
;;(setq make-backup-files nil)
;;(setq version-control t)
;;(setq require-final-newline t)
;; auto-mode-alist を大文字小文字を区別して探し、その後に無視して探す。
(setq auto-mode-case-fold t)

;; 頻繁に開くファイル（org-captureで代用）

(defvar tkw-find-file-shortcut-keys
  `((?a . "~/.emacs.d/authinfo.gpg")
    (?e . "~/.environ")
    (?3 . "~/share/org/work/w3c.org.txt")
    (?h . "~/share/org/home/home.org.txt")
    (?H . "~/share/org/home/home.org.txt") ; M-h はMacOSの予約キーなので念の為
    (?c . ,(locate-user-emacs-file "Cask"))
    (?g . ,(locate-user-emacs-file "gnus.el"))
    (?i . ,(locate-user-emacs-file "init.el"))
    (?I . "~/share/cvs/cjkvi/ids-orig/ids.orig.txt")
    (?E . "~/share/cvs/cjkvi/ids-orig/extf-ids.orig.txt")
    (?j . "~/share/org/work/jsc2.org.txt")
    (?L . ,(locate-user-emacs-file "lookup/init.el"))
    (?l . "*Help*")
    (?m . "*Messages*")
    (?p . "~/share/dotfiles/.passwd.gpg")
    (?s . "*scratch*")
    (?t . "~/share/org/toodledo.org")
    (?v . "~/share/cvs/cjkvi/variants-orig/cjkvi-variants.orig.txt")
    (?u . "~/share/Standards/UCV/iwds/iwds.orig.xml")
    (?w . "~/share/org/work/work.org.txt")
    (?W . "~/share/org/work/w3c.org.txt")
    (?z . "~/.zsh_history")
    (?Z . "~/share/cvs/cjkvi/dict-orig/zihai.orig.txt")
    (?x . "~/share/cvs/cjkvi/dict-orig/zihai-check.txt")))

(dolist (pair tkw-find-file-shortcut-keys)
  (eval `(bind-key ,(format "C-x A-%c" (car pair)) 'tkw-find-file)))

(defun tkw-find-file (arg)
  "Find file shortcut keys. Prefix ARG for new window."
  (interactive "P")
  (let* ((last-char (tkw-this-command-char))
         (target (cdr (assoc last-char tkw-find-file-shortcut-keys))))
    (if (string-match "^\\*" target) (switch-to-buffer-other-window target)
      (if (not arg) (find-file target)
        (if (functionp 'popwin:find-file)
            (popwin:find-file target)
          (find-file-other-window target))))))

;;;; fill.el
(setq adaptive-fill-regexp
      (purecopy (concat "[ 　\t]*\\([-!|#%;>*＃"
                        '(#x0b7 #x2022 #x2023 #x2043 #x25e6)
                        "]+[ 　\t]*\\|(?[0-9]+[.)][ \t]*\\)*")))
(setq adaptive-fill-mode t)

;;;; finder.el / finder-inf.el
;; M-x finder-commentary
;; M-x finder-by-keywords
;; finder-inf.el を再生性するには、パッケージインストール後に
;; finder-compile-keywords を実行する。

;; finder-inf keywords 一覧 ::
;; 公式 ::

;; abbrev        abbreviation handling, typing shortcuts, and macros
;; bib           bibliography processors
;; c             C and related programming languages
;; calendar      calendar and time management tools
;; comm          communications, networking, and remote file access
;; convenience   convenience features for faster editing
;; data          editing data (non-text) files
;; docs          Emacs documentation facilities
;; emulations    emulations of other editors
;; extensions    Emacs Lisp language extensions
;; faces         fonts and colors for text
;; files         file editing and manipulation
;; frames        Emacs frames and window systems
;; games         games, jokes and amusements
;; hardware      interfacing with system hardware
;; help          on-line help systems
;; hypermedia    links between text or other media types
;; i18n          internationalization and character-set support
;; internal      code for Emacs internals, build process, defaults
;; languages     specialized modes for editing programming languages
;; lisp          Lisp support, including Emacs Lisp
;; local         code local to your site
;; maint         Emacs development tools and aids
;; mail          email reading and posting
;; matching      searching, matching, and sorting
;; mouse         mouse support
;; multimedia    images and sound
;; news          USENET news reading and posting
;; outlines      hierarchical outlining and note taking
;; processes     processes, subshells, and compilation
;; terminals     text terminals (ttys)
;; tex           the TeX document formatter
;; tools         programming tools
;; unix          UNIX feature interfaces and emulators
;; vc            version control
;; wp            word processing

;;;; font-core.el
(global-font-lock-mode 1) ;; 必要に応じて0にする。
(bind-key "M-M" 'font-lock-mode)

;;;; frame.el
;; iconify を設定する
(with-eval-after-load 'help
  (bind-key "M-i" 'iconify-or-deiconify-frame help-map))

;;;; generic-x
;; fvmrc, XDefaults, xmodmap, /etc/hosts, *.inf, *.reg, etc.
;; generic-mode-find-file-hook が、Windowsでエラーを起こんすのでコメントアウト
;;(when-interactive-and t
;;  (require 'generic-x))

;;;; gnus/auth-source.el
;; 詳細は [[info:auth]] 参照。
(autoload 'auth-source-search "auth-source")
(setq auth-sources `(,(locate-user-emacs-file "authinfo.gpg")))

;;;; gnus/gnus.el
;; 設定は ~/.emacs.d/gnus.el に分離する。
(bind-key "C-c n" 'gnus)
(bind-key "C-c m" 'gnus-msg-mail)
(with-eval-after-load 'gnus-start
  (set-variable 'gnus-init-file (locate-user-emacs-file "gnus.el")))

;;;; gnus/html2text.el
;; iso-2022-jpをうっかりhtml化した時の復元に便利な設定
(defvar html2text-replace-list)
(defvar html2text-remove-tag-list)
(defvar html2text-remove-tag-list2)
(with-eval-after-load 'html2text
  (setq html2text-replace-list
        '(("&amp;" . "&") ("&nbsp;" . " ") ("&gt;" . ">") ("&lt;" . "<")
          ("&quot;" . "\"")))
  (setq html2text-remove-tag-list
        '("html" "body" "p" "img" "dir" "head" "div" "br" "font" "title"
          "meta" "tr" "td" "table" "span" "div"))
  (setq html2text-remove-tag-list2 '("li" "dt" "dd" "meta")))

;;;; gnus/mm-util.el
(with-eval-after-load 'mm-util
  (when (coding-system-p 'cp50220)
    (pushnew '(iso-2022-jp . cp50220) mm-charset-override-alist)))

;;;; gnus/plstore.el
(with-eval-after-load 'plstore
  (let ((plstore-file (locate-user-emacs-file "auth.plist")))
    (when (file-exists-p plstore-file)
      (plstore-open plstore-file))))
;; MacのInfo.plist と誤認する可能性があるので auto-mode-alist から外す。
;; (push (cons "\\.plist\\'" 'plstore-mode) auto-mode-alist)

;;;; gnus/score.el
(push '("\\.SCORE\\'" . gnus-score-mode) auto-mode-alist)

;;;; gnus/sieve.el
;; RFC3028 Email Filtering Language Sieve.

;;;; gv.el
;; 汎変数マクロ一覧
;; | マクロ名 | 変数型 | 説明                                         |
;; |----------+--------+----------------------------------------------|
;; | setf     | 任意   | setq と同様、汎変数に値を設定する            |
;; | incf     | 数値   | 汎変数の値を１（または任意数）増やす         |
;; | decf     | 数値   | 汎変数の値を１（または任意数）減らす         |
;; | remf     | plist  | 汎変数plist からシンボルの値を除去する       |
;; | shiftf   | 任意   | 汎変数群の値を１つずつずらす                 |
;; | rotatef  | 任意   | 汎変数群の値をローテートする                 |
;; | callf    | 任意   | 汎変数の値に関数を適用する                   |
;; | callf2   | 任意   | 汎変数の値に関数を適用する（関数の第二引数） |
;; | letf     | 任意   | 一時的に汎変数の値を変更して評価する         |
;; | letf*    | 任意   | 一時的に汎変数の値を変更して評価する         |
;; | getf     | plist  | 汎変数plistからシンボルの値を取得する        |
;; | push     | リスト | 汎変数リストの先頭に要素を追加               |
;; | pushnew  | リスト | 汎変数リストの先頭に新要素を追加             |
;; | pop      | リスト | 汎変数リストの先頭から要素を取得             |
;; Emacs 24.3 gv.el でコメントアウトされていた関数。
;; Emacs 25.1 で導入される予定。（HEADにはすでに有る。）
(unless (functionp 'alist-get)
  (defun alist-get (key alist)
    "Get the value associated to KEY in ALIST."
    (declare
     (gv-expander
      (lambda (do)
        (macroexp-let2 macroexp-copyable-p k key
          (gv-letplace (getter setter) alist
            (macroexp-let2 nil p `(assoc ,k ,getter)
              (funcall do `(cdr ,p)
                       (lambda (v)
                         `(if ,p (setcdr ,p ,v)
                            ,(funcall setter
                                      `(cons (cons ,k ,v) ,getter)))))))))))
    (cdr (assoc key alist))))

;;;; help
(temp-buffer-resize-mode 1)

;;;; help-at-pt
(with-eval-after-load 'help-at-pt
  (set-variable 'help-at-pt-display-when-idle t)
  (set-variable 'help-at-pt-timer-delay 0.1))

;;;; hippie-exp.el
(bind-key "M-/" 'hippie-expand)
(with-eval-after-load 'hippie-exp
  (set-variable 'hippie-expand-dabbrev-as-symbol t))

;;;; hl-line.el
;; 現在の行をハイライトする
;; smart-cursor-line と衝突するのでオフ。
;; (global-hl-line-mode)

;;;; htmlfontify.el (obsolete)
;; → hfyview.el に移行。
;; coral が入手できないので、hfyview.el を使ってブラウザに表示させて
;; それを印刷する。
;;(defun print-buffer-html ()
;;  "印刷する。"
;;  (interactive)
;;  (let ((file (make-temp-file "print-buffer-" nil ".html")))
;;    (htmlfontify-buffer nil file)
;;    (write-region (point-min) (point-max) file)
;;    (message "printing... %s " file)
;;    (cond ((eq system-type 'darwin)
;;           (shell-command (concat "coral -d " file)))
;;          ((eq system-type 'windows-nt)
;;           (w32-shell-execute "print " file))
;;          (t (shell-command (concat "open " file))))
;;    (message "printing... done")
;;    (delete-file file)))

;;;; icomplete.el
;; ミニバッファ補完機能（入力中途で候補を表示する）
;; icomplete-post-command-hook でエラーが出るので一旦中止。
;; [?\M-\t] 'minibuffer-force-complete
;; [?\C-j]  'minibuffer-force-complete-and-exit
;; [?\C-.]  'icomplete-forward-completions
;; [?\C-,]  'icomplete-backward-completions
;; (icomplete-mode 1)

;;;; indent.el
(setq indent-line-function 'indent-relative-maybe)
(setq-default indent-tabs-mode nil) ; no tabs indentation.

;;;; ibuffer.el
;; list-buffersの高機能版。色が付いて、様々なパラメータ表示が可能。隠れ
;; バッファを表示したい場合は、一時的にibuffer-maybe-show-predicatesの
;; 値をnilにする。
(bind-key "C-x C-b" 'ibuffer)
(defvar ibuffer-auto-mode)
(with-eval-after-load 'ibuffer
  ;; バッファに変化があれば、自動的にリストを更新する。
  (set-variable 'ibuffer-mode-hook '(ibuffer-auto-mode))
  (set-variable 'ibuffer-default-sorting-mode 'alphabetic) ; default is 'recency
  ;; ibufferのオリジナルカラムの設定
  ;; 以下を設定すると unused lexical argumetn `mark' エラーが出る。
  ;; (define-ibuffer-column
  ;;  ;; ibuffer-formats に追加した文字
  ;;  coding
  ;;  ;; 一行目の文字
  ;;  (:name " coding ")
  ;;  ;; 以下に文字コードを返す関数を書く
  ;;  (condition-case nil
  ;;      (if (coding-system-get buffer-file-coding-system 'mime-charset)
  ;;          (format " %s"
  ;;                  (coding-system-get buffer-file-coding-system 'mime-charset))
  ;;        " undefined")
  ;;    (error " undefined"))))

  ;; 新しいibufferフォーマットの設定
  ;; オリジナルは、以下の通り。（ibuffer-switch-format で切り替え）
  ;; ((mark modified read-only " "  (name 16 -1) " "
  ;;        (size 6 -1 :right) " "
  ;;        (mode 16 16 :right) " " filename)
  ;;  (mark " " (name 16 -1) " " filename))
  ;; (setq ibuffer-formats
  ;;       '((mark modified read-only (coding 15 15) " " (name 30 30)
  ;;               " " (size 6 -1) " " (mode 16 16) " " filename)
  ;;         (mark (coding 15 15) " " (name 30 -1) " " filename)))
  (set-variable 'ibuffer-fontification-alist
                '((10 buffer-read-only font-lock-constant-face)
                  (15 (and buffer-file-name
                           (string-match ibuffer-compressed-file-name-regexp buffer-file-name))
                      font-lock-doc-face)
                  (20 (string-match "^*"
                                    (buffer-name))
                      font-lock-keyword-face)
                  (25 (and
                       (string-match "^ "
                                     (buffer-name))
                       (null buffer-file-name))
                      italic)
                  (30 (memq major-mode ibuffer-help-buffer-modes)
                      font-lock-comment-face)
                  (35 (eq major-mode 'dired-mode)
                      font-lock-function-name-face))))

;;;; ido.el
;; ファイル保存時にファイル名の自動補完を行う。
;; C-f :: デフォルトの動作に戻る。
;; C-j :: 入力テキストをそのまま使う。
;; C-r/C-s :: 前・次の候補
;; ファイル保存時に勝手に既存ファイル名を補完するのが不便なので使用中止。
;; → 慣れたら便利なので利用再開。
;; (ido-mode t)
;; (ido-everywhere 1)
;; ここまで
;; (bind-key "SPC" 'ido-exit-minibuffer ido-file-dir-completion-map)
;; (bind-key "C-h" 'ido-delete-backward-updir ido-file-dir-completion-map)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-use-virtual-buffers t ; [C-x b] (or M-x ido-switch-buffer)
;;       ido-max-prospects 10)

;;;; image.el
;;(setq imagemagick-render-type 1)

;;;; image-file.el
;; 画像ファイルは自動的にimage-file-modeで開く。
(auto-image-file-mode t)

;;;; image-mode.el
(set-variable 'image-animate-loop t)
;; SVGをXMLで開きたい場合は以下のようにする。
;; (setf (alist-get "\\.svgz?\\'" auto-mode-alist) 'nxml-mode)

;;;; imenu.el
;; Emacs 24.3 only.
;; load-theme を実行すると、eval → edebug-read → edebug-read-sexp
;; → which-function → imenu--make-index-alist →
;; →imenu-default-create-index-function
;; でエラーが起きるのを抑止する。
;; 対応モード : C, C++, Java, Fortran, etc.
(setq-default which-function-imenu-failed t)
;; emacs-lisp で追加
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(custom-set-faces
 '(which-func ((t (:foreground "gainsboro")))))

;;;; info.el
(with-eval-after-load 'info
  ;; /usr/share/info を取り除く（/usr/local/share/info を優先する）
  (info-initialize) ; Info-directory-list を設定
  (callf2 delete "/usr/share/info" Info-directory-list)
  ;; M-n が奪われるので注意。
  (bind-key "M-n" nil Info-mode-map)
  ;;(define-key Info-mode-map (kbd "M-n") nil)
  )

;;;; info-look.el
;; - gawk ... /opt/local/share/info/gawk.info
;; - python :: https://github.com/wilfred/python-info
;; latex, texinfo, perl
;; M-x info-lookup-symbol

;;;; international/iso-transl.el
;; key-translation-map を設定・変更する。
;; これをロードすると、"A-*" のショートカット入力がすべて奪われるので注意。

;;;; international/kinsoku.el
;; 禁則処理によって行長が何文字延びてもいいか。
(defvar kinsoku-limit 10)

;;;; international/mule.el
;; new command :: revert-buffer-with-coding-system
(case system-type
  ('darwin
   (modify-coding-system-alist 'process "zsh" '(utf-8-hfs . utf-8))
   (modify-coding-system-alist 'process "git" '(utf-8-hfs . utf-8)))
  ('windows-nt
   ;; Windows の場合は、cygwinかWindows proxy かで異なるので注意する。
   ;; - undecided-dos :: cmdproxy/plink
   ;; - undecided-unix :: zsh/git など、cygwin系
   (modify-coding-system-alist 'process "bash" '(utf-8-unix . utf-8-unix))
   (modify-coding-system-alist 'process "zsh" '(utf-8-unix . utf-8-unix))
   (modify-coding-system-alist 'process "git" '(utf-8-unix . utf-8-unix))
   (set-keyboard-coding-system 'cp932)))
;; w32select.c では、'utf-16le-dos で CF_UNICODEを利用する。
(set-selection-coding-system
 (case system-type
   ;('darwin 'utf-8-hfs)
   ('windows-nt 'utf-16le-dos)
   (t 'utf-8)))

;; mule-version を日本語に直す。
;; かつて mm-version.el としても実装されていた。
;; 本家Emacsのタイムラインは http://www.jwz.org/doc/emacs-timeline.html 参照。
(let*
    ;;; History of Emacs for Japanese
    ;;; Unipress Emacs
    ;; 1985.?:     emaKs? <based on Unipress Emacs?>
    ;; 1986.?:     ΣEMACS (based on Unipress Emacs) (DIT)
    ;;; MicroEmacs
    ;; 1986.10.31: μEmacs 3.7J (SRA)
    ;; 1987.2.20:  Kemacs <μEmacs> (Kyoto-Univ.)
    ;;; Emacs-Clones
    ;; 1990.4.14:  NitEmacs 4.1 (Tokyo-Univ.)
    ;;; GNU Emacs
    ;; 1987.6.21:  Nemacs Ver.1.0 <Emacs 18.41 (1987.3.22)> (ETL)
    ;; 1988.2.9:   Nemacs Ver.2.0 <Emacs 18.45 (1987.6.2)>
    ;; 1988.6.15:  Nemacs Ver.2.1 <Emacs 18.50 (1988.2.13)>
    ;; 1989.4.14:  Nemacs Ver.3.0 <Emacs 18.53 (1989.2.24)>
    ;; 1989.6.14:  Nemacs Ver.3.1 <Emacs 18.54 (1989.4.26)>
    ;; 1989.12.8:  Nemacs Ver.3.2 (HARIKUYOU) <Emacs 18.55 (1989.8.23)>
    ;; 1989.12.15: Nemacs Ver.3.2.1 (MUSUME-DOUJOUJI version)
    ;; 1989.12.17: Nemacs Ver.3.2.1A (MUSUME-DOUJOUJI version with ANCHIN patch)
    ;; 1989.12.22: Nemacs Ver.3.2.3 (YUMENO-AWAYUKI version)
    ;; 1990.3.3:   Nemacs Ver.3.3.1 (HINAMATSURI version)
    ;; 1990.6.6:   Nemacs Ver.3.3.2 (FUJIMUSUME version)
    ;; 他に Meadowy, XEmacs-Mule 等のブランチあり。
    ((versions
      '(
        ;;                              1992.3.4:   0.9.0 Beta
        ;;                              1992.3.23:  0.9.1 Beta
        ;;                              1992.4.6:   0.9.2 Beta
        ;;                              1992.4.18:  0.9.3 Beta
        ;;                              1992.5.28:  0.9.4 Beta
        ;;                              1992.7.31:  0.9.5 Beta
        ;;                              1992.8.5:   0.9.5.1 Beta
        ;;                              1992.10.27: 0.9.6 Beta
        ;;                              1992.12.28: 0.9.7 Beta
        ;;                              1993.1.22:  0.9.7.1 Beta
        ;;                              1993.6.14:  0.9.8 Beta
        ;;("KIRITSUBO"    . "桐壷")   ; 1993.8.1:   1.0 Emacs 18.59
        ;;("HAHAKIGI"     . "帚木")   ; 1994.2.8:   1.1 Emacs 18.59
        ;;("UTSUSEMI"     . "空蝉")   ; 1994.8.6:   2.0 Emacs 19.25
        ;;("YUUGAO"       . "夕顔")   ; 1994.11.2:  2.1 Emacs 19.27
        ;;("WAKAMURASAKI" . "若紫")   ; 1994.12.28: 2.2 Emacs 19.28
        ;;("SUETSUMUHANA" . "末摘花") ; 1995.7.24:  2.3 Emacs 19.28
        ;;("MOMIJINOGA"   . "紅葉賀") ; 1997.9.14:  3.0 Emacs 20.1 <merged>
        ;;("HANANOEN"     . "花宴")   ; 1998.8.13:  4.0 Emacs 20.3
        ;;("AOI"          . "葵")     ; 1999.7.16:  4.1 Emacs 20.4
        ;;("SAKAKI"       . "賢木")   ; 2002.3.18:  5.0 Emacs 21.3
        ("HANACHIRUSATO" . "花散里")  ; 2009.7.29:  6.0 Emacs 23.1
        ("SUMA"         . "須磨")
        ("AKASHI"       . "明石")
        ;;("MIWOTSUKUSHI" . "澪標")
        ;;("YOMOGIU"      . "蓬生")
        ;;("SEKIYA"       . "関屋")
        ;;("EAWASE"       . "絵合")
        ;;("MATSUKAZE"    . "松風")
        ;;("USUKUMO"      . "薄雲")
        ;;("ASAGAO"       . "槿")
        ;;("WOTOME"       . "少女")
        ;;("TAMAKAZURA"   . "玉鬘")
        ;;("HATSUNE"      . "初音")
        ;;("KOCHO"        . "胡蝶")
        ;;("HOTARU"       . "蛍")
        ;;("TOKONATSU"    . "常夏")
        ;;("KAGARIHI"     . "篝火")
        ;;("NOWAKI"       . "野分")
        ;;("MIYUKI"       . "行幸")
        ;;("FUDIBAKAMA"   . "藤袴")
        ;;("MAKIBASHIRA"  . "真木柱")
        ;;("UMEGAE"       . "梅枝")
        ;;("FUJINOURAHA"  . "藤裏葉")
        ;;("WAKANA"       . "若菜")
        ;;("KASHIWAGI"    . "柏木")
        ;;("YOKOBUE"      . "横笛")
        ;;("SUZUMUSHI"    . "鈴虫")
        ;;("YUUGIRI"      . "夕霧")
        ;;("MINORI"       . "御法")
        ;;("MABOROSHI"    . "幻")
        ;;("KUMOGAKURE"   . "雲隠")
        ;;("NIHOUNOMIYA"  . "匂宮")
        ;;("KOUBAI"       . "紅梅")
        ;;("TAKEKAWA"     . "竹河")
        ;;("HASHIHIME"    . "橋姫")
        ;;("SHIIGAMOTO"   . "椎本")
        ;;("AGEMAKI"      . "総角")
        ;;("SAWARABI"     . "早蕨")
        ;;("YADORIGI"     . "宿木")
        ;;("ADUMAYA"      . "東屋")
        ;;("UKIFUNE"      . "浮舟")
        ;;("KAGEROU"      . "蜻蛉")
        ;;("TENARAI"      . "手習")
        ;;("YUMENOUKIHASHI" . "夢浮橋")
        )))
  (mapc
   (lambda (args)
     (when (string-match (car args) mule-version)
       (setq mule-version (replace-match (cdr args) nil nil mule-version))))
   versions))
;; Note: Emacs 本体の歴史は
;; %  bzr tags | grep -i emacs で確認可能。

;; Adobe-Japan1 の文字集合を define-charset で定義する。
(let ((cid2code (locate-user-emacs-file "cmap/aj16/cid2code.txt"))
      (aj16map  (locate-user-emacs-file "cmap/aj16/aj16.map"))
      (code2cid nil))
  (unless (file-exists-p aj16map)
    (when (file-exists-p cid2code)
      (with-temp-file aj16map
        (setq code2cid (make-hash-table :test 'equal))
        (with-temp-buffer
          (insert-file-contents cid2code)
          (re-search-forward "^CID")
          ;; Column 22 : UniJIS-UTF32-H
          (while (re-search-forward "^.+$" nil t)
            (let* ((split (split-string (match-string 0) "\t"))
                   (col22 (elt split 21)))
              (dolist (split2 (split-string col22 ","))
                (when (string-match "^[0-9A-F]+$" split2)
                  (puthash split2 t code2cid))))))
        (maphash (lambda (key _val) (insert "0x" key " 0x" key "\n")) code2cid))))
  (when (file-exists-p aj16map)
    (define-charset 'adobe-japan1 "Adobe-Japan1"
      :map (file-name-sans-extension aj16map)
      :invalid-code 128)))

;;;; international/mule-cmds.el
(bind-key "C-o" 'toggle-input-method) ;; non-Mac用
(bind-key "M-S-SPC" 'toggle-input-method) ;; Mac用
;; pushnew で利用するため、gv化。
(unless (function-get 'get-char-code-property 'gv-expander t)
  (gv-define-simple-setter get-char-code-property
                           put-char-code-property))

;;;; iswitchb.el (obsolete)
;;(when-interactive-and (iswitchb-mode 1)) ; → icomplete.el に移行
;;(with-eval-after-load 'iswitchb
;;  (setq iswitchb-use-virtual-buffers t)
;;  ;; 起動毎にいちいちリセットされるので、iswitchb-mode-map には直接
;;  ;; define-keyできない。
;;  ;; MEMO
;;  ;; - C-s/C-r で候補一覧をローテートできる。
;;  (add-hook
;;    'iswitchb-define-mode-map-hook
;;    (lambda () (bind-key "C-f" 'iswitchb-next-match iswitchb-mode-map)
;;          (bind-key "C-b" 'iswitchb-prev-match iswitchb-mode-map))))
;;  (define-key minibuffer-local-completion-map
;;    "\C-c\C-i" 'file-cache-minibuffer-complete)
;;  (setq iswitchb-method 'samewindow))
;; 以下の命令を実行する場合は、iswitchb-methodを、samewindowにしておく。
;; さもなければ、バッファ選択中に他のフレームに勝手に移動することがある。
;;(defadvice iswitchb-exhibit (after
;;                             iswitchb-exhibit-with-display-buffer
;;                             activate)
;;  "選択している buffer を window に表示してみる。"
;;  (when iswitchb-matches
;;    (setq iswitchb-method 'samewindow)
;;    (select-window (get-buffer-window (cadr (buffer-list))))
;;    (iswitchb-visit-buffer (get-buffer (car iswitchb-matches)))
;;    (select-window (minibuffer-window))))

;;;; jka-compr.el
(auto-compression-mode t)
;; 下記追加済み
;; (add-to-list 'jka-compr-compression-info-list
;;             '["\\.dz\\'"
;;               nil nil nil
;;               "dict uncompressing" "gzip" ("-c" "-q" "-d")
;;               nil t "\037\213"])

;;;; linum.el
;; 行番号の表示
(with-eval-after-load 'linum
  (set-face-attribute 'linum nil :height 0.8)
  (set-variable 'linum-format "%4d"))
;;(global-linum-mode t)

;;;; locate.el
;; Macintosh で locate を使う場合
;; % sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
;; Macintosh で外部ファイルサーバをインデックス化する場合
;; % mdutil -i on /Volumes/mountname
(with-eval-after-load 'locate
  (when (eq system-type 'darwin)
    (set-variable 'locate-command "mdfind")))

;;;; ls-lisp.el
;; ls プロセスを使わずに dired を使用。
;; (eval-and-compile (require 'ls-lisp))
;; (setq ls-lisp-ignore-case t)
;; (setq ls-lisp-dirs-first nil)

;;;; man.el
(defvar Man-mode-map)
(with-eval-after-load 'man
  (bind-key "M-n" nil Man-mode-map)
  (bind-key "M-p" nil Man-mode-map))

;;;; mb-depth.el
(minibuffer-depth-indicate-mode 1)

;;;; menu-bar.el
;; メニューバーを消す。
(menu-bar-mode nil)
;; kill-buffer の代わりに kill-this-buffer を使うことで、現在バッファを
;; killするのにいちいち確認しないようにする。
(bind-key "C-x k" 'kill-this-buffer)

;;;; minibuf-eldef.el
;; プロンプト入力時にミニバッファにデフォルト値を表示し、改行だけで入力
;; できるようにする。
(minibuffer-electric-default-mode 1)

;;;; msb.el
;; メニューのバッファ一覧を階層化する。
;; (msb-mode 1)

;;;; mwheel.el
(eval-and-compile (mwheel-install))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 5)))

;;;; newcomment.el
;; only auto-fill inside comments.
(setq comment-auto-fill-only-comments t)

;;;; net/browse-url.el
;; 必要な場合は、以下に設定する。
;; (setq browse-url-browser-function 'browse-url-default-browser)
;; 2ch等の "ttp://..." を開けるようにする。
(declare-function browse-url "browse-url" (url &rest arsgs))
(defadvice browse-url (before support-omitted-h (url &rest args) activate)
  (when (and url (string-match "\\`ttps?://" url))
    (callf2 concat "h" url)))
(setq browse-url-browser-function
      '(("." . browse-url-default-browser)))
(bind-key "C-c B" 'browse-url-of-file)
;; (bind-key "C-c M-b" 'browse-url-at-point) → C-c O (org-mode-open-at-point) を使う。
;; dired で、HTML等のファイルをブラウザで開く。
(with-eval-after-load 'dired
  (bind-key "C-c B" 'browse-url-of-dired-file dired-mode-map))

;;;; net/eww.el
;; Emacs Web Wowser
;; 超高速Webブラウザ
(with-eval-after-load 'eww
  :config
  (set-variable 'eww-search-prefix "https://google.com/#q="))

;;;; net/newsticker
;; 非同期なフィード取得が可能なRSSリーダ
;; → 非同期が重いので crontab + nnrss (asynchronous) に移行。
;; M-x newsticker-start :: 非同期RSS取得の開始
;; M-x newsticker-show-news  ::
;;(lazyload () "newsticker"
;;  (let ((library "my-newticker-url-list.el.gpg"))
;;    (and (locate-library library)
;;         (load-library library))))

;;;; net/tramp.el
;; e.g. C-x C-f /kawabata@femto:/var/www/html/index.html
;;      C-x C-f /root@localhost:/etc/passwd

;; 注意：auth-source.el を使えば、ssh利用時のパスワードの入力は不要。

;; 注意：相手側hostでは、zshのプロンプトに色を付けたり、ls のエイリアス
;;       で --colorオプションを付けたりしないこと。freezeする場合は、そ
;;       こでBacktraceを出して、*tramp*バッファのプロセス状態を確認する。

;;       sudo を使う場合は、zshで"Igonore insecure directories?" プロン
;;       プトが出ないよう、sudo -s 実行時で確認しておく。出る場合は、
;;       SHELL環境変数を無効にする(sudo の-iオプション）か、fpath から
;;       自分ディレクトリを外す。

;;       なお、smbclientを使ったWindows FSへのアクセスは面倒なことがなくて便利。
;;       これに統一してしまうのもいいかも。;->  使い方：/smb:server_name:/path
(with-eval-after-load 'tramp
  ;; root@localhostへはsudoメソッドを使う。
  (pushnew
   '("\\`localhost\\'" "\\`root\\'" "sudo")
   tramp-default-method-alist :test 'equal))

;; ファイルがrootの場合、自動的にsudoで開き直す。
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

;;(defun th-rename-tramp-buffer ()
;;  (when (file-remote-p (buffer-file-name))
;;    (rename-buffer
;;     (format "%s:%s"
;;             (file-remote-p (buffer-file-name) 'method)
;;             (buffer-name)))))
;;
;;(add-hook 'find-file-hook
;;          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (not (file-directory-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;;;; nxml/nxml-mode.el, nxml/rng-nxml.el
;; xml-mode を{auto,magic}-mode-alistから取り除く。
;; (cl-delete-if (lambda (x) (equal (cdr x) 'xml-mode)) auto-mode-alist)
;; (cl-delete-if (lambda (x) (equal (cdr x) 'xml-mode)) magic-mode-alist)
;; その他のXML関連のモードのうち、自分で最初から編集するものを追加していく。
;; (add-to-auto-mode-alist '("\\.xhtml\\'" . nxml-mode))
;; (add-to-auto-mode-alist '("\\.mxml\\'" . nxml-mode))
;; (add-to-auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))
;; (add-to-auto-mode-alist '("\\.kml\\'" . nxml-mode))
;; (add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))

;;;;; 各種 RelaxNG スキーマと拡張モード
;; |            | Schema/Mode                                        | snippet          | tags  |
;; |------------+----------------------------------------------------+------------------+-------|
;; | ant        |                                                    |                  | ctags |
;; | docbook    |                                                    | docbook-snippets |       |
;; | Google KML | http://members.home.nl/cybarber/geomatters/kml.xsd |                  |       |
;; | epub OPML  | http://epub-revision.googlecode.com/svn/trunk      |                  |       |
;; | mallard    | mallard-mode                                       | mallard-snippe   |       |
;; | xhtml5     | https://github.com/hober/html5-el                  |                  |       |
;; | xmp        | ISO/IEC 16684-1                                    |                  |       |
;; | xsl/xslt   | https://github.com/ndw/xslt-relax-ng               |                  |       |
;; ※ 現在は古くなったため、https://github.com/mavit/html5-el/ の方を利用。

;; XSDは、trangで rnc へ変換する。
;; 便利な RelaxNG Schema集
;; https://github.com/skybert/my-little-friends/tree/master/emacs/.emacs.d/xml
;; kml における section/name 設定例：
;; (setq nxml-section-element-name-regexp "Folder\\|Placemark\\|GroundOverlay")
;; (setq nxml-heading-element-name-regexp "name")
;; 手動でschemaを設定するには、C-c C-s C-f を使用する。
(defvar nxml-mode-map)
(defvar rng-schema-locating-files)
(with-eval-after-load 'nxml-mode
  (pushnew (locate-user-emacs-file "schema/schemas.xml")
           rng-schema-locating-files :test 'equal)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (bind-key "C-c /" 'rng-complete nxml-mode-map)
              ;;(bind-key "M-q" 'tkw-xml-pretty-print-buffer  nxml-mode-map)
              ))
  (set-variable 'nxml-slash-auto-complete-flag t)
  ;; 自分で編集するXMLにおけるセクションの設定
  (set-variable 'nxml-section-element-name-regexp
                (eval-and-compile
                  (regexp-opt
                   '(
                     ;; html5
                     "head" "body" "blockquote" "details" "fieldset"
                     "figure" "td" "section" "article" "nav" "aside"
                     ;; atom
                     "entry"))))
  ;; 自分で編集するXMLにおけるヘッダの設定
  (set-variable 'nxml-heading-element-name-regexp
                (eval-and-compile
                  (regexp-opt
                   '(
                     ;; html5
                     "h1" "h2" "h3" "h4" "h5" "h6"
                     ;; atom
                     "title"))))
  (set-variable 'nxml-slash-auto-complete-flag t))

;; saxon
;; Ubuntu 11 : /usr/share/java/saxon-6.5.5.jar
;; MacOS X   : /opt/local/share/java/saxon-9.1he.jar

;;;; obsolete/yow.el
;; yow が obsolete になり、autoload対象外になったので再設定
;;(autoload 'yow "yow" nil t)

;;;; org/org.el (major-mode)
;; Emacs 24.4 付属の org でも利用できる設定はここで行う。
;; ELPAの最新版や、contrib の設定は use-package で行う。
;; 基本グローバル４＋２コマンド
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c b" 'org-iswitchb)
;; [[info:org#External links][org link]]
(bind-key "C-c O" 'org-open-at-point-global) ;; follow link
(bind-key "C-c L" 'org-insert-link-global)
;; mode
(push '("\\.org.txt$" . org-mode) auto-mode-alist)

(with-eval-after-load 'org
  ;; Shiftキーでまとめて選択しての処理を可能にする。
  (set-variable 'org-support-shift-select t)
  (set-variable 'org-ellipsis "↓")
  ;; org-caputure/org-mobile 等が使用するデフォルトディレクトリ
  (defvar org-directory)
  (setq org-directory "~/share/org")
  ;; C-o 回避
  (defvar org-mode-map)
  (bind-key "C-c M-o" 'org-open-at-point org-mode-map)
  ;; #+STARTUP: indent 相当。自動的にインデントする。必須。
  (set-variable 'org-startup-indented t)
  ;; テンプレートについては、すでに org-structure-template-alist で定義
  ;; されている。
  (set-variable 'org-hide-leading-stars t)
  (set-variable 'org-todo-keywords
        '((sequence "TODO(t)" "DELEGATED(g)" "SOMEDAY(s)" "WAITING(w)" "|"
                    "DONE(d!)" "CANCELLED(c!)" "REFERENCE(r!)")))
  ;; インライン画像の幅は、#ATTR画像で設定する。
  (set-variable 'org-image-actual-width nil)
  ;; * org-preview-latex-fragment (数式の画像化 C-c C-x C-l) について
  ;;   現在のorg-mode は、直接 latex 命令で数式を生成する
  ;;   "org-create-formula-image-with-dvipng" と、LaTeXからPDFを生成して
  ;;   そこからImageMagickで画像を生成する
  ;;   "org-create-formula-image-with-imagemagick" の２つの方法が用意さ
  ;;   れている。そのうち、..-with-dvipng は、直接 latex 命令を呼び出している。
  ;;   imagemagick は動作が遅い。
  (set-variable 'org-latex-create-formula-image-program 'dvipng) ; imagemagick
  ;; * HTML出力の際は 自動的に色付けをする。
  (set-variable 'org-src-fontify-natively t)
  ;; glyphwiki リンクの設定（ローカルにあればそれを参照してもよい。）
  ;;(add-to-list 'org-link-abbrev-alist
  ;;            ("glyphwiki" . "http://glyphwiki.org/wiki/%s"))
  ;; org-agenda
  (set-variable 'org-agenda-files
        (list (expand-file-name "work/work.org.txt" org-directory)
              (expand-file-name "home/home.org.txt" org-directory)))
  ;;(set-variable 'org-agenda-file-regexp "\\`[^.].*\\.org\\.txt$")
  ;; org-clock
  ;; タイマー管理の自動設定
  ;; (org-clock-persistence-insinuate)
  ;; tags
  ;; (set-variable 'org-tags-alist)
  ;; リファイル
  (set-variable 'org-refile-targets
        '(("~/share/org/agenda.org" . (:level . 1))))
  (set-variable 'org-refile-target-verify-function nil)
  ;; 一ヶ月後の予定を入れる。
  (define-key org-mode-map (kbd "C-c C-M-s")
    (command (org-schedule nil "+4w")))
  ;; デフォルトでは org-babel は使用しない。
  (set-variable 'org-babel-load-languages nil))

;;;; outline.el (minor-mode)
;; org-mode のFAQにある方法
;; http://orgmode.org/worg/org-faq.html#use-visibility-cycling-in-outline-mode
(with-eval-after-load 'outline
  (bind-key "<tab>" 'org-cycle outline-minor-mode-map)
  (bind-key "C-<tab>" 'org-global-cycle outline-minor-mode-map)
  (bind-key "C-c C-f" 'outline-forward-same-level outline-minor-mode-map)
  (bind-key "C-c C-b" 'outline-backward-same-level outline-minor-mode-map)
  (bind-key "C-c C-n" 'outline-next-visible-heading outline-minor-mode-map)
  (bind-key "C-c C-p" 'outline-previous-visible-heading outline-minor-mode-map)
  (bind-key "<tab>" 'org-cycle outline-mode-map)
  (bind-key "S-<tab>" 'org-global-cycle outline-mode-map))

;;;; paren.el
(eval-and-compile (show-paren-mode t))
(set-variable 'show-paren-style 'parenthesis)

;;;; play/gamegrid.el
;; udpate-game-score :: setuid していれば、`games' (または chown したユーザ) しか書き込めないファイルにアクセスできる。
;; % sudo chmod -R 4775  update-game-score*
;; % sudo chown -R games update-game-score*
;;   → update-game-score の onwer が games に、mod が "rwsrwxr-x" になっていることを確認。
;; Emacs はインストール時に /usr/local/var/games/emacs に tetris-scores と snake-scores の２ファイルを用意する。
;; テスト: % /Users/kawabata/cvs/emacs-macport/lib-src/update-game-score -d /usr/local/var/games/emacs/ tetris-scores 1 "extra info"
;; 使い方：
;; 1. スーパーユーザが /usr/local/var/games/emacs/ 以下にゲームディレクトリを用意する。

;;;; play/zone.el
;; zone-pgm-rainbow

(defvar zpr-hue-factor 50)
(defvar zpr-sat 1.0)
(defvar zpr-light 0.5)
(defvar zpr-background "#000000")
(defun zone-pgm-rainbow ()
  (require 'color)
  (cl-loop
   while (not (input-pending-p))
   with k = 0
   do (cl-loop
       for i from (window-start) to (1- (window-end))
       do (add-text-properties
           i (1+ i)
           `(face ((foreground-color
                    . ,(apply 'color-rgb-to-hex
                              (color-hsl-to-rgb
                               (/ (* (% (+ i k) zpr-hue-factor) 1.0) zpr-hue-factor)
                               zpr-sat zpr-light)))
                   (background-color
                    . ,zpr-background)
                   ))))
   (sit-for 0.1)
   (incf k)
   ))

(declare-function zone-choose "init.el")
(defvar zone-programs)
(with-eval-after-load 'zone
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone)))
  (defun zone-rainbow ()
    (interactive)
    (zone-choose "zone-pgm-rainbow"))

  (aset zone-programs 0 'zone-pgm-rainbow)

  ;; スクリーンセーバー代わり
  (unless (equal system-type 'gnu/linux)
    (set-variable 'zone-rainbow-timer
                  (run-with-idle-timer 120 nil 'zone-rainbow)))

  )

;;;; progmodes/ada-mode.el
;; Mac版ada のインストールは、GNATをダウンロードして、
;; ./doinstall スクリプトを実行 → /usr/local/gnat/ にインストールされる。

;;;; progmodes/cc-mode.el (major-mode)
(with-eval-after-load 'cc-mode
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (outline-minor-mode 1)
     ;;(c-toggle-auto-state 1)
     (set-variable 'indent-tabs-mode t)
     (set-variable 'tab-width 4)
     (set-variable 'c-default-style '((c . "gnu") (java . "java"))))))

;;(defconst c-DOE-style
;;  '((c-basic-offset . 4)
;;    (c-comment-only-line-offset . 0)
;;    (c-label-minimum-indentation . 0)
;;    (c-offsets-alist . ((statement-block-intro . +)
;;                        (knr-argdecl-intro . +)
;;                        (label . [0])
;;                        (case-label . *)
;;                        (statement-case-intro . 1)
;;                        (access-label . /)
;;                        (friend . /)
;;                        (arglist-close . 0)
;;                        (extern-lang-open . 0)
;;                        (extern-lang-close . 0)
;;                        (inextern-lang . 0)
;;                        (brace-list-intro . ++)
;;                        (brace-list-close . 0)
;;                        (inline-open . 0)
;;                        ;;(inline-close . 0)
;;                        ))))

;;;; progmodes/compile.el
(bind-key "C-x '" 'compile)

;;;; progmodes/flymake.el
;; 原則として flycheck を用いる。
;;(lazyload () "flymake"
;;  (set-face-background 'flymake-errline "red4")
;;  (set-face-background 'flymake-warnline "dark slate blue"))

;;;; progmodes/grep.el
(if (executable-find "xzgrep")
    (set-variable 'grep-program "xzgrep")
  (if (executable-find "bzgrep")
      (set-variable 'grep-program "bzgrep")))
(bind-key "M-s g" 'grep)

;;;; progmodes/gdb-mi
(with-eval-after-load 'gdb-mi
  (when (or (getenv "LD_LIBRARY_PATH")
            (getenv "DYLD_LIBRARY_PATH"))
    (message "注意！ LD_LIBRARY_PATH等を設定し、シェル起動時の先頭行に警告が出ると、\
GDBは動作しない可能性があります！") (sit-for 2))
  (set-variable 'gdb-many-windows t)
  (set-variable 'gdb-show-main t))

;;;; progmodes/gud.el
;; 将来は realgud に乗り換えることを目指すが、現在は特に
;; C/C++ では gud を使用する。
(defvar gud-minor-mode-map)
(defvar gud-break-main)
(declare-function gud-def "gud" (func cmd key &optional doc)) ;; macro
(with-eval-after-load 'gud
  ;; http://www.youtube.com/watch?v=p7XdkrlFXnU
  ;; https://gist.github.com/chokkan/5693497
  ;; http://www.chokkan.org/lectures/2013c/emacs-gdb.pdf
  (add-hook
   'gdb-mode-hook
   (lambda ()
     (gud-tooltip-mode t)
     ;; メインで止まる関数を定義
     (gud-def gud-break-main "break main" nil "Set breakpoint at main.")))
  ;; Altキーにデバッガを割り当て。
  (bind-key "A-b" 'gud-break gud-minor-mode-map)      ; ブレーク
  (bind-key "A-m" 'gud-break-main gud-minor-mode-map) ; main でブレイク
  (bind-key "A-p" 'gud-print gud-minor-mode-map)      ; カーソル上の変数の中身を表示
  (bind-key "A-n" 'gud-next gud-minor-mode-map)       ; ステップオーバー
  (bind-key "A-s" 'gud-step gud-minor-mode-map)       ; ステップイン
  (bind-key "A-w" 'gud-watch gud-minor-mode-map)      ; 変数ウォッチ
  (bind-key "A-f" 'gud-finish gud-minor-mode-map)     ; ステップアウト
  (bind-key "A-l" 'gud-refresh gud-minor-mode-map)    ; 再描画
  (bind-key "A-c" 'gud-cont gud-minor-mode-map)       ; プログラム実行再開
  (bind-key "A-t" 'gud-until gud-minor-mode-map)      ; 続行
  (bind-key "A-u" 'gud-up gud-minor-mode-map)         ; スタックアップ
  (bind-key "A-d" 'gud-down gud-minor-mode-map)       ; スタックダウン
  (bind-key "A-<" 'gud-up gud-minor-mode-map)         ; スタックアップ
  (bind-key "A->" 'gud-down gud-minor-mode-map)       ; スタックダウン
  ;;(bind-key "A-k" 'gud-kill gud-minor-mode-map)       ; 終了
  ;; ファンクションキーにデバッグ機能を割り当て
  (bind-key "<f1>" 'gud-print gud-minor-mode-map)
  (bind-key "<S-f1>" 'gud-watch gud-minor-mode-map)
  (bind-key "<f2>" 'gud-refresh gud-minor-mode-map)
  (bind-key "<f5>" 'gud-cont gud-minor-mode-map)
  (bind-key "<f6>" 'gud-until gud-minor-mode-map)
  (bind-key "<f9>" 'gub-break gud-minor-mode-map)
  (bind-key "<S-f9>" 'gud-break-main gud-minor-mode-map)
  (bind-key "<f10>" 'gud-next gud-minor-mode-map)
  (bind-key "<f11>" 'gud-step gud-minor-mode-map)
  (bind-key "<C-f10>" 'gud-until gud-minor-mode-map)
  (bind-key "<C-f11>" 'gud-finish gud-minor-mode-map)
  (bind-key "<S-f11>" 'gud-finish gud-minor-mode-map)
  ;;(bind-key "<S-f5>" 'gud-kill gud-minor-mode-map)

  ;;(defun gdb-set-clear-breakpoint ()
  ;;  (interactive)
  ;;  (if (or (buffer-file-name) (eq major-mode 'gdb-assembler-mode))
  ;;      (if (or
  ;;           (let ((start (- (line-beginning-position) 1))
  ;;                 (end (+ (line-end-position) 1)))
  ;;             (catch 'breakpoint
  ;;               (dolist (overlay (overlays-in start end))
  ;;                 (if (overlay-get overlay 'put-break)
  ;;                     (throw 'breakpoint t)))))
  ;;           (eq (car (fringe-bitmaps-at-pos)) 'breakpoint))
  ;;          (gud-remove nil)
  ;;        (gud-break nil))))

  ;;(defun gud-kill ()
  ;;  "Kill gdb process."
  ;;  (interactive)
  ;;  (with-current-buffer gud-comint-buffer (comint-skip-input))
  ;;  (kill-process (get-buffer-process gud-comint-buffer)))
  )

;;;; progmodes/hideshow.el
(with-eval-after-load 'hideshow
  (set-variable 'hs-hide-comments-when-hiding-all nil)
  (pushnew '(ruby-mode
      "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
            (lambda (arg) (ruby-end-of-block)) nil)
           hs-special-modes-alist))

;;;; progmodes/inf-lisp.el
(with-eval-after-load 'inf-lisp
  (set-variable 'inferior-lisp-program (or (executable-find "ccl")
                                  (executable-find "sbcl"))))

;;;; progmodes/lisp-mode.el
(bind-key "C-c M-l" 'lisp-interaction-mode)
(pushnew '("^Cask" . emacs-lisp-mode) auto-mode-alist :test 'equal)

;;;; progmodes/make-mode.el
;;(add-hook 'makefile-mode-hook
;;          (function (lambda ()
;;                      (fset 'makefile-warn-suspicious-lines 'ignore))))

;;;; progmodes/opascal.el
;; Emacs 24.4以降。
(autoload 'opascal-mode "opascal")
(pushnew '("\\.\\(pas\\|dpr\\|dpk\\)\\'" . opascal-mode) auto-mode-alist
         :test 'equal)

;;;; porgmodes/prog-mode.el
;; Since emacs 24.4
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;;;; progmodes/scheme.el
(with-eval-after-load 'scheme
  (set-variable 'scheme-program-name (executable-find "gosh")))

;;;; progmodes/sh-script.el
(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook 'outline-minor-mode))

;;;; progmodes/sql.el
;; 主にsqliteと組み合わせて使うことを想定する。
;; http://www.emacswiki.org/cgi-bin/wiki.pl?SqlMode
;; M-x sql-help, M-x sql-sqlite
(autoload 'master-mode "master" "Master mode minor mode." t)

;; SQL mode に入った時点で sql-indent / sql-complete を読み込む
(with-eval-after-load 'sql
  ;; デフォルトのデータベースの設定
  (set-variable 'sql-user nil)
  (set-variable 'sql-sqlite-program "sqlite3")
  (set-variable 'sql-database "sqlite")
  ;; SQLi の自動ポップアップ
  (set-variable 'sql-pop-to-buffer-after-send-region t)
  ;; 「;」をタイプしたら SQL 文を実行
  (set-variable 'sql-electric-stuff 'semicolon)
  ;; (add-hook 'sql-mode-hook
  ;;           (lambda ()
  ;;             (local-set-key "\C-cu" 'sql-to-update) ; sql-transform
  ;;             ;; master モードを有効にし、SQLi をスレーブバッファにする
  ;;             (master-mode t)
  ;;             (master-set-slave sql-buffer)))
  ;;(add-hook 'sql-set-sqli-hook
  ;;          (lambda () (master-set-slave sql-buffer)))
  ;;(add-hook 'sql-interactive-mode-hook
  ;;          (lambda ()
  ;;            ;; comint 関係の設定
  ;;            (set-variable 'comint-input-autoexpand t)
  ;;            (set-variable 'comint-output-filter-functions
  ;;                  'comint-truncate-buffer)))
  ;;
  ;;;; SQL モードから SQLi へ送った SQL 文も SQLi ヒストリの対象とする
  ;;(defadvice sql-send-region (after sql-store-in-history)
  ;;  "The region sent to the SQLi process is also stored in the history."
  ;;  (let ((history (buffer-substring-no-properties start end)))
  ;;    (save-excursion
  ;;      (set-buffer sql-buffer)
  ;;      (message history)
  ;;      (if (and (funcall comint-input-filter history)
  ;;               (or (null comint-input-ignoredups)
  ;;                   (not (ring-p comint-input-ring))
  ;;                   (ring-empty-p comint-input-ring)
  ;;                   (not (string-equal (ring-ref comint-input-ring 0)
  ;;                                      history))))
  ;;          (ring-insert comint-input-ring history))
  ;;      (set-variable 'comint-save-input-ring-index comint-input-ring-index)
  ;;      (set-variable 'comint-input-ring-index nil))))
  ;;(ad-activate 'sql-send-region)
  )

;;;; progmodes/which-func.el
;; Cやemacs-lisp で、現在どの関数にいるかを表示する。
(which-function-mode 1)
;; モードラインは狭いので header-line に表示させる。
(setq-default header-line-format
              '((which-function-mode ("" which-func-format " "))))

;; モードラインから削除する。（どうせ見えない）
(setq mode-line-misc-info
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;;;; recentf.el
;; 最近開いたファイルの一覧を表示。helm.el と組み合わせる。
(bind-key "C-c F" 'recentf-open-files)
(recentf-mode)
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)

;;;; register.el
(setq register-separator "/")

;;;; savehist.el
;; ミニバッファのヒストリを保存する。
(savehist-mode 1)

;;;; saveplace.el
;; ファイルでのカーソルの位置を保存しておく
;; Emacs 24.4 からデフォルトで以下の設定に。
;;(setq save-place-file
;;      (locate-user-emacs-file "places")))

;;;; server.el
(when (not (equal system-type 'windows-nt))
  (server-start))

;;;; shell.el
;; ファイル名に使われる文字
(defvar shell-file-name-chars)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

(bind-key "C-c s" 'shell)

(defun cmdproxy-shell ()
  "Windowsにおいて、cmdproxyでシェルを起動する."
  (interactive)
  (let ((shell-file-name "cmdproxy")
        (explicit-shell-file-name "cmdproxy"))
    (shell (generate-new-buffer "*cmdproxy*"))))

;; | Functions           | UTF-8               | eight-bit       |
;; |---------------------+---------------------+-----------------|
;; | string-as-unibyte   | same individual     | single byte     |
;; | string-make-unibyte | nonascii & low 8bit | low 8bit        |
;; | string-to-unibyte   | same individual     | same individual |

;; | Functions             | utf-8                     | non-utf-8        |
;; |-----------------------+---------------------------+------------------|
;; | string-as-multibyte   | same individual           | eight-bit        |
;; | string-make-multibyte | unibyte-char-to-multibyte | unibyte-char-... |
;; | string-to-multibyte   | eight-bit                 | eight-bit        |

;; 以下の関数は comint-input-ring-separator を適切に設定すれば不要。
;;(defun zsh-arrange-history (hist)
;;  "zshのヒストリデータのタイムスタンプとメタキャラを除去する。"
;;  (when hist
;;    (replace-regexp-in-string "^.+?;" "" hist)))
;;
;;(setq shell-mode-hook
;;      ;; ヒストリファイルのタイムスタンプとメタキャラ除去
;;      (lambda ()
;;        (when (and (stringp comint-input-ring-file-name)
;;                   (string-match "zsh_history" comint-input-ring-file-name))
;;          (let ((array (cddr comint-input-ring)))
;;            (do ((i 0 (+ i 1))) ((> i (1- (length array))))
;;              (aset array i (zsh-arrange-history (aref array i))))))))

;; C-u M-x shell
;;(defun new-shell ()
;;  (interactive)
;;  (let ((shell (get-buffer "*shell*")))
;;    (if (null shell) (shell)
;;      (set-buffer shell)
;;      (rename-uniquely)
;;      (shell))))

;;;; speedbar.el
;; 不明なファイルは非表示。
(with-eval-after-load 'speedbar
  (set-variable 'speedbar-show-unknown-files nil))

;;;; term.el
;; エスケープ・シーケンスは、/etc/e/eterm-color.ti に従う。
(setq term-buffer-maximum-size 4096)

;;;; term/common-win.el
;; [Emacs 24.3] x-select-enable-clipboardはデフォルトでtになった。
;;(when (eq window-system 'x)
;;  (setq x-select-enable-clipboard t))

;;;; term/mac-win.el
;; ミニバッファに入る際に自動的に英語モードにする。
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode))
;; デフォルトでフォントパネルを表示する。
;; → フォント選択後、フォントセットDBが崩れるので一旦中止。
;; (when (fboundp 'mac-font-panel-mode)
;;   (mac-font-panel-mode))

;; アップルイベント関連 :: mac-ae-*
;; :: mac-odb-*


;; ターミナル上で Meta+modifier のprefixを動かす場合は以下のパッチを適用。
;; === modified file 'lisp/term/mac-win.el'
;; *** lisp/term/mac-win.el        2014-11-21 01:03:43 +0000
;; --- lisp/term/mac-win.el        2014-12-02 00:08:16 +0000
;; ***************
;; *** 1299,1304 ****
;; --- 1299,1307 ----
;;   (setq mac-ts-active-input-overlay (make-overlay 1 1))
;;   (overlay-put mac-ts-active-input-overlay 'display "")
;; 
;; + (defvar mac-ts-active-input-string ""
;; +   "String of text in Mac TSM active input area.")
;; +
;;   (defface mac-ts-caret-position
;;     '((t :inverse-video t))
;;     "Face for caret position in Mac TSM active input area.
;; ***************
;; *** 1441,1447 ****
;;                 (delete-region (+ (point-min) (car replacement-range))
;;                                (+ (point-min) (car replacement-range)
;;                                   (cdr replacement-range)))
;; !             (error nil)))))))
;; 
;;   (defvar mac-emoji-font-regexp "\\<emoji\\>"
;;     "Regexp matching font names for emoji.
;; --- 1444,1451 ----
;;                 (delete-region (+ (point-min) (car replacement-range))
;;                                (+ (point-min) (car replacement-range)
;;                                   (cdr replacement-range)))
;; !             (error nil))))
;; !       (setq mac-ts-active-input-string active-input-string))))
;; 
;;   (defvar mac-emoji-font-regexp "\\<emoji\\>"
;;     "Regexp matching font names for emoji.
;; ***************
;; *** 1492,1497 ****
;; --- 1496,1502 ----
;;                                       '(mac-ts-active-input-string nil)
;;                                       msg)
;;               (message "%s" msg))))))
;; +     (setq mac-ts-active-input-string "")
;;       (if replacement-range
;;         (condition-case nil
;;             ;; Strictly speaking, the replacement range can be out of
;; ***************
;; *** 1547,1553 ****
;;   (defun mac-auto-ascii-select-input-source ()
;;     "Select the most-recently-used ASCII-capable keyboard input source.
;;   Expects to be added to normal hooks."
;; !   (if (eq (terminal-live-p (frame-terminal)) 'mac)
;;         (mac-select-input-source 'ascii-capable-keyboard)))
;; 
;;   (defun mac-auto-ascii-setup-input-source (&optional _prompt)
;; --- 1552,1558 ----
;;   (defun mac-auto-ascii-select-input-source ()
;;     "Select the most-recently-used ASCII-capable keyboard input source.
;;   Expects to be added to normal hooks."
;; !   (if (= (length mac-ts-active-input-string) 0)
;;         (mac-select-input-source 'ascii-capable-keyboard)))
;; 
;;   (defun mac-auto-ascii-setup-input-source (&optional _prompt)
;; ***************
;; *** 1578,1584 ****
;;     :group 'mac
;;     :package-version '(Mac\ port . "5.2")
;;     (if mac-auto-ascii-mode
;; !       (progn
;;         (map-keymap (lambda (event definition)
;;                       (if (and (keymapp definition) (integerp event)
;;                                (not (eq event ?\e)))
;; --- 1583,1589 ----
;;     :group 'mac
;;     :package-version '(Mac\ port . "5.2")
;;     (if mac-auto-ascii-mode
;; !       (when (eq (terminal-live-p (frame-terminal)) 'mac)
;;         (map-keymap (lambda (event definition)
;;                       (if (and (keymapp definition) (integerp event)
;;                                (not (eq event ?\e)))

;;;; textmodes/artist.el
;; | key       | Picture   |
;; |-----------+-----------|
;; | C-c C-a P | polyline  |
;; | C-c C-a r | rectangle |
;; | C-c C-a V | vaporize  |
;; | C-c C-k   | kill-rect |
;; | C-c C-y   | yank-rect |
(bind-key "C-c A" 'artist-mode)

;;;; textmodes/bibtex.el

;; BibTeXファイルの基本構成

;; | 文献フォルダ                | BibTeXフォルダ                   |
;; |-----------------------------+----------------------------------|
;; | ~/Documents/Books/          | ~/Bibliography/Documents/        |
;; | ~/Resources/Books/          | ~/Bibliography/Resources/        |
;; | ~/share/Books_and_Papers/ | ~/Bibliography/Books_and_Papers/ |
;; | 非デジタル文献              | ~/Bibliography/Books/            |
;; |-----------------------------+----------------------------------|
;; | 全ファイルの結合            | ~/Bibliography/all.bib           |

;; - BibTeXディレクトリ場所は BIBINPUTS 環境変数で管理。
;; - BibTeXディレクトリの"~/Bibliography/" を除いた場所が実際の文献の場所。
;;   （ただしDropBoxのフォルダの場合はそれを除く。）
;; - 文献ディレクトリのサブディレクトリ名とBibTeXファイル名は一致。
;; - 全てのbibファイルは、"~/Bibliography/all.bib" に集める。

;; - 文献のフォルダ内の移動に伴い、BibTeXファイルも自動更新する。
;;   (M-x tkw-update-bibtex-files)

(defun tkw-collect-bibtex-files ()
  (shell-command
   (concat
    "cat ~/share/Bibliography/**/*.bib~all.bib"
    "  > ~/share/Bibliography/all.bib ")))

;; - BibTeX 基本要素について
;; |        | file-path                    | files                       |
;; |--------+------------------------------+-----------------------------|
;; | bibtex | bibtex-file-path             | bibtex-files                |
;; |        | BIBINPUTS 環境変数           |                             |
;; |        | bibtex-string-file-path      |                             |
;; |        | BIBINPUTS 環境変数           |                             |
;; | reftex | BIBINPUTS 環境変数           | reftex-default-bibliography |
;; |        |                              | → tkw-bibtex-files          |
;; | refer  | refer-bib-directory          | refer-bib-files             |
;; |        | →'bibinputs                 | → tkw-bibtex-files          |
;; | ebib   | ebib-preload-bib-search-dirs | ebib-preload-bib-files      |
;; |        | → tkw-bibtex-directories     | → tkw-bibtex-files          |

;; 一旦、全てのBibTeXファイルは、all.bib につなげる。
;; % cat *.bib~all.bib > all.bib

(defvar tkw-bibtex-file-path nil)
(defvar tkw-bibtex-directories nil)
(defvar tkw-bibtex-files nil)

(defun tkw-bibtex-setup ()
  "BibTeXファイルのセットアップ"
  (interactive)
  (setq tkw-bibtex-file-path (getenv "BIBINPUTS"))
  (setq tkw-bibtex-directories
        (and tkw-bibtex-file-path (split-string tkw-bibtex-file-path ":")))
  (setq tkw-bibtex-files
        (loop for dir in tkw-bibtex-directories
              nconc
              (if (file-directory-p dir)
                  (let ((default-directory dir)) (file-expand-wildcards "*.bib"))
                (message "BIBDIR %s does not exist." dir)
                nil)))
  (callf2 remove "all.bib" tkw-bibtex-files))

(tkw-bibtex-setup)

;; - biblatex

;; +-- mvbook ---------+-- book --------+-- inbook
;; |                   |                +-- bookinbook
;; |                   |                +-- suppbook
;; |                   +-- booklet
;; +-- mvcollection ------ collection --+-- incollection
;; |                                    +-- suppcollection
;; +-- manual
;; +-- misc
;; +-- online
;; |   (electronic)
;; +-- patent
;; +-- periodical
;; +-- mvproceedings ----- proceedings ----- inproceedings
;; |                                         (conference)
;; +-- mvreference ------- reference ------- inreference
;; +-- report
;; |   (techreport)
;; +-- set
;; +-- thesis
;; |   (masterthesis)
;; |   (phdthesis)
;; +-- unpublished

;; BibLaTeXを使う。
(add-hook 'bibtex-mode-hook
          (lambda () (setq outline-regexp "[ \t]*\\(@\\|title\\)"
                       ;; fill はさせない。
                       fill-column 1000)))

(declare-function bibtex-set-dialect "bibtex" (&optional dialect local))
(with-eval-after-load 'bibtex
  (defvar bibtex-biblatex-entry-alist)
  ;; BibLaTeX の拡張
  (let ((misc (assoc-default "Misc" bibtex-biblatex-entry-alist))

        (types '("artwork" "audio" "bibnote" "commentary" "image" "jurisdiction"
                 "legislation" "legal" "letter" "movie" "music" "performance"
                 "review" "softare" "standard" "video" "map")))
    (dolist (type types)
      (pushnew (cons type misc) bibtex-biblatex-entry-alist
               :test 'equal)))
  (bibtex-set-dialect 'biblatex)
  ;; BibLaTeX ファイルは、環境変数BIBINPUTSから取得。
  (set-variable 'bibtex-files 'bibtex-file-path)
  (set-variable 'bibtex-entry-format
                ;; BibTeXの整形
                '(opt-or-alts numerical-fields whitespace inherit-booktitle
                  last-comma delimiters unify-case sort-fields))
        ;; `=' で揃えるか、値で揃えるか。
  (set-variable 'bibtex-align-at-equal-sign nil)
  (set-variable 'bibtex-autofill-types nil)
  ;;(set-variable 'bibtex-entry-alist bibtex-biblatex-entry-alist)
  (set-variable 'bibtex-field-indentation 2)
  (set-variable 'bibtex-include-OPTkey nil)
  (set-variable 'bibtex-search-entry-globally t)
  (set-variable 'bibtex-text-indentation 14)

;;(setq bibtex-entry-type
;;      (concat "@[ \t]*\\(?:"
;;              (regexp-opt (mapcar 'car bibtex-entry-field-alist)) "\\)"))
;;(setq bibtex-any-valid-entry-type
;;      (concat "^[ \t]*@[ \t]*\\(?:"
;;              (regexp-opt
;;               (append '("String" "Preamble")
;;                       (mapcar 'car bibtex-entry-field-alist))) "\\)"))
;;(setq bibtex-entry-head
;;      (concat "^[ \t]*\\("
;;              bibtex-entry-type
;;              "\\)[ \t]*[({][ \t\n]*\\("
;;              bibtex-reference-key
;;              "\\)"))
;;(setq bibtex-entry-maybe-empty-head
;;      (concat bibtex-entry-head "?"))
;; (setq bibtex-field-delimiters 'double-quotes)

;; 厄介な問題：

;; bibtex-field-delimiters が、braces だと、abstractの中でカッコの対応
;; がとれていない場合（たとえば (...》など）エラーになる。稀に機械的に
;; 取得したデータにAbstractにそういうものが紛れ込むそのため、
;; double-quotes にする。
  (setq bibtex-field-delimiters 'double-quotes)
  (setq bibtex-user-optional-fields
        '(("file"   "PD/DjVu File location (ignored)")
          ("access" "文献を入手・借入した日 (ignored)")
          ("library" "文献のある図書館・または書庫での位置 (ignored)")
          ("start" "文献を読み始めた日 (ignored)")
          ("finish" "文献を読み終えた日 (ignored)")
          ("review" "文献に対する批評・ノート (ignored)")
          ("ranking" "文献に対する評価（５段階） (ignored)")
          ("attribute" "文献の属性、フリーキーワード (ignored)")))) ;; end bibtex

;;;; textmodes/css-mode.el
(with-eval-after-load 'css-mode
  (set-variable 'css-indent-offset 2)) ;; TAB値を8にしておく。

;;;; textmodes/flyspell.el
;; 強力無比なスペルチェッカ。 設定はispellを確認。
;; M-x flyspell-mode
;; C-, 移動 C-. 修正
(when (executable-find "aspell")
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;; textmodes/ispell.el
;; spell check は aspell で行う。 (ispell-program-name)
;; % sudo port install aspell aspell-dict-en
(with-eval-after-load 'ispell
  (pushnew '("[^\000-\377]") ispell-skip-region-alist :test 'equal))

;;;; textmodes/page-ext.el
;; ^L で区切られた領域をnarrowingして一覧表示・ジャンプ。
(autoload 'pages-directory "page-ext" "pages" t)
;;(bind-key "C-c P" 'pages-directory)

;;;; textmodes/paragraphs.el
;; fill の際に文章の終わりはピリオド＋１スペースに認識させる。
(setq sentence-end-double-space nil)

;;;; textmodes/picture.el
;; picture の日本語に関する課題
;; - 日本語文字の挿入・削除の際に、カーソル後方の位置がずれる。
;; JIS罫線は以下を使う場合：
;; ┓┛┗┏┃━┣┻┫┳╋
;; ┐┘└┌│─├┴┤┬┼
;; ┷┯┠┨╂┿┝┥┰┸
;;(defun picture-draw-thick-rectangle ()
;;  (let ((picture-rectangle-ctl ?┏)
;;        (picture-rectangle-ctl-2 (?┏ . )
;;        (picture-rectangle-ctr ?┓)
;;        (picture-rectangle-cbl ?┗)
;;        (picture-rectangle-cbr ?┛)
;;        (picture-rectangle-v ?┃)
;;        (picture-rectangle-h ?━))

;;;; textmodes/refer.el
(with-eval-after-load 'refer
  (set-variable 'refer-bib-directory 'bibinputs) ; BIBINPUTS環境変数ディレクトリ
  (set-variable 'refer-bib-files 'dir))          ; ディレクトリ内の全 *.bib ファイル

;;;; textmodes/reftex.el
;; 色々と引用文献を楽に入力するようにする。
;; reftex-browse （スペースで該当Bibファイルにジャンプ）
(defvar reftex-comment-citations)
(with-eval-after-load 'reftex
  (set-variable
   'reftex-cite-format
   '((?b . "[[bib:%l][%l-bib]]")
     (?n . "[[note:%l][%l-note]]")
     (?t . "%t")
     (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[bib:%l][%l-bib]]")))
  (set-variable 'reftex-default-bibliography tkw-bibtex-files)
  (set-variable 'reftex-cite-format-builtin '((default "Default macro %t \\cite{%l}"
                                       "%t \\cite[]{%l}")))
  (set-variable 'reftex-plug-into-AUCTeX t))

(defun reftex-browse ()
  (interactive) (reftex-citation t))

;; Wikipedia 執筆用のreftex引用を用意する。
(defvar reftex-cite-format)
(defun tkw-reftex-wikipedia-reference ()
  (interactive)
  (require 'reftex)
  (let ((reftex-cite-format
         (concat
          ;; http://en.wikipedia.org/wiki/Wikipedia:Citation_templates
          "<ref>"
          "{{Citation"
          " | author = %a" ;; first author only
          " | publisher = %u"
          ;; " | last ="
          ;; " | first ="
          ;; " | author-link ="
          ;; " | last2 ="
          ;; " | first2 ="
          ;; " | author2-link ="
          " | title = %t"
          " | journal = %j"
          ;; " | newspaper = "
          " | volume = %v"
          ;; " | issue ="
          " | pages = %p"
          ;; " | date ="
          ;; " | origyear ="
          " | year = %y"
          " | month ="
          ;; " | language ="
          ;; " | url = "
          ;; " | jstor ="
          ;; " | archiveurl ="
          ;; " | archivedate ="
          ;; " | doi ="
          ;; " | id ="
          ;; " | isbn="
          ;; " | mr ="
          ;; " | zbl ="
          ;; " | jfm ="
          " }}"
          "</ref>"))
        (reftex-comment-citations t))
    (reftex-citation)))

;;;; textmodes/sgml-mode.el
(with-eval-after-load 'sgml-mode
  (add-hook 'sgml-mode-hook
            (lambda () (setq tab-width 2))))

;;;; textmodes/table.el
;; (参照) http://emacswiki.org/emacs/TableMode
;; M-x table-insert
;; C-c C-c !   table-fixed-width-mode
;; C-c C-c #   table-query-dimension
;; C-c C-c *   table-span-cell
;; C-c C-c +   table-insert-row-column
;; C-c C-c -   table-split-cell-vertically
;; C-c C-c :   table-justify
;; C-c C-c <   table-narrow-cell
;; C-c C-c >   table-widen-cell
;; C-c C-c ^   table-generate-source
;; C-c C-c {   table-shorten-cell
;; C-c C-c |   table-split-cell-horizontally
;; C-c C-c }   table-heighten-cell
;; C-c C-c <I> table-backward-cell
;; C-c C-c <i> table-forward-cell
;; C-c C-c <j> *table--cell-newline-and-indent
;; C-c C-c <m> *table--cell-newline
;; +-----+-----+-----+
;; |aaa  |bbb  |cde  |
;; +-----+-----+-----+
;; |     |     |     |
;; +-----+-----+-----+
;; |     |     |     |
;; +-----+-----+-----+
;; insert a new table.el table           C-c ~
;; recognize existing table.el table     C-c C-c
;; convert table (Org-mode <-> table.el) C-c ~

;; M-x table-capture
;; M-x table-unrecognize
;; M-x table-recognize

;;;; thingatpt.el
;; `javascript:' と 'ttp://' の追加
(with-eval-after-load 'thingatpt
  (setq thing-at-point-url-regexp
        (mapcar (lambda (x) (push x thing-at-point-uri-schemes))
                '("javascript:" "ttp:"))
        thing-at-point-url-regexp
        (concat "\\<\\("
                (mapconcat 'identity thing-at-point-uri-schemes "\\|") "\\)"
         thing-at-point-url-path-regexp)))
(defadvice thing-at-point-url-at-point (after support-omitted-h activate)
  (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
    (callf2 concat "h" ad-return-value)))

;;;; time-stamp.el
;; バージョン管理に使用。
(add-hook 'before-save-hook 'time-stamp)

;;;; uniquify.el
;; バッファ名にディレクトリ名を追加する。
(eval-and-compile (require 'uniquify))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-min-dir-content 1)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified

;;;; vc/add-log.el
;; ChangeLogの英語のパターン
;; - ピリオドを付けない。
;; - 大文字で始める。
;; - 過去分詞でも現在分詞でも構わない。
;; 動詞
;; - Add support for XYZ
;; - Avoid, Circumvent A
;; - Change A to B
;; - Disambiguate
;; - Enable,Disable A of B
;; - Enhance A
;; - Extend A
;; - Extract A from B
;; - Fix possible XYZ
;; - Implement A
;; - Introduce A
;; - Modify A to B
;; - Optimized A
;; - Reduce A of B
;; - Refactor A
;; - Remove, Get rid of (obsolete) A
;; - Refer A (参照する)
;; - Resurrect よみがえる
;; - Retrieve A
;; - Revert A to B
;; - Revise A
;; - Tweak A (微調整する)
;; - Update A for B
;; - Upgrade A to version B
;; - XYZ is now ZZZed (now+過去分詞)
;; 名詞
;; - usability
;; 形容詞
;; - undesireble 好ましくない
;; - insight of ～ ～の視点で
;; - quite とても（veryのかわりにつかえる
;; - literally 文字通り
;; - exactly
;; - in favor of ～に賛成して, 何かに沿って
;; - no longer used もはや使われてない

;;;; vc/ediff.el
(declare-function ediff-jump-to-difference "ediff-util" (difference-number))
(with-eval-after-load 'ediff
  (set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
  (defun ediff-at-point ()
    (interactive)
    (let* ((overlays (overlays-at (point)))
           ediff-num)
      (dolist (overlay overlays)
        (setq ediff-num (plist-get (overlay-properties overlay) 'ediff-diff-num)))
      (unless (and (integerp ediff-num)
                   (< 0 ediff-num))
        (error "Not a diff position!"))
      (with-current-buffer "*Ediff Control Panel*"
        (ediff-jump-to-difference (1+ ediff-num))))))

;;;; vc/vc.el
(delete 'Bzr vc-handled-backends)
(delete 'Git vc-handled-backends)

;;;; view.el
;; 一週間以上古いファイルは、自動的にread-onlyにする。
(defun tkw-read-only-if-old-file ()
  (let* ((file-name (buffer-file-name))
         (modification-time
          (when file-name
            (elt (file-attributes file-name) 5))))
    (when (and modification-time
               (< 6 (time-to-day-in-year
                     (time-subtract (current-time)
                                    modification-time)))
               ;; (org-mode) "_archive" で終わる名前は対象外
               (not (string-match "_archive$" (buffer-file-name)))
               ;; (org-feed) "feeds.org" を含む名前は対象外
               (not (string-match "feeds\\.org$" (buffer-file-name)))
               ;; (org-feed) "org-clock-save" を含む名前は対象外
               (not (string-match "org-clock-save" (buffer-file-name)))
               ;; (org-feed) "Cask" を含む名前は対象外
               (not (string-match "Cask" (buffer-file-name)))
               ;; (package) "-autoloads" を含む名前は対象外
               (not (string-match "-autoloads" (buffer-file-name)))
               ;; (bookmark+)
               (not (string-match "bookmarks" (buffer-file-name)))
               (not (string-match "emacs-bmk-bmenu" (buffer-file-name))))
      (read-only-mode))))
;; 切り替え関数の定義
(defun turn-on-old-file-read-only () (interactive)
  (add-hook 'find-file-hook 'tkw-read-only-if-old-file)
  (message "old-file-read-only is on."))
(defun turn-off-old-file-read-only () (interactive)
  (remove-hook 'find-file-hook 'tkw-read-only-if-old-file)
  (message "old-file-read-only is off."))
(defun toggle-old-file-read-only () (interactive)
  (if (memq 'tkw-read-only-if-old-file find-file-hook)
      (turn-off-old-file-read-only)
    (turn-on-old-file-read-only)))
(turn-on-old-file-read-only)

;;;; wdired.el
(autoload 'wdired-change-to-wdired-mode "wdired" nil t)
(with-eval-after-load 'dired
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map))

;;;; windmove.el
;; (eval-and-compile (windmove-default-keybindings)) ; Shift + ↑←↓→ で、移動。
(bind-key "M-N" 'windmove-down)
(bind-key "M-P" 'windmove-up)
(bind-key "M-F" 'windmove-right) ;; Mac OSに奪われるので要変更。
(bind-key "M-B" 'windmove-left)
(with-eval-after-load 'windmove
  ;; 一番左のウィンドウから左への移動は一番右へいく。
  (set-variable 'windmove-wrap-around t))

;;;; window.el (todo)
(defvar scroll-with-cursor t)

(defun tkw-scroll-up ()
  (interactive)
  (scroll-up 1)
  (when scroll-with-cursor (forward-line 1)))

(defun tkw-scroll-down ()
  (interactive)
  (scroll-down 1)
  (when scroll-with-cursor (forward-line -1)))

(defun tkw-toggle-scroll-with-cursor ()
  (interactive)
  (callf not scroll-with-cursor))

(bind-key "M-n" 'tkw-scroll-up)
(bind-key "M-p" 'tkw-scroll-down)

;;;; winner.el
;; (winner-mode t)

;;;; w32-ime.el
(declare-function w32-ime-initialize "w32-ime")
(declare-function ime-get-mode "w32-ime")
(declare-function ime-force-off "w32-ime")
(declare-function wrap-function-to-control-ime "w32-ime")
(defvar w32-ime-mode-line-state-indicator-list)
(defvar w32-ime-buffer-switch-pa)
(when (functionp 'w32-ime-initialize)
  (set-variable 'default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  ;;(setq-default w32-ime-mode-line-state-indicator "[--]")
  (set-variable 'w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  ;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  ;; バッファ切り替え時にIME状態を引き継ぐ
  (set-variable 'w32-ime-buffer-switch-pa nil)
  ;; 日本語IMEと仲良くする設定
  (when (functionp 'ime-get-mode)
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (and (ime-get-mode)
                     (ime-force-off))))
    (wrap-function-to-control-ime 'universal-argument t nil)
    (wrap-function-to-control-ime 'read-string nil nil)
    (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
    (wrap-function-to-control-ime 'y-or-n-p nil nil)
    (wrap-function-to-control-ime 'yes-or-no-p nil nil)
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
    (eval-after-load "ange-ftp"
      '(wrap-function-to-control-ime 'ange-ftp-get-passwd nil nil)))
  ;; (load "fake-fep" t)

  ;; IME のオン・オフでカーソル色を変える
  ;; TODO 暗い背景と明るい背景で色を変えること
  (defcustom w32-ime-on-color
    (if (string= frame-background-mode 'dark)
        "SkyBlue"
      "light blue")
    "IMEがオン時のカーソル色."
    :type 'string
    :group 'Meadow)

  (defcustom w32-ime-off-color
    (if (string= frame-background-mode 'dark)
        "LemonChiffon"
      "bisque3")
    "IMEがオフ時のカーソル色."
    :type 'string
    :group 'Meadow))

;;;; woman.el
(with-eval-after-load 'woman
  (set-variable 'woman-cache-filename "~/.wmncach.el"))
;;(defalias 'man 'woman)



;;; 個人用ツール
;;;; 事前にカスタム設定を読み込み
(load custom-file t)
;;;; eval-and-replace
;; S式を結果に置き換える。
;; https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun tkw-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;;; clear-char-code-property
;; add-char-code-property
;; -> (pushnew val (get-char-code-property ch prop))
(defun clear-char-code-property (prop)
  "全文字から、 PROP プロパティを削除する. "
  (map-char-table
   (lambda (char plist)
     (remf plist prop)
     (aset char-code-property-table char plist))
   char-code-property-table))

;;;; データ操作
;; 以下の関数は必要に応じてコピペして使用する。
(defun map-plist (plist func)
  "PLISTの <key,value> にFUNCを適用した結果をリストで返す。"
  (let (result)
    (while plist
      (push (funcall func (car plist) (cadr plist)) result)
      (callf cddr plist))
    result))

(defun assoc-all (key alist &optional test)
  "ALISTの key が、KEYである全てのvalueを返す。TESTで比較しても良い。"
  (loop for cons in alist
        if (funcall (or test 'equal) (car cons) key)
        collect (cdr cons)))

(defun rassoc-all (key alist &optional test)
  "ALIST の value が KEY である全ての key を返す。TESTで比較しても良い。"
  (loop for cons in alist
        if (funcall (or test 'equal) (cdr cons) key)
        collect (car cons)))

;;;; リスト操作
;; 基本的に dash ライブラリを用いる。
(defun tkw-cartesian-product (head &rest tails)
  "Make a list of combinations of list in arguments (HEAD and TAILS).
That means to create the all possible combinations of sequences.
For example, if the first sequence contains 3 elements, and the
second one contains 5 elements, then 15 lists of length 2 will be
returned."
  (if tails
      (cl-mapcan (lambda (y) (mapcar (lambda (x) (cons x y)) head))
                 (apply 'tkw-cartesian-product tails))
    (mapcar 'list head)))

(defun tkw-rotate-to (list target)
  (require 'dash)
  (-rotate (- (cl-position target list)) list))
; (tkw-rotate-to '(red blue green yellow) 'green) -> (green yellow red blue)

;;;; テキストプロパティ・オーバレイ操作
(defun remove-overlays-region (from to)
  "指定した領域のoverlayを全て除去する。"
  (interactive "r")
  (remove-overlays from to))

(defun remove-text-properties-region (from to)
  "指定した領域のpropertiesを全て除去する。"
  (interactive "r")
  (set-text-properties from to nil))

(defun put-text-property-region (from to property value)
  "指定した領域に指定した名前のテキストプロパティを設定する。"
  (interactive "*r\nSproperty name: \nSproperty value: ")
  (put-text-property from to property value))

(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;;;; スクラッチバッファ
;; *scratch*バッファは削除させない (mmemo-buffersより)
(defun tkw-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (case arg (0 (message "*scratch* is cleared up."))
              (1 (message "another *scratch* is created")))))

(defun tkw-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (tkw-make-scratch 0) nil)
              t)))
;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(add-hook 'after-save-hook
          (lambda ()
            (unless (member "*scratch*" (tkw-buffer-name-list))
              (tkw-make-scratch 1))))

;;;; モードラインに私用表示
(define-minor-mode docomo-usage-mode
  "ドコモのパケホーダイの利用目安量."
  :global t
  :init-value nil)

(setf (alist-get 'docomo-usage-mode header-line-format)
      '((:eval
         ;; 7Gbyte を 30日で分割した場合の、現在日における２桁数字。
         (format
          "利用目安量 %sGb "
          (/ (* (/ 70 30)
                (string-to-number
                 (format-time-string "%d" (current-time)))) 10.0)))))
(setq-default header-line-format header-line-format)

(docomo-usage-mode 1)

;;;; DropBox
(declare-function ffap-read-file-or-url "ffap" (prompt guess))
(declare-function ffap-guesser "ffap" nil)
(defun find-file-in-dropbox (&optional filename)
  "FILENAME を DropBox で開く。"
  ;; ffap 機能の活用例。
  (interactive)
  (or filename
      (setq filename
            (ffap-read-file-or-url
             "Find a file in DropBox Folder: " (ffap-guesser))))
  (setq filename (expand-file-name (file-truename filename)))
  (if (string-match "Dropbox/\\(.*\\)$" filename)
      (if (file-directory-p filename)
          (browse-url (concat "https://www.dropbox.com/home/"
                              (match-string 1 filename)))
        (browse-url (concat "https://www.dropbox.com/revisions/"
                            (match-string 1 filename))))
    (message "Not Dropbox Directory! %s -- " filename)))
(bind-key "C-x M-f" 'find-file-in-dropbox)

(defun browse-dropbox-public-folder (&optional filename)
  "FILENAME を Dropbox Public フォルダとしてブラウザで開く。"
  (interactive)
  (or filename
      (setq filename
            (ffap-read-file-or-url
             "Browse a file in DropBox Folder: " (ffap-guesser))))
  (setq filename (expand-file-name (file-truename filename)))
  (if (string-match "Dropbox/Public/\\(.*\\)$" filename)
      (let ((url (concat "https://dl.dropboxusercontent.com/u/463784/"
                         (match-string 1 filename))))
        (message "url=%s" url)
        (browse-url url))
    (message "Not Dropbox Public Directory! %s -- " filename)))
(bind-key "C-x M-F" 'browse-dropbox-public-folder)

;;;; Melpa
(defun browse-melpa-package (&optional package-name)
  "PACKAGE-NAME を MELPA パッケージとしてブラウザで開く."
  (interactive)
  (or package-name
      (setq package-name
            (ffap-read-file-or-url
             "Find file in MELPA: " (ffap-guesser))))
  (browse-url (concat "http://melpa.milkbox.net/#/" package-name)))
(bind-key "C-x C-S-f" 'browse-melpa-package)

;;;; count-features
(defvar tkw-count-features 0)

(defun tkw-count-features-update ()
  (setq tkw-count-features
        (length
            (cl-remove-if
             (lambda (feature)
               (string-match "-autoload" (symbol-name feature)))
             features))))

(defadvice provide
  (after tkw-count-features activate)
  (tkw-count-features-update))

(define-minor-mode tkw-count-features-mode
  "Minor mode to display current number of features at mode-line."
  :global t
  :init-value t
  :lighter (:eval (format "【%s】" tkw-count-features)))

;;;; Ingress
(defun tkw-intelmap-url-region (from to)
  "Ingress Intel Map URL と Google Map のURLを置換."
  ;; https://www.google.co.jp/maps/@35.2527083,139.6836931,15z ←→
  ;; https://www.ingress.com/intel?ll=35.2527083,139.6836931&z=15
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (if (looking-at "https://www.ingress.com/intel\\?ll=\\(.+?\\),\\(.+?\\)&z=\\([0-9]+\\)")
          (replace-match "https://www.google.co.jp/maps/@\\1,\\2,\\3z")
        (if (looking-at "https://www.google.co.jp/maps/@\\(.+?\\),\\(.+?\\),\\([0-9]+\\)z")
            (replace-match "https://www.ingress.com/intel?ll=\\1,\\2&z=\\3"))))))

(defun tkw-ingressmm-find (word)
  (interactive "sMission Name: ")
  (browse-url (concat "http://ingressmm.com/?find=" word)))

;;;; その他の関数
(defun find-file-no-mode ()
  "Find file without no mode settings."
  (interactive)
  (let ((auto-mode-alist-bak auto-mode-alist))
    (setq-default auto-mode-alist nil)
    (condition-case nil
        (call-interactively 'find-file)
      ((quit error) (setq-default auto-mode-alist auto-mode-alist-bak)))
    (setq-default auto-mode-alist auto-mode-alist-bak)))

(defun find-in-naxos-music-library (word)
  (interactive "s Naxos Keyword? ")
  (browse-url (concat "http://ml.naxos.jp/KeywordSearch.aspx?word=" word)))

(defun ucs-code-point-region (from to)
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (let ((string (buffer-substring from to)))
        (delete-region from to)
        (insert (mapconcat (lambda (x) (format "U+%X" x))
                           (string-to-list string) " "))))))

;; MELPAの org-screenshot は, MacOS未対応。
(declare-function org-display-inline-images "org" (&optional include-linked refresh beg end))
(declare-function org-defkey "org" (keymap key def))
(with-eval-after-load 'org
  (defun tkw-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (let ((filename
           (concat
            (make-temp-name
             (concat (buffer-file-name)
                     "_"
                     (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
          (command (or (executable-find "screencapture")
                       (executable-find "import"))))
      (call-process command nil nil nil filename)
      (insert (concat "[[" filename "]]"))
      (org-display-inline-images))))

(defun translate-string (string table)
  (with-temp-buffer
    (insert string)
    (translate-region (point-min) (point-max) table)
    (buffer-string)))

;;;; YAML Parser
;; http://stackoverflow.com/questions/10093211/what-is-the-best-yaml-parser-in-elisp
;; % pip install PyYaml
(defun yaml-to-json-region (from to)
  "Parse YAML region FROM TO and convert it to json.
If SEXP is t, convert it to S-expression."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (shell-command-on-region
       (point-min) (point-max)
       "python -c 'import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)'"
       nil t))))

(defun yaml-parse-string (string)
  (with-temp-buffer
    (insert string)
    (yaml-to-json-region (point-min) (point-max))
    (goto-char (point-min))
    (json-read)))

(defun yaml-to-sexp-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (insert (pp (yaml-parse-string (buffer-substring from to))))
      (delete-region (point) (point-max)))))

;;;;; ログ・ファイル出力
(defvar logger-process nil)
(defun logger (&rest msg)
  (unless logger-process
    (setq logger-process (start-process-shell-command "logger" nil (concat "cat >> ~/log.txt"))))
  (process-send-string logger-process (concat (apply 'format msg) "\n")))

;;;;; キー入力の表示
(defun tkw-show-key-input ()
  (message "%s" (format-kbd-macro (vector last-command-event))))

(defun tkw-toggle-show-key-input ()
  (interactive)
  (if (memq 'show-key-input post-command-hook)
      (remove-hook 'post-command-hook 'show-key-input)
    (add-hook 'post-command-hook 'show-key-input)))

;;; 非標準ライブラリ
;;;; dash
(use-package dash :no-require t :defer t :ensure t)

;;;; dash-functional
(use-package dash-functional :no-require t :defer t :ensure t)

;;;; s
(use-package s :no-require t :defer t :ensure t)

;;;; f
(use-package f :no-require t :defer t :ensure t)

;;;; ht
(use-package ht :no-require t :defer t :ensure t)

;;;; kv
;; 相互変換
;;(use-package kv :no-require t :defer t :ensure t)

;;;; peg
;; parsing expression grammar
(use-package peg :no-require t :defer t :ensure t)


;;;; wisi
;; API
;; - wisi-setup
;; - wisi-backward-cache
;; - wisi-backward-token
;; - wisi-cache-class
;; - wisi-cache-containing
;; - wisi-cache-end
;; - wisi-cache-last
;; - wisi-cache-nonterm
;; - wisi-cache-text
;; - wisi-cache-token
;; - wisi-forward-cache
;; - wisi-forward-find-class
;; - wisi-forward-token
;; - wisi-get-cache
;; - wisi-get-containing-cache
;; - wisi-goto-containing
;; - wisi-goto-containing-paren
;; - wisi-goto-end-1
;; - wisi-indent-current
;; - wisi-indent-paren
;; - wisi-next-statement-cache
;; - wisi-prev-statement-cache
;; - wisi-validate-cache
(use-package wisi :no-require t :defer t :ensure t)

;;; 非標準マイナーモード
;;;; anzu
;; isearchのマッチ数を左下に表示
(use-package anzu :no-require t :defer t :ensure t
  :init
  (add-hook 'isearch-mode-hook (command (require 'anzu)))
  :config
  (global-anzu-mode 1)
  (setq anzu-mode-lighter
        (if (eq window-system 'mac) "🍏"
          (propertize "杏" 'face '(:foreground "green")))))

;;;; ace-isearch
(use-package ace-isearch :no-require t :defer t :ensure t
  ;; 漢字１文字の検索ができない。→ ace-jump の問題。要解決。
  :config
  ;;(global-ace-isearch-mode 1)
  )

;;;; ace-jump-buffer (abstain)
;; M-x ace-jump-buffer
;; helm-buffers-list で十分なので不要。

;;;; ace-jump-mode
;; カーソル移動を伴わずに、画面中の指定した場所に移動する。
(use-package ace-jump-mode :no-require t :defer t :ensure t
  :init
  (defun tkw-ace-jump-word-mode ()
    (interactive)
    (ace-jump-word-mode (tkw-this-command-char)))
  (defun tkw-ace-jump-char-mode ()
    (interactive)
    (ace-jump-char-mode (tkw-this-command-char)))
  ;; Alt+文字で、その単語に移動。
  (progn
    (cl-loop for c from ?0 to ?9
             do (eval `(bind-key ,(format "A-%c" c) 'tkw-ace-jump-word-mode)))
    (cl-loop for c from ?a to ?z
             do (eval `(bind-key ,(format "A-%c" c) 'tkw-ace-jump-word-mode)))
    ;; Meta+Shift+文字で、その文字に移動。
    (cl-loop for c from ?A to ?Z
             do (eval `(bind-key ,(format "A-%c" c) 'tkw-ace-jump-char-mode)))))

;;;; auto-dim-other-buffers (abstain)
;; run-hooks: Invalid function: (quote adob--after-change-major-mode-hook)
;; エラーが出るのでデフォルトでの使用中止。
;; M-x auto-dim-other-buffers
(use-package auto-dim-other-buffers :no-require t :defer t :ensure t
  ;; :init
  ;; (auto-dim-other-buffers-mode)
  :diminish "")

;;;; auto-save-buffers-enhanced (abstain)
;; とりあえず手保存でいいので利用中止。
;;(when (functionp 'auto-save-buffers-enhanced)
;;  (auto-save-buffers-enhanced))
;;(defun auto-save-file-name-p (filename)
;;  (or (string-match "^#.*#$" filename)
;;      (string-match "\\.passwd$" filename)))

;;;; auto-yasnippet (abstain)
;; https://github.com/abo-abo/auto-yasnippet
;;(use-package auto-yasnippet :defer t
;;  :commands (aya-create aya-expand
;;           (bind-key "C-x a C" 'aya-create)
;;           (bind-key "C-x a E" 'aya-expand)))

;;;; button-lock
;; バッファ中の正規表現にマッチする場所をクリック可能にする。
;; 副次的なモードなのでlighterは消す。
(use-package button-lock :no-require t :defer t :ensure t
  :diminish "")

;;;; color-identifiers-mode
;; 識別子をカラフルに色付けしてくれる。便利。
(use-package color-identifiers-mode :no-require t :defer t :ensure t
  :init
  ;; (global-color-identifiers-mode)
  (add-hook 'prog-mode-hook 'color-identifiers-mode))

;;;; crosshairs (abstain)
;; 十字カーソルモード。重いので使用しない。
;; M-x crosshairs-toggle-when-idle がお勧め。

;;;; dash-at-point
;; M-x dash-at-point (Mac)
;; Lookup documents for CSS, C++, C, javascript, ruby, boost, angularJS, etc.
(use-package dash-at-point :no-require t :ensure t
  :bind ("C-c d" . dash-at-point))

;;;; diffscuss-mode
;; % pip install diffscuss
(use-package diffscuss-mode :no-require t :defer t :ensure t)

;;;; diminish
;; minor-mode のモードライン名を削除・変更する。
(use-package diminish :no-require t :defer t :ensure t
  ;; Emacs 標準パッケージは下記で設定。それ以外は use-package の :diminish で管理。
  :config
  (with-eval-after-load 'abbrev
    (diminish 'abbrev-mode (propertize "省" 'face '(:foreground "orange"))))
  (with-eval-after-load 'checkdoc
    (diminish 'checkdoc-minor-mode "✔"))
  (with-eval-after-load 'flyspell
    (diminish 'flyspell-mode (if (eq window-system 'mac) "🐬" "飞")))
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode (propertize "概" 'face '(:foreground "blue")))))

;;;; drag-stuff
;; - http://github.com/rejeep/drag-stuff
;; マウスで行等を移動するモード。
;; M-x drag-stuff-global-mode
(use-package drag-stuff :no-require t :defer t :ensure t)

;;;; emmet-mode
;; https://github.com/smihica/emmet-mode/blob/master/README.md
;; C-j :: emmet-expand-line
(use-package emmet-mode :no-require t :defer t :ensure t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (set-variable 'emmet-indentation 2)) ; indent はスペース2個

;;;; enclose-mode (abstain)
;; 自動閉じ括弧挿入
;; 馴染まないので利用中止。
;; (enclose-global-mode 1)

;;;; engine-mode
;; 検索エンジンツール
;;(use-package engine-mode :defer t)

;;;; exec-path-from-shell (abstain)
;; exec-path 環境変数をシェルから取得する。sync-env があるので不要。

;;;; evernote-mode
;; - URL : https://github.com/pymander/evernote-mode
;; % gem install evernote_oauth
;; % gem install gdbm
;; % apt-get install libgdbm-dev
;; % export EN_PROXY $HTTP_PROXY
;; evernote-mode は、autoload時 に ruby へのアクセスをして（お行儀が悪
;; い）、rubyがインストールされていない環境では回避不可なエラーを出すの
;; で、MELPAではなく直接、site-lisp にインストールする。
;;
;; - Developer Token の取得方法 :: Developer 登録して、
;;   https://www.evernote.com/api/DeveloperToken.action にアクセスし、
;;   パスワードを入力すると Token が現れる。（１年間有効）
(use-package evernote-mode :no-require t
  :commands (evernote-open-note)
  :config
  (when (or (/= 0 (shell-command "gem list evernote_oauth -i"))
            (/= 0 (shell-command "gem list gdbm -i")))
    (error "Required gem not installed."))
  (set-variable 'evernote-username "kawabata")
  (let* ((lib (locate-library "evernote-mode"))
         (api-key
           (plist-get
            (nth 0 (auth-source-search :host "evernote.com"))
            :secret)))
    (when lib
      (set-variable 'enh-enclient-command
                    (expand-file-name
                     "ruby/bin/enclient.rb"
                     (file-name-directory lib))))
    (when (functionp api-key) (set-variable 'evernote-developer-token (funcall api-key))))
  ;;(set-variable 'evernote-enml-formatter-command
  ;;      '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
  ;;(bind-key "C-c e c" 'evernote-create-note)
  ;;(bind-key "C-c e o" 'evernote-open-note)
  ;;(bind-key "C-c e s" 'evernote-search-notes)
  ;;(bind-key "C-c e S" 'evernote-do-saved-search)
  ;;(bind-key "C-c e w" 'evernote-write-note)
  ;;(bind-key "C-c e p" 'evernote-post-region)
  ;;(bind-key "C-c e b" 'evernote-browser))
  )

;;;; fixmee (abstain)
;; fixmee を使うと、shellで M-p 押下時にコマンドプロンプトの前にコマンドが挿入されるので使用中止。
;; M-x fixmee-view-listing
;; XXX :: その部分のコードが正しくないが多くの場合動いてしまう
;; FIXME :: コードが間違っていて修正を要する
;; TODO :: 将来強化すべき箇所の表示
;;(use-package fixmee
;;  :commands (fixmee-mode)
;;  :init
;;  (add-hook 'prog-mode-hook 'fixmee-mode)
;;  :config
;;  (set-variable 'fixmee-mode-lighter
;;        (if (eq window-system 'mac) "🔨"
;;          (propertize "修" 'face '(:foreground "green")))))

;;;; flycheck
;; - URL :: http://www.flycheck.org/
;; flymake の改良版
;; 便利だが、ファイルが巨大になると動作が重くなる。
(use-package flycheck :no-require t :defer t :ensure t
  :init
  (add-hook 'prog-mode-hook
            'flycheck-mode)
            ;; 大きなファイルでは重いので通常はオフ
            ;;(lambda ()
            ;;  (unless (tkw-large-file-p) (flycheck-mode))))
  :config
  (when (eq window-system 'mac)
    (set-variable 'flycheck-mode-line-lighter "🐦"))
  ;; TODO JSON Schema Validator の作成
  ;; （完成したら、Melpa で、jarファイル込みで配布したい…）
  ;; https://bintray.com/fge/maven/json-schema-validator/view
  ;; java -Done-jar.silent=true -jar json-schema-validator-2.1.10-standalone.jar --help
  ;; Syntax: java -jar jsonschema.jar [options] file [file...]
  ;;(flycheck-define-checker json-schema
  ;;  :command)
  )

;;;; ggtags
;; manual :: https://github.com/leoliu/ggtags
;; 以下でキー割当の衝突を起こすので、利用後は速やかにquitするよう、注意が必要
;; - M-n,M-p :: タグの移動
;; - M-{,M-} :: ファイルの移動
;; - M-O (M-o) :: navigation-mode の切り替え
;; - M-* :: navigation-mode の終了
;; - gtags :: C/C++/Java/PHP/yacc
;; - gtags --gtagslabel=ctags-exuberant :: awk, C#, cobol, Eiffel,
;;   Erlang, HTML, JavaScript, Lua, OCaml, Pascal, Perl, Python, Ruby,
;;   Scheme, SQL, TeX
;;   （emacs付属のctagsをインストールしないように気をつけること。）
;; after-save-hook で、保存ファイルのタグ自動更新機能付き
(use-package ggtags :no-require t :defer t :ensure t
  :defines (ggtags-navigation-mode-map)
  :config
  (bind-key "M-S-o" 'ggtags-navigation-visible-mode ggtags-navigation-mode-map))

;;;; gtags (obsolete)
;; → ggtags を使う。（ナビゲーション・自動更新機能付き）
;; http://www.emacswiki.org/emacs/GnuGlobal
;; (lazyload () "gtags"
;;   (set-variable 'gtags-suggested-key-mapping t)
;;   (when (functionp 'gtags-mode)
;;     (add-hook 'c-mode-hook
;;               (command (gtags-mode t))))
;;   (add-hook 'after-save-hook #'gtags-update-hook)
;;   (defun gtags-root-dir ()
;;     "Returns GTAGS root directory or nil if doesn't exist."
;;     (with-temp-buffer
;;       (if (zerop (call-process "global" nil t nil "-pr"))
;;           (buffer-substring (point-min) (1- (point-max)))
;;         nil)))
;;   (defun gtags-update ()
;;     "Make GTAGS incremental update"
;;     (call-process "global" nil nil nil "-u"))
;;   (defun gtags-update-hook ()
;;     (when (gtags-root-dir)
;;       (gtags-update))))

;;;; hiwin (abstain)
;; auto-dim-other-buffers-mode を使うので利用休止。
;; http://d.hatena.ne.jp/ksugita0510/20111223/p1
;; 現在のウィンドウをハイライトする。
;;(require 'hiwin nil :no-error)
;;(hiwin-activate) ;; (hiwin-deactivate)
;;(use-package hiwin
;;  :diminish " hiw"
;;  :commands hiwin-mode)

;;;; highlight-indentation
(use-package highlight-indentation :no-require t :defer t :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'highlight-indentation-current-column-mode))

;;;; idle-highlight-mode
;; アイドル中に同じ部分文字列をハイライトする。
;; color-identifiers-mode に似ている。
(use-package idle-highlight-mode :no-require t :defer t :ensure t
  :init
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

;;;; mb-depth+
;; mb-depth の数字の表示のカスタマイズ化

;;;; mic-paren (abstain)
;; show-paren の拡張
;; エラーが頻発するので使用中止。
;;(when (functionp 'paren-activate)
;;  (paren-activate))

;;;; mmm-mode (abstain)
;; 使用中止。web-mode へ。

;;;; moz (abstain)
;; MozRepl との連携 → Chrome / skewer-mode に移行。
;; C-c C-s: open a MozRepl interaction buffer and switch to it
;; C-c C-l: save the current buffer and load it in MozRepl
;; C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;; C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;; C-c C-r: send the current region to MozRepl
;;(use-package moz
;;  :commands moz-minor-mode
;;  :config
;;  (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;;  (defun javascript-custom-setup ()
;;    (moz-minor-mode 1)))

;;;; multicolumn
(use-package multicolumn :no-require t :defer t :ensure t)

;;;; nyan-mode
;; nyancat がファイル一をモードラインで表示。
;; M-x nyan-mode
(use-package nyan-mode :no-require t :defer t :ensure t)

;;;; openwith
;; 【注意】 openwith の (error) の前後に "(let ((debug-on-error nil))...) " を入れること。
;; : (let ((debug-on-error nil))
;; : (error "Opened %s in external program"
;; :        (file-name-nondirectory file))
;; : )
(use-package openwith :no-require t :defer t :ensure t
  :config
  ;; なぜ必要？
  ;;(require 'recentf nil :no-error)
  ;;(set-variable 'openwith-associations
  ;;      `((,tkw-open-externally-ext-regexp
  ;;        "open" (file))
  ;;        ("/$"
  ;;         "open" (file))))
  (openwith-mode)
  ;; gnus で添付ファイル送信の際にopenwithが実行されるのを防止する。
  (with-eval-after-load 'mm-util
    (pushnew 'openwith-file-handler mm-inhibit-file-name-handlers)))

;;;; outshine (abstain)
;; outline with outshine outshines outline
;; Emacs Lisp の outline 設定を無視して org-mode 風にするため使用中止。
;;(use-package outshine :defer t
;;  :commands outshine-hook-function
;;  :init
;;  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;;  :config
;;  (add-hook 'org-mode-hook
;;             (lambda ()
;;               ;; Redefine arrow keys, since promoting/demoting and moving
;;               ;; subtrees up and down are less frequent tasks then
;;               ;; navigation and visibility cycling
;;               (org-defkey org-mode-map
;;                           (kbd "M-<left>") 'outline-hide-more)
;;               (org-defkey org-mode-map
;;                           (kbd "M-<right>") 'outline-show-more)
;;               (org-defkey org-mode-map
;;                           (kbd "M-<up>") 'outline-previous-visible-heading)
;;               (org-defkey org-mode-map
;;                           (kbd "M-<down>") 'outline-next-visible-heading))
;;             'append))

;;;; pandoc-mode
;; pandoc 1.12.4 以降で org-mode をサポート
;; install
;; % brew install cabal-install
;; % cabal update
;; % cabal install cabal-install
;; % cabal install alex happy pandoc pandoc-citeproc
;; autoload :: pandoc-mode, turn-on-pandoc, conditionally-turn-on-pandoc
;; document :: http://joostkremers.github.io/pandoc-mode/
;; keybindings :: C-c / XXX
;; 使い方：まず、
;;   C-c / w :: pandoc-set-write (pandoc--set-write)
;;   C-c / s :: pandoc-save-settings-file
(use-package pandoc-mode :no-require t :defer t :ensure t
  :init
  ;; 過去に一度、pandoc-modeにして、.FILENAME.pandoc が保存されている場合のみ、
  ;; pandoc-mode をオンにする。
  (add-hook 'text-mode-hook 'conditionally-turn-on-pandoc)
  ;;
  ;; ((latex-engine "xelatex")
  ;;  (bibliography "~/share/Bibliography/all.bib))
  )
;; Pandoc メモ
;;
;; * マニュアル
;; - 原則は標準入力（ファイルの場合は直接指定）・標準出力（ファイルの場合は-o指定）
;; - フォーマット指定 :: -f (from) -t (to)
;; - その他 :: -s (standalone) -i (incremental slideshow) --toc
;; - scripting :: http://johnmacfarlane.net/pandoc/scripting.html
;; - samples :: http://johnmacfarlane.net/pandoc/demos.html
;; * テンプレート
;; - local :: ~/.pandoc/templates
;; - github :: http://github.com/jgm/pandoc-templates
;; - filters :: https://github.com/jgm/pandocfilters
;; HTML5/Word/EPubへの変換
;; % pandoc -t html5 -o output.html input.org
;; Wordへの変換
;; % pandoc -t docx -o output.docx input.org
;; epub3への変換
;; % pandoc -t epub3 -o output.epub input.org
;; slide
;; % pandoc -s -t {s5, slidy, slideous, dzslides, revealjs} -o slide.html input.org
;; --slidy-url, --slideous-url, or --s5-url (--revealjs-url not supported)

;; Pandoc Options

;; Pandoc slide
;; http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html
;; | format   | options            | Sample Value                             |
;; |----------+--------------------+------------------------------------------|
;; | pdf      | geometry           | (see LaTeX geometry package doc.)        |
;; | epub     | epub-stylesheet    |                                          |
;; |          | epub-embed-fonts   |                                          |
;; |----------+--------------------+------------------------------------------|
;; | (slides) | (5 formats)        |                                          |
;; | revealjs | css                | http://cdn.jsdelivr.net/reveal.js/2.6.2/ |
;; | s5       | s5-url             |                                          |
;; | slideous | slideous-url       | ??                                       |
;; | slidy    | slidy-url          |                                          |
;; | dzslides | (standalone)       |                                          |

;; Pandoc の数式は、--webtex, --mathjax, --mathml があるが、--mathjax が一番安定。

;; Pandoc Xelatex
;; | options      |         | variables |
;; |--------------+---------+-----------|
;; | latex-engine | xelatex | mainfont  |

;; pandoc slides

;;;; pangu-spacing (abstain)
;; Minor-mode to add space between Chinese and English characters.
;; 日本語の平仮名にも適用される。表示のみのスペースがアルファベットとの間にはいる。
;; Emacsの動作が重くなるので使用中止。
;; autoload でモードがオンになるので、インストールも中止。
;;(use-package pangu-spacing
;;   :commands (global-pangu-spacing-mode))

;;;; paredit-mode (abstain)
;; → smartparens に移行。重くなるので利用中止。

;;;; powerline (abstain)
;; http://hico-horiuchi.com/wiki/doku.php?id=emacs:powerline
;; 仮名漢字変換が表示できなくなるので使用中止。

;;;; popwin
;; *Help* などのバッファのポップアップを便利にする。
(use-package popwin :no-require t :defer t :ensure t
  :functions (popwin-mode)
  :defines (popwin:keymap popwin:special-display-config)
  :config
  (popwin-mode 1)
  (set-variable 'popwin:popup-window-width 24)
  (set-variable 'popwin:popup-window-height 15)
  (set-variable 'popwin:popup-window-position 'bottom)
  (pushnew '("*helm*" :height 20)      popwin:special-display-config)
  (pushnew '(dired-mode :position top) popwin:special-display-config)
  (pushnew '("*BBDB*" :height 10)      popwin:special-display-config)
  (bind-key "M-Z" popwin:keymap))

;;;; pretty-mode (obsolete)
;; → pretty-symbols に移行
;; nil や lambda 等を λ や ∅ に置き換える。
;;(when (functionp 'turn-on-pretty-mode)
;;  (add-hook 'lisp-mode-hook 'turn-on-pretty-mode)
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-pretty-mode))

;;;; pretty-symbols (obsolete)
;; → prettify-symbols-mode として、Emacs 24.4 に標準添付
;;(use-package pretty-symbols :defer t :ensure t
;;  :diminish "λ"
;;  :init
;;  (dolist (mode '(emacs-lisp c go js perl cperl enh-ruby python inferior-python))
;;    (add-hook (intern (concat (symbol-name mode)
;;                              "-mode-hook")) 'pretty-symbols-mode)))

;;;; pretty-symbols-mode (obsolete)
;; → pretty-symbols に改名。

;;;; rainbow-delimiters
;; ネストしたカッコを色違いで表示する。
(use-package rainbow-delimiters :no-require t :defer t :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; :config
  ;; (global-rainbow-delimiters-mode)
  )

;;;; rainbow-mode
;; CSSなどの色指定をその色で表示する。
(use-package rainbow-mode :no-require t :defer t :ensure t
  :init
  (add-hook 'css-mode-hook
            (command (rainbow-mode 1))))

;;;; rbenv (abstain)
;; 現在の rbenv の状態をモードラインに表示する。
;; 将来的には、anyenv で mode line単位で切り替えたい。
;; M-x global-rbenv-mode/ rbenv-use-global/ rbenv-use

;;;; relative-line-numbers
;; M-x (global-)relative-line-numbers-mode
(use-package relative-line-numbers :no-require t :defer t :ensure t)

;;;; regexp-lock
;; 正規表現の \\(....\\) に対応番号を付与する elisp
;; http://osdir.com/ml/emacs.sources/2005-11/msg00004.html
(use-package regexp-lock :no-require t
  :commands (turn-on-regexp-lock-mode)
  ;; 重いので、動作がもっさりしていると感じたらすぐにオフにする。
  ;; .bbdb.el をアクセスする際に、ナローイングと衝突してエラーを出すので
  ;; とりあえず、デフォルト起動を停止。
  ;;:init
  ;;(add-hook 'emacs-lisp-mode-hook 'turn-on-regexp-lock-mode)
  )

;;;; repl-toggle
;; inferior と major の間をトグルする。
;; autoload :: repl-toggle-mode
(use-package repl-toggle :no-require t :defer t :ensure t
  :config
  (autoload 'php-boris "php-boris")
  (autoload 'elixir-mode-iex "elixir-mode")
  (set-variable 'rtog/mode-repl-alist
                '(;;(php-mode . php-boris)
                  (emacs-lisp-mode . ielm)
                  ;;(elixir-mode . elixir-mode-iex)
                  (ruby-mode . inf-ruby))))

;;;; rudel
;; 共同編集システム
(use-package rudel :no-require t :defer t :ensure t
  :config
  (global-rudel-minor-mode 1))

;;;; smart-compile
;; quickrun に移行。
;; 特定ファイルのコンパイル命令を、smart-compile-alist のモードに応じて設定する。
;;(use-package smart-compile
;;  :bind ("C-x '" . smart-compile)
;;  :config
;;  (setf (alist-get "\\.xhtml$" smart-compile-alist)
;;        "/usr/local/AHFormatterV62/run.sh -pdfver PDF1.4 -peb 1 -x 4 -d %f -o %n.pdf")
;;  (setf (alist-get "\\.epub$" smart-compile-alist) "open %f"))

;;;; smart-cursor-color (abstain)
;; 背景色や表示色に応じてカーソル色を変化させる。
;; あまり効果を感じられないので使用中止。
;;(use-package smart-cursor-color
;;  :config
;;  (smart-cursor-color-mode +1))

;;;; smartparens
;; 最近人気のカッコ処理モード　quoteを入力する度にfont-lockで色変化するのを防止する。
;; M-x smartparens-global-mode
(use-package smartparens :no-require t :defer t :ensure t
  :diminish "括" ;; ,(propertize "括" 'face '(:foreground "blue"))
  ;; :init
  ;; (smartparens-global-mode)
  :functions (sp-local-pair)
  :config
  (sp-local-pair 'rhtml-mode "<" ">")
  (sp-local-pair 'rhtml-mode "<%" "%>")
  (show-smartparens-global-mode t))

;;;; stripe-buffer
;; バッファを縞々模様にする。
;; M-x stripe-buffer-mode
;; ちょっと見づらいので利用休止。
;;(use-package stripe-buffer :defer t :ensure t
;;  :init
;;  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
;;  ;; 動作が重くなるので中止。
;;  ;; (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
;;  )

;;;; tabbar (使用中止)
;; [注意] tabbarはバッファが増えると著しく重くなるので使用中止。
;; gnupack の設定を利用。
;;(when (require 'tabbar nil :no-error)
;;  ;; tabbar有効化
;;  (tabbar-mode -1)
;;  ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
;;  (tabbar-mwheel-mode -1)
;;  ;; タブグループを使用（t：有効，nil：無効）
;;  (set-variable 'tabbar-buffer-groups-function nil)
;;  ;; ボタン非表示
;;  (dolist (btn '(tabbar-buffer-home-button
;;                 tabbar-scroll-left-button
;;                 tabbar-scroll-right-button))
;;    (set btn (cons (cons "" nil) (cons "" nil))))
;;  ;; タブ表示 一時バッファ一覧
;;  (defvar tabbar-displayed-buffers
;;    '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*"
;;      "*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
;;    "*Regexps matches buffer names always included tabs.")
;;  ;; 作業バッファの一部を非表示
;;  (set-variable 'tabbar-buffer-list-function
;;        (lambda ()
;;          (let* ((hides (list ?\  ?\*))
;;                 (re (regexp-opt tabbar-displayed-buffers))
;;                 (cur-buf (current-buffer))
;;                 (tabs (delq
;;                        nil
;;                        (mapcar
;;                         (lambda (buf)
;;                           (let ((name (buffer-name buf)))
;;                             (when (or (string-match re name)
;;                                       (not (memq (aref name 0) hides)))
;;                               buf)))
;;                         (buffer-list)))))
;;            (if (memq cur-buf tabs)
;;                tabs
;;              (cons cur-buf tabs)))))
;;  ;; キーバインド設定
;;  ;(bind-key "<C-tab>"   'tabbar-forward-tab)
;;  ;(bind-key "<C-S-tab>" 'tabbar-backward-tab)
;;  ;; タブ表示欄の見た目（フェイス）
;;  (set-face-attribute 'tabbar-default nil
;;                      :background "SystemMenuBar")
;;  ;; 選択タブの見た目（フェイス）
;;  (set-face-attribute 'tabbar-selected nil
;;                      :foreground "red3"
;;                      :background "SystemMenuBar"
;;                      :box (list
;;                            :line-width 1
;;                            :color "gray80"
;;                            :style 'released-button)
;;                      :overline "#F3F2EF"
;;                      :weight 'bold
;;                      :family "Inconsolata"
;;                      )
;;  ;; 非選択タブの見た目（フェイス）
;;  (set-face-attribute 'tabbar-unselected nil
;;                      :foreground "black"
;;                      :background "SystemMenuBar"
;;                      :box (list
;;                            :line-width 1
;;                            :color "gray80"
;;                            :style 'released-button)
;;                      :overline "#F3F2EF"
;;                      :family "Inconsolata"
;;                      )
;;  ;; タブ間隔の調整
;;  (set-face-attribute 'tabbar-separator nil
;;                      :height 0.1)
;;  (defun rotate-tabbar (arg)
;;    (interactive "P")
;;    (if (null arg) (tabbar-forward-tab) (tabbar-backward-tab)))
;;  (add-to-list 'rotate-command-set
;;               '(rotate-tabbar . "Tabbar") t))

;;;; tabula-rasa
(use-package tabula-rasa :no-require t :ensure t
  :commands tabula-rasa-mode)

;;;; wakatime-mode
;;(use-package wakatime-mode :defer t :ensure t
;;  :diminish "若"
;;  :config
;;  (let ((api-key
;;         (plist-get
;;          (nth 0 (auth-source-search :host "wakatime.com"))
;;          :secret)))
;;    (when (functionp api-key) (callf api-key funcall api-key))
;;    (set-variable 'wakatime-api-key api-key)
;;    (set-variable 'wakatime-cli-path "~/cvs/wakatime/wakatime-cli.py")
;;    ))

;;;; winhist (abstain)
;;(lazyload (winhist-forward winhist-backward
;;           rotate-winhist
;;           (defun rotate-winhist (&optional arg)
;;             (interactive "P")
;;             (let ((command (if (null arg) 'winhist-forward 'winhist-backward)))
;;               (call-interactively command)
;;               command)))
;;    "winhist"
;;  (winhist-mode 1))
;;(require 'winhist)

;;(lazyload ((smartrep-define-rotate-key tkw-rotate-map "w"
;;             (rotate-winhist) (rotate-winhist -1)))
;;    "smartrep")

;;;; zencoding-mode (abstain)
;; emmet-mode へ移行。
;; http://www.emacswiki.org/emacs/ZenCoding
;; http://fukuyama.co/zencoding
;; M-x zencoding-mode で、 "ul#name>li.item*2" C-j で入力。
;;(lazyload ((bind-key "C-x Z" 'zencoding-mode)) "zencoding-mode"
;;  (mapc (lambda (x) (add-to-list 'zencoding-block-tags x))
;;        '("article" "section" "aside" "nav" "figure"
;;          "address" "header" "footer"))
;;  (mapc (lambda (x) (add-to-list 'zencoding-inline-tags x))
;;        '("textarea" "small" "time" "del" "ins" "sub" "sup"
;;          "i" "s" "b" "ruby" "rt" "rp" "bdo" "iframe" "canvas"
;;          "audio" "video" "ovject" "embed" "map"))
;;  (mapc (lambda (x) (add-to-list 'zencoding-self-closing-tags x))
;;        '("wbr" "object" "source" "area" "param" "option"))
;;  ;; yasnippetと連携する場合 (キーバインドは自由に)
;;  (bind-key "C-," 'zencoding-expand-yas zencoding-mode-keymap))


;;; 非標準入力ツール
;;;; ac-dabbrev (abstain)
;;(use-package ac-dabbrev :defer t
;;  :init
;;  (with-eval-after-load 'auto-complete
;;    (add-to-list 'ac-sources 'ac-source-dabbrev))
;;  :config
;;  ;; 3rd party ac-dict を追加する。（手作業）
;;  (let ((dir "~/.emacs.d/ac-dict/dotfiles/.emacs.d/ac-dict/"))
;;    (when (file-directory-p dir)
;;      (add-to-list 'ac-dictionary-directories dir))))

;;;; ac-emmet (abstain)
;; emmet の auto-complete 版
;;(use-package ac-emmet :defer t
;;  :init
;;  (add-hook 'css-mode-hook 'ac-emmet-css-setup)
;;  (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
;;  (add-hook 'web-mode-hook 'ac-emmet-html-setup))

;;;; ac-ispell (abstain)
;; ispell/aspell を使い英文のスペルチェック。
;;(use-package ac-ispell
;;  :init
;;  (defun tkw/enable-ac-ispell ()
;;    (add-to-list 'ac-sources 'ac-source-ispell))
;;  (add-hook 'mail-mode-hook 'tkw/enable-ac-ispell)
;;  :config
;;  (with-eval-after-load 'auto-complete
;;    (ac-ispell-setup)))

;;;; ac-ja (abstain)
;; 日本語の自動補完。
;; ホームディレクトリに SKK-JISYO.L のリンクを入れておく。

;;;; ac-math (abstain)
;; → math-symbols へ移行。
;; - 参照 :: https://github.com/vitoshka/ac-math#readme
;;(use-package ac-math
;;  :init
;;  (with-eval-after-load 'auto-complete
;;    (add-to-list 'ac-modes 'latex-mode)))

;;;; ansible
;; ansible 入力のためのマイナーモード
;; yasnippetとauto-complete が設定される。
;; M-x ansible で起動。
(use-package ansible :no-require t :defer t :ensure t)

;;;; anthy
(use-package anthy :no-require t :defer t
  :if (and (or (eq window-system 'x) (null window-system))
           (locate-library "anthy")
           (executable-find "anthy-agent")
           (null (equal default-input-method "japanese-mozc"))
           (null (require 'uim-leim nil :no-error)))
  :init
  (register-input-method "japanese-anthy" "Japanese"
                         'anthy-leim-activate "[anthy]"
                         "Anthy Kana Kanji conversion system")
  :config
  (set-variable 'anthy-accept-timeout 1) ;; Emacs 22のみの設定を23でも行う。
  ;; leim-list.el 相当の動作を行う。
  (set-variable 'default-input-method "japanese-anthy"))

;;(anthy-kana-map-mode)
;;(mapcar
;; (lambda (x) (anthy-change-hiragana-map (car x) (cdr x)))
;; '(("&" . "ゃ") ("*" . "ゅ") ("(" . "ょ") ("=" . "へ") ("_" . "ー")
;;   ("'" . "け") (":" . "む") ("\"" . "ろ") ("[" . "゛") ("]" . "゜")
;;   ("t[" . "が") ("g[" . "ぎ") ("h[" . "ぐ") ("'[" . "げ") ("b[" . "ご")
;;   ("x[" . "ざ") ("d[" . "じ") ("r[" . "ず") ("p[" . "ぜ") ("c[" . "ぞ")
;;   ("q[" . "だ") ("a[" . "ぢ") ("z[" . "づ") ("w[" . "で") ("s[" . "ど")
;;   ("f[" . "ば") ("v[" . "び") ("2[" . "ぶ") ("^[" . "べ") ("-[" . "ぼ")
;;   ("f]" . "ぱ") ("v]" . "ぴ") ("2]" . "ぷ") ("^]" . "ぺ") ("-]" . "ぽ")))

;;;; auto-complete
;; company とどちらを使うか.　company は日本語との相性が悪いので
;; こちらを使って見る。
;; - 参照 :: http://auto-complete.org/doc/manual.html
(use-package auto-complete-config :no-require t :defer t :ensure auto-complete
  :init
  ;; 動作が重いのでデフォルトはオフにする。
  (add-hook 'prog-mode-hook
            (lambda () (require 'auto-complete-config nil :no-error)))
  :config
  ;; (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
  ;; CSS3 properties の追加
  ;;(with-eval-after-load 'auto-complete-config
  ;;  (mapc
  ;;   (lambda (entry) (push entry ac-css-property-alist))
  ;;   '(
  ;;     ;; http://dev.w3.org/csswg/css-writing-modes/
  ;;     ("direction" "ltr" "rtl")
  ;;     ("text-combine-horizontal" "none" "all" integer)
  ;;     ("text-orientation" "mixed" "upright" "sideways-right" "sideways-left" "sideways"
  ;;      "use-glyph-orientation mixed")
  ;;     ("unicode-bidi" "normal" "embed" "isolate" "bidi-override" "isolate-override"
  ;;      "plaintext")
  ;;     ("writing-mode" "horizontal-tb" "vertical-rl" "vertical-lr")
  ;;     ;;http://dev.w3.org/csswg/css-text-decor-3
  ;;     ("text-decoration-line" "none" "underline" "overline" "line-through" "blink")
  ;;     ("text-decoration-color" color))))
  (ac-config-default))

;;;; autopair (abstain)
;; smartparen とかぶるので使用中止。
;;(use-package autopair
;;  :init
;;  (autopair-global-mode))

;;;; company (abstain)
;; mac-auto-ascii-mode と相性が悪いので利用中止。
;;(use-package company :defer t
;;  :init
;;  ;;(add-hook 'prog-mode-hook 'company-mode)
;;  :config
;;  (global-company-mode 1)
;;  )

;;;; company-anaconda
;;(use-package company-anaconda :no-require t :defer t :ensure t
;;  :if (fboundp 'company-mode))

;;;; company-auctex
;; (use-package company-auctex :no-require t :ensure t
;;   :if (fboundp 'company-mode)
;;   :commands (company-auctex-init)
;;   :init
;;   (with-eval-after-load 'auctex
;;     (company-auctex-init)))

;;;; company-c-headers
;; company は利用中断。
;;(use-package company-c-headers :no-require t :defer t :ensure t
;;  :if (fboundp 'company-mode)
;;  :config
;;  (pushnew 'company-c-headers company-backends)
;;  )

;;;; company-ghc
;;(use-package company-ghc :no-require t :defer t :ensure t
;;  :if (and (fboundp 'company-mode)
;;           (executable-find "ghc-mod"))
;;  :init
;;  (add-hook 'haskell-mode-hook 'company-mode)
;;  (set-variable 'company-ghc-show-info t))

;;;; company-go
;;;; company-inf-ruby
;;;; company-tern

;;;; company-ycmd
;;(use-package company-ycmd :no-require t :defer t :ensure t
;;  :if (fboundp 'company-mode))

;;;; codepage 51932 設定
(use-package cp5022x :no-require t :defer t :ensure t
  :config
  (define-coding-system-alias 'euc-jp 'cp51932))

;;;; dabbrev-ja
;; 使用中止


;;;; IIMECF (obsolete)
;;(set-variable 'iiimcf-server-control-hostlist
;;      (list (concat "/tmp/.iiim-" (user-login-name) "/:1.0")))
;;(when nil ;(and (= 0 (shell-command
;;          ;          (concat
;;          ;            "netstat --unix -l | grep " (car iiimcf-server-control-hostlist))))
;;          ;   (require 'iiimcf-sc nil :no-error))
;;  ;;(set-variable 'iiimcf-server-control-hostlist '("unix/:9010"))
;;  ;; IIIM server のデバッグには、rc.d/iiimd に、-d オプションと、>/tmp/iiim.debugへの出力を付加してrestart。
;;  ;; IIIM server に送出するusernameは通常は不要。
;;  ;;(set-variable 'iiimcf-server-control-username "kawabata")
;;  (set-variable 'iiimcf-server-control-default-language "ja")
;;  (set-variable 'iiimcf-server-control-default-input-method "atokx3") ;; atokx2 / wnn8(not work)
;;  (set-variable 'default-input-method "iiim-server-control")

;;  ;;(set-variable 'iiimcf-UI-lookup-choice-style 'buffer)
;;  ;; set kana keyboard
;;  ;;(set-variable 'iiimcf-current-keycode-spec-alist 'iiimcf-kana-keycode-spec-alist)
;;  (defun tkw-add-kana-map (key val)
;;    (set-variable 'iiimcf-kana-keycode-spec-alist
;;          (cons `(,key 0 ,val)
;;                (assq-delete-all key iiimcf-kana-keycode-spec-alist))))
;;  ;; かなの位置を、ASCII+AppleKeyboard風に。
;;  ;; １段
;;  (tkw-add-kana-map ?^ #xff6b) ; 「ぉ」を"^"に (US key)
;;  (tkw-add-kana-map ?& #xff6c) ; 「ゃ」を"&"に (US key)
;;  (tkw-add-kana-map ?* #xff6d) ; 「ゅ」を"*"に (US key)
;;  (tkw-add-kana-map ?\( #xff6e) ; 「ょ」を"("に (US key)
;;  (tkw-add-kana-map ?\) #xff66) ; 「を」を")"に (US key)
;;  (tkw-add-kana-map ?= #xff8d) ; 「へ」を"="に (US key)
;;  (tkw-add-kana-map ?\\ #xff70) ; 「ー」を"\"に (US key)
;;  (tkw-add-kana-map ?_ #xff70) ; 「ー」を"_"にも (original)
;;  ;; ２段
;;  (tkw-add-kana-map ?\[ #xff9e) ; 「濁音」を"["に (US key)
;;  (tkw-add-kana-map ?\] #xff9f) ; 「半濁音」を"]"に (US key)
;;  ;; ３段
;;  (tkw-add-kana-map ?: #xff91) ; 「む」を":"に (original)
;;  (tkw-add-kana-map ?' #xff79) ; 「け」を"'"に (US key)
;;  (tkw-add-kana-map ?\" #xff9b) ; 「ろ」を`"'に (apple style)

;;  ;; 上記変更のalternative
;;  (setcdr (assoc 15 iiimcf-keycode-spec-alist) '(39)) ; C-o を 左矢印に
;;  (setcdr (assoc 9 iiimcf-keycode-spec-alist)  '(37)) ; C-i を 右矢印に。
;;  (setcdr (assoc 6 iiimcf-keycode-spec-alist)  '(39 nil 1)) ; C-f → S-right
;;  (setcdr (assoc 2 iiimcf-keycode-spec-alist)  '(37 nil 1)) ; C-b → S-left

;;  ;;(iiimp-debug)
;;  )

;;;; MacUIM (1.6.2)
;; http://code.google.com/p/macuim/
;; Macintosh Only
(let ((uim-el "/Library/Frameworks/UIM.framework/Versions/Current/share/emacs/site-lisp/uim-el")
      (uim-bin "/Library/Frameworks/UIM.framework/Versions/Current/bin")
      (uim-libexec "/Library/Frameworks/UIM.framework/Versions/Current/libexec/")
      (uim-im "japanese-mozc-uim"))
  (when (and (eq window-system 'x)
             (file-directory-p uim-el)
             (file-directory-p uim-bin))
    (pushnew uim-el load-path)
    (pushnew uim-bin exec-path)
    (pushnew uim-libexec exec-path)
    (when (and (executable-find "uim-el-agent")
               (require 'uim-leim nil :no-error)
               ;; uim-leim → uim
               ;; uim-init → uim-im-init で、uim-im-alist 更新。
               (assoc uim-im input-method-alist))
      (defvar uim-candidate-display-inline)
      (set-variable 'uim-candidate-display-inline nil) ; org-mode でインラインがバグることがあるので。
      (set-variable 'default-input-method uim-im))))
;; 注意。稀に、"uim-el-agent"があるにも関わらず、japanese-mozc-uim が
;; 動かない場合がある。(uim-im-alistが空。)
;; その場合は、MacUIMを再度インストールすること。

;;;; migemo
;; cmigemo search
;; https://github.com/Hideyuki-SHIRAI/migemo-for-ruby1.9
;; 上記の migemo.el を利用する。
;; ($ sudo apt-get install mercurial)
;; ($ sudo apt-get install nkf)
;; $ hg clone https://code.google.com/p/cmigemo/
;; $ cd cmigemo
;; $ ./configure
;; $ make gcc (make osx)
;; $ cd dict
;; $ make utf-8
;; $ cd ..
;; $ sudo make gcc-install (osx-install)
;; migemoのtoggleは、M-m で。
;; Meadow with cmigemo の設定
(defvar migemo-command (executable-find "cmigemo"))
(defvar migemo-dictionary
  (locate-file "migemo-dict"
               '("/usr/share/cmigemo/utf-8"      ; fedora
                 "/usr/local/share/migemo/utf-8" ; cvs
                 "/usr/share/migemo")))          ; ubuntu
(use-package migemo :no-require t :defer t :ensure t
  :if (and migemo-command migemo-dictionary)
  :functions migemo-init
  :config
  ;; 保存パターンファイルを設定すると、Emacs終了時に極端に重くなる。
  (set-variable 'migemo-pattern-alist-file nil)
  ;;
  (set-variable 'migemo-directory (file-name-directory migemo-dictionary))
  (set-variable 'migemo-options '("-q" "--emacs")); "-i" "\a"))
  (set-variable 'migemo-user-dictionary nil)   ; nil with C/Migemo
  (set-variable 'migemo-regex-dictionary nil)  ; nil with C/Migemo
  (set-variable 'migemo-coding-system 'utf-8-unix)
  ;; ("-s" "_your_dict_path") で、辞書を追加可能。
  ;; 高速化のためにキャッシュを使う
  (set-variable 'migemo-use-pattern-alist t)
  (set-variable 'migemo-use-frequent-pattern-alist t)
  ;; キャッシュの長さ
  (set-variable 'migemo-pattern-alist-length 1024)
  ;; 最初は migemo-isearch-enable-p をnil にする。
  ;; M-m を押したらオンになるようにしていおく。
  (set-variable 'migemo-isearch-enable-p nil)
  ;(defadvice isearch-yank-string
  ;(before migemo-off activate)
  ;"文字をバッファからコピーするときにはmigemo をオフにする。"
  ;(set-variable 'migemo-isearch-enable-p nil))
  ;(defadvice isearch-mode
  ;  (before migemo-on activate)
  ;  "isearch で検索する時にはmigemo をオンにする。"
  ;  ;;(set-variable 'migemo-isearch-enable-p t)) ; ちょっとmigemoをoffにする。キー一発で切り替えたいので、方法を考える。
  ;  (set-variable 'migemo-isearch-enable-p nil))
  ; Meadowyでは、isearchに入る前にfepをオフにする。
  ;(when (eq window-system 'w32)
  ;  (defadvice isearch-mode
  ;    (before fep-off activate)
  ;    (fep-force-off))
  ;  (defadvice isearch-edit-string
  ;    (after fep-off activate)
  ;    (fep-force-off))
  ;  (set-variable 'w32-pipe-read-delay 10))     ; default 50
  ;;; kogiku
  ;; ファイルを開く際 にmigemo を利用して補完できる。(M-k で小菊on。)
  ;(when (and (locate-library "migemo") (locate-library "kogiku"))
  ;  (eval-after-load 'migemo
  ;    '(require 'kogiku)))
  (migemo-init)
  )

;;;; mozc
(use-package mozc :no-require t :defer t :ensure t
  :if (and (not (equal window-system 'mac))
           (executable-find "mozc_emacs_helper"))
  :init
  (set-variable 'default-input-method "japanese-mozc"))

;;;; sekka
;; (~/.boot2docker/ 確認)
;; % boot2docker upgrade
;; % boot2docker init
;; % boot2docker up
;; % export DOCKER_HOST=tcp://192.168.59.103:2375
;; % docker run -p 12929:12929 -t kiyoka/sekka
(use-package sekka :no-require t :defer t :ensure t
  :if (and (executable-find "nc")
           (= 0 (call-process "nc" nil nil nil "-z" "localhost" "12929")))
  :config
  (set-variable 'sekka-server-url "http://127.0.0.1:12929/")
  (set-variable 'current-sekka-server-url "http://127.0.0.1:12929/"))

;;;; skk (一時利用停止)
;; ccc-update-*** 関数が25.1 でエラーを起こすので利用中止

;; DDSKK
;; http://openlab.ring.gr.jp/skk/maintrunk/
;; http://openlab.ring.gr.jp/skk/dic.html
;; http://coexe.web.fc2.com/skkfep.html （網羅的な紹介）
;;;;; 辞書サーバ
;; (http://openlab.ring.gr.jp/skk/skkserv.html)
;; - dbskkd-cdb :: https://github.com/jj1bdx/dbskkd-cdb (2014/5)
;; - rskkserv :: http://en.sourceforge.jp/projects/rskkserv/ (2005/6)
;; - mecab-skkserv :: http://chasen.org/~taku/software/mecab-skkserv/ (2007)
;; - multi-skkserv :: https://github.com/sina-ht/multiskkserv/tree/master (2009)

;;;;; CDB辞書の生成
;; % cd ~/src/ddskk-14.1/dic
;; % wget http://openlab.jp/skk/dic/SKK-JISYO.L.gz
;; % gunzip SKK-JISYO.L.gz
;; % cd ..
;; % make cdb

;;(use-package skk :ensure ddskk
;;  :bind (("M-O" . skk-mode)
;;         ("C-x j" . skk-auto-fill-mode))
;;  :config
;;  (set-variable 'skk-user-directory
;;        (expand-file-name "skk" user-emacs-directory))
;;  ;;; 辞書設定
;;  (let (fname)
;;    (cond
;;     ((and (executable-find "nc")
;;           (= 0 (call-process "nc" nil nil nil "-z" "localhost" "1178")))
;;      (set-variable 'skk-server-host "127.0.0.1")
;;      (set-variable 'skk-server-portnum "1178")
;;      (set-variable 'skk-large-jisyo nil)
;;      ;; server completion
;;      (pushnew '(skk-server-completion-search) skk-search-prog-list)
;;      (pushnew '(skk-comp-by-server-completion) skk-search-prog-list))
;;     ((file-exists-p (set-variable 'fname (expand-file-name "SKK-JISYO.L.cdb" skk-user-directory)))
;;      (set-variable 'skk-cdb-large-jisyo fname))
;;     ((file-exists-p (set-variable 'fname (expand-file-name "SKK-JISYO.L" skk-user-directory)))
;;      (set-variable 'skk-large-jisyo fname)))
;;    ;;(set-variable 'skk-extra-jisyo-file-list
;;    ;;     (list "/usr/share/skk/SKK-JISYO.JIS2"
;;    ;;           '("/usr/share/skk/SKK-JISYO.JIS3_4" . euc-jisx0213)
;;    ;;           "/usr/share/skk/SKK-JISYO.notes"
;;    ;;           "/usr/share/skk/SKK-JISYO.assoc"
;;    ;;           "/usr/share/skk/SKK-JISYO.edict"
;;    ;;           "/usr/share/skk/SKK-JISYO.geo"
;;    ;;           "/usr/share/skk/SKK-JISYO.hukugougo"
;;    ;;           "/usr/share/skk/SKK-JISYO.jinmei"
;;    ;;           "/usr/share/skk/SKK-JISYO.law"
;;    ;;           "/usr/share/skk/SKK-JISYO.okinawa"
;;    ;;           "/usr/share/skk/SKK-JISYO.propernoun"
;;    ;;           "/usr/share/skk/SKK-JISYO.pubdic+"
;;    ;;           "/usr/share/skk/SKK-JISYO.station"
;;    ;;           "/usr/share/skk/SKK-JISYO.zipcode"
;;    ;;           "/usr/share/skk/SKK-JISYO.office.zipcode"))
;;    ;;(set-variable 'skk-byte-compile-init-file t)
;;    ;;; 表示設定
;;    (set-variable 'skk-show-inline t)
;;    (set-variable 'skk-show-inline 'vertical)
;;    (set-variable 'skk-japanese-message-and-error t)
;;    ;;; キーボードとチュートリアル
;;    ;; かな＆NICOLAを使う場合：
;;    ;; % cd ./nicola
;;    ;; % make
;;    ;; % make install （これができないならば、直接Emacs内部で (compile-nicola-ddskk) を実行。
;;    ;;(set-variable 'skk-use-kana-keyboard t) ;; t だと 「qwerty」→「たていすかんな」
;;    ;;(set-variable 'skk-kanagaki-keyboard-type 'nicola-us) ; skk-kanagaki-nicola-us-base-rule-list
;;    ;;(set-variable 'skk-kanagaki-keyboard-type 'omelet-us) ; skk-kanagaki-omelet-us-rule-list
;;    ;;(set-variable 'skk-tut-file "/usr/local/share/skk/NICOLA-SKK.tut")
;;    ;;; AZIK
;;    (set-variable 'skk-use-azik t)
;;    (set-variable 'skk-azik-keyboard-type 'us101)
;;    (set-variable 'skk-show-candidates-always-pop-to-buffer t)))

;;;; smartchr <abstain>
;; 簡易テンプレート入力機構
;; - 参照 :: https://github.com/imakado/emacs-smartchr
;;(lazyload () "ruby-mode"
;;  (when (require 'smartchr nil :no-error)
;;    (bind-key "{" (smartchr '("{" "do |`!!'| end" "{|`!!'| }" "{{" ruby-mode-map)))
;;    (bind-key "#" (smartchr '("#" "##" "#{`!!'}" ruby-mode-map)))
;;    (bind-key "%" (smartchr '("%" "%%" "%{`!!'}" ruby-mode-map)))
;;    (bind-key "W" (smartchr '("W" "%w[`!!']" "WW"  ruby-mode-map)))))

;;;; tc (abstain)
;; tcode/bushu.index2 が見つかりません、等のエラーが出るので中止。
;; isearch-search を行儀悪く上書きするので使用中止。

;;;; yasnippet
;; TODO: 重要 yasnippet 読み込み時にエラーが出たら、とりあえず
;; (set-variable 'clojure-snippets-dir nil) を実行してみること。
;;
;; official doc: https://capitaomorte.github.io/yasnippet
;;   http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/snippet-expansion.html
;; - snippets を使うときは、M-x yas-minor-mode
;;   + キーワードを入力して、<tab>キーを押す。
;;   + キーワード一覧が分からなくなったときはメニューで確認。
;; - snippets を編集したら、 M-x yas-reload-all でリロード。
;; - snippets の呼び出しは、 M-x yas-insert-snippet (C-c & C-s)
;; - snippets の展開は、M-x yas-expand (<tab>)

;; 日本語文章の入力においては、空白で区切ってキーワードを入力することができない。
;; そのため、snippetは、bindingのショートカットキーで呼び出す。
;; - helm との連携 ::  <先頭文字をタイプ>,  M-x helm-c-yasnippet (M-X y)
;; clojure-snippet で出るエラーについて
;; これらは、yas-minor-mode を実行すると、yasnippet がロードされ、その
;; 結果、eval-after-load で、そのバッファからsnippetを読み込もうとして
;; エラーになる。なぜ何度も yasnippet がロードされようとするのかは不明。
;; (yas-global-mode)
(use-package yasnippet :no-require t :ensure t
  :commands snippet-mode
;;  :config
;;  ;; 他スニペットのダウンロード (~/.emacs.d/snippets-3rd-party/)
;;  (dolist (snip-dir (directory-files
;;                     (locate-user-emacs-file "snippets-3rd-party") t "^[^.]"))
;;    (when (file-directory-p snip-dir)
;;      (add-to-list 'yas-snippet-dirs snip-dir t)
;;      ;;(yas-load-directory snip-dir)
  ;;      )))
  )

;;; 非標準メジャーモード、および関連マイナーモード
;;;; abc-mode
;; abc音楽ファイルを編集メジャーモード。abcm2ps で楽譜PSファイルに変換。
;; * 問題
;;   autoload 時の auto-insert-mode の設定が不適切で autoload にしてエ
;;   ラーを引き起こすため使用中止（2013/07/08） EmacsWikiでは修正済。
(use-package abc-mode :no-require t :ensure t
  :mode "\\.ab[cp]\\'"
  :config (require 'align))

;;;; ac-c-headers (abstain)
;; #include <s[tdio.h>]   <- ac-source-c-headers
;; pr[intf]               <- ac-source-c-header-symbols
;;(use-package ac-c-headers :defer t
;;  :init
;;  (add-hook 'c-mode-hook
;;            (lambda ()
;;              (when (and (require 'auto-complete nil :no-error)
;;                         (require 'ac-c-headers 'nil :no-error))
;;                (add-to-list 'ac-sources 'ac-source-c-headers)
;;                (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))))

;;;; ac-haskell-process
(use-package ac-haskell-process :no-require t :defer t :ensure t
  :init
  (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
  (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
  (with-eval-after-load 'auto-complete
    (add-to-list 'ac-modes 'haskell-interactive-mode)))

;;;; ac-js2 (abstain)
;; Auto-complete source for Js2-mode
;;(use-package ac-js2 :defer t
;;  :init
;;  (add-hook 'js2-mode-hook 'ac-js2-mode))

;;;; ac-nrepl (abstain)
;; Clojure に対する、ciderを用いた自動補完
;; M-x cider, M-x cider-jack-in
;;(lazyload () "cider-repl-mode"
;;  (when (functionp 'ac-nrepl-setup)
;;    (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup))
;;)

;;(lazyload () "cider-interaction"
;;(when (functionp 'ac-nrepl-setup)
;;  (add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup))
;;)

;;;; ac-slime (abstain)
;;(use-package ac-slime :defer t
;;  :init
;;  (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;  (with-eval-after-load 'auto-complete
;;    (add-to-list 'ac-modes 'slime-repl-mode)))

;;;; agda2-mode
;; * インストール
;;   : % cabal install agda
(use-package agda2-mode :no-require t
  :mode ("\\.l?agda\\'" . agda2-mode))

;;;; anaconda-mode
;; Python プログラミングモード
;; TODO: 実験
(use-package anaconda-mode :no-require t :defer t :ensure t)

;;;; apache-mode
(use-package apache-mode :no-require t :ensure t
  :mode
  (("\\.htaccess\\'"   . apache-mode)
   ("httpd\\.conf\\'"  . apache-mode)
   ("srm\\.conf\\'"    . apache-mode)
   ("access\\.conf\\'" . apache-mode)
   ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;;;; apples-mode
;; http://d.hatena.ne.jp/sandai/20120613/p2
;; M-x apples-open-scratch
(use-package apples-mode :no-require t :ensure t
  :mode (("\\.applescript$" . apples-mode)
         ("\\.scpt$" . apples-mode)))

;;;; applescript-mode (obsolete)
;; → apples-mode へ移行。

;;;; ats-mode
;; http://svn.code.sf.net/p/ats-lang/code/trunk/utils/emacs/ats-mode.el
(use-package ats-mode :no-require t
  :mode (("\\.ats\\'" . ats-mode)
         ("\\.yats\\'" . ats-parser-mode)
         ("\\.lats\\'" . ats-lexer-mode)
         ("\\.\\(d\\|s\\)ats\\'" . ats-two-mode-mode)))

;;;; auctex
;;;;; メモ
;; auto-mode-alist は自動追加される。
;; http://oku.edu.mie-u.ac.jp/~okumura/texfaq/auctex.html
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX#db9dca5d
;; http://ftp.gnu.org/pub/gnu/auctex/11.86-extra/tex-ref.pdf
;; Emacs 24.4 以降は、autoload からのファイル参照を許さないため、
;; auctex-autoloades.el の (require 'tex-site) は削除した方が無難か。
;; (require 'tex-site nil :noerror)

;; - LatexMk Options
;; |        | opt                                  | PDF出力 | DVI出力 |
;; |--------+--------------------------------------+---------+---------|
;; | euptex | -latex='uplatex -synctex=1 ...'      | -pdfdvi | -dvi    |
;; | luatex | -pdflatex='lualatex --synctex=1 ...' | -pdf    |         |
;; | xetex  | -pdflatex='xelatex -synctex=1 ...'   | -pdf    |         |
;; - 表示ソフト（SyncTeX対応）
;;   + MacOS :: Skia
;;   + Linux :: Evince
;;   + Windows :: SumatraPDF
(defun tkw-latexmk-command (latex options &optional target output)
  "Generate LatexMk command line for LATEX, (LatexMk-)OPTIONS, TARGET and OUTPUT directory."
  (let* ((latex-options
         '("-src-specials" "-file-line-error" "-interaction=nonstopmode"
           "-shell-escape" "-synctex=1"))
         (luatex-option
          (mapconcat (lambda (opt) (concat "-" opt)) latex-options " "))
         (latex-option
          (mapconcat 'identity latex-options " ")))
    (concat "latexmk -gg " options " "
            (case latex
              ('euptex "-pdfdvi -latex='uplatex ")
              ('xetex  "-pdf -pdflatex='xelatex ")
              ('luatex "-pdf -pdflatex='lualatex "))
            (case latex
              ('luatex luatex-option)
              (t latex-option))
            "' "
            (if output (concat "-output-directory=" output " "))
            target)))

;;;;; auctex
(use-package auctex :no-require t :ensure t
  :defines (TeX-output-view-style TeX-command-list TeX-engine)
  :commands (tkw-TeX-engine-set)
  :config
  (require 'latex)
  (set-variable 'TeX-engine-alist
        '(;;(symbol name %(tex) %(latex) ConText)
          (euptex "eupTeX" "euptex" "uplatex" "euptex")
          (xetex "XeTeX" "xetex" "xelatex" "xetex")
          (luatex "LuaTeX" "luatex" "lualatex" "luatex")))
  ;; 以下の設定は、複数ファイルをソースにするときの情報
  (set-variable 'TeX-engine 'euptex)
  (set-variable 'TeX-auto-save t) ; 保存時にスタイル情報も保存
  (set-variable 'TeX-parse-self t) ; 読み込み時にファイルをパーズ
  (setq-default TeX-master nil) ; ?
  ;; [[info:auctex#Japanese]]
  ;;(set-variable 'TeX-default-mode 'japanese-latex-mode)
  ;; 上記の設定で、TeX-Commandの参照先が japanese-(La)TeX-command に切り替わる。
  (set-variable 'TeX-command-default "TeX")
  ;; TeX-viewer
  (set-variable 'TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (set-variable 'TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  ;; エンジンに応じた設定
  (defun tkw-TeX-engine-set (&optional engine)
    (interactive)
    (if (null engine)
        (call-interactively 'TeX-engine-set)
      (set-variable 'TeX-engine engine))
    (setq-default TeX-engine TeX-engine)
    (case TeX-engine
      ('luatex
       (set-variable 'LaTeX-default-style   "ltjsarticle")
       (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
      ('xetex
       (set-variable 'LaTeX-default-style   "bxjsarticle")
       (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
      ('euptex
       (set-variable 'LaTeX-default-style   "jsarticle")
       (remove-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
       (pushnew '("^dvi$" "."
                  "%(o?)xdvi -watchfile 1 %dS %d")
                TeX-output-view-style)
       (pushnew '("^pdf$" "."
                  "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")
                TeX-output-view-style)))
    ;; TeX-command-list
    (set-variable 'TeX-command-list
                  (cl-remove-if (lambda (item) (equal "LatexMk" (car item)))
                                TeX-command-list))
    (set-variable 'TeX-command-list
          (cons
           `("LatexMk"
             ,(tkw-latexmk-command TeX-engine "-pvc" "%t")
             TeX-run-TeX nil (latex-mode) :help "Run LatexMk")
           TeX-command-list))
    ;;(set-variable 'LaTeX-command-default "LatexMk")
    ))

;;;;; auctex/latex
(use-package auctex :no-require t :defer t :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode) ; src-specials に対応
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ; synctex に対応
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'tkw-latex-mode-initialization)
  (set-variable 'LaTeX-indent-level 4)
  (set-variable 'LaTeX-item-indent 2))

(declare-function TeX-match-style "tex")
(declare-function TeX-engine-set "tex")
;; TODO TeX-engine-set → tkw-TeX-engine-set
(defun tkw-latex-mode-initialization ()
  "Automatically detect appropriate TeX engine from style specification."
  (require 'tex nil t)
  (let
      ((engine
        (cond
         ((TeX-match-style "\\`[jt]s?\\(article\\|report\\|book\\)\\'\\|\\`uplatex\\'")
          'euptex)
         ((TeX-match-style "\\`bxjs\\(article\\|report\\|book\\)\\'")
          'xetex)
         ((TeX-match-style "\\`ltj\\(article\\|report\\|book\\)\\'")
          'luatex))))
    (when engine (tkw-TeX-engine-set engine))))

;;;;; auctex/bib-cite
(use-package bib-cite :no-require t :defer t
  :config
  (set-variable 'bib-cite-use-reftex-view-crossref t))

;;;;; auctex-latexmk
;; M-x TeX-command-master にlatexmkメニュー追加
;; TeX-command-list を LaTeXMkだけに統一する。
;; C-c C-c で一発全部Make
;;(lazyload () "tex"
;;  (when (functionp 'auctex-latexmk-setup)
;;    (auctex-latexmk-setup)
;;    (set-variable 'japanese-LaTeX-command-default "LaTexMk")))

;;;; auctex-lua
;; - M-x LaTeX-edit-Lua-code-start
(use-package auctex-lua :no-require t :defer t :ensure t)

;;;; auto-complete-clang (abstain)
;; auto-complete-clang-async へ移行。

;;;; auto-complete-clang-async (abstain)
;; C/C++ 用 auto-complete。ソースコード解析に libclang を使用する。
;; - 参照 :: https://github.com/8bit-jzjjy/emacs-clang-complete-async
;; * インストール
;;   : % brew install emacs-clang-complete-async
;;   で、`clang-complete' がインストールされる。
;;(use-package auto-complete-clang-async :defer t
;;  :if (and (functionp 'ac-cc-mode-setup)
;;           (set-variable 'ac-clang-complete-executable
;;                 (executable-find "clang-complete")))
;;  :init
;;  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;  :config
;;  (defun ac-cc-mode-setup ()
;;    (set-variable 'ac-sources '(ac-source-clang-async))
;;    (ac-clang-launch-completion-process)))

;;;; auto-complete-nxml (abstain)
;;(use-package auto-complete-nxml :defer t
;;  :init
;;  (with-eval-after-load 'nxml-mode
;;    (require 'auto-complete-nxml nil :no-error)))

;;;; auto-compile
;; Elisp ファイルを保存する際に自動的にコンパイルする。
(use-package auto-compile :no-require t :defer t :ensure t
  :diminish "♺"
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

;;;; c-eldoc
(use-package c-eldoc :no-require t :defer t :ensure t
  :init
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;;;; cdlatex
;; Emacs 24.3では last-command-char が使えないので使用禁止。
;; (when (functionp 'turn-on-cdlatex)
;;   (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;;   (add-hook 'latex-mode-hook 'turn-on-cdlatex))
;; org-mode でも、M-x org-cdlatex-mode で利用可能。

;;;; caml (abstain)
;; → tuareg に移行。

;;;; ceylon-mode
;; https://github.com/lucaswerkmeister/ceylon-mode
;; - samples :: /usr/share/doc/ceylon-1.0.0/samples/
;; - man (1) :: ceylon, ceylon-doc, ceylon-doc-tool, ceylon-compile-js, ceylon-run-js, etc.
;;(use-package ceylon-mode :no-require t :defer t
;;  :mode ("\\.ceylon$" . ceylon-mode)
;;  :defines (java-mode-syntax-table java-mode-abbrev-table
;;            java-mode-map c-java-menu cc-imenu-java-generic-expression)
;;  :functions (c-common-init cc-imenu-init c-run-mode-hooks
;;              c-update-modeline c-init-language-vars-for)
;;  :config
;;  (define-derived-mode ceylon-mode prog-mode "Java"
;;    "Major mode for editing Ceylon code."
;;    (c-initialize-cc-mode t)
;;    (set-syntax-table java-mode-syntax-table)
;;    (set-variable 'local-abbrev-table java-mode-abbrev-table)
;;    (set-variable 'abbrev-mode t)
;;    (use-local-map java-mode-map)
;;    (c-init-language-vars-for 'java-mode)
;;    (c-common-init 'java-mode)
;;    (easy-menu-add c-java-menu)
;;    (cc-imenu-init cc-imenu-java-generic-expression)
;;    (c-run-mode-hooks 'c-mode-common-hook 'java-mode-hook)
;;    (c-update-modeline)))

;;;; cider
;; ref. https://github.com/clojure-emacs/cider
;; M-x cider-jack-in (in leiningen mode)
;; M-x cider (`% lein repl' is executed)
(use-package cider :no-require t :defer t :ensure t
  :config
  ;; (when (require 'auto-complete nil :no-error)
  ;;   (add-to-list 'ac-modes 'cider-repl-mode))
  ;; (when (functionp 'cider-turn-on-eldoc-mode)
  ;;   (add-to-list 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode))
  ;; *nrepl-connection* と *nrepl-server* を隠す
  ;; (set-variable 'nrepl-hide-special-buffers t)
  ;; (set-variable 'cider-repl-tab-command 'indent-for-tab-command)
  ;; (set-variable 'cider-repl-pop-to-buffer-on-connect nil)
  ;; (set-variable 'cider-popup-stacktraces nil)
  ;;(set-variable 'cider-repl-popup-stacktraces t)
  ;;(set-variable 'cider-auto-select-error-buffer t)
  ;; (set-variable 'nrepl-buffer-name-separator "-")
  ;;(set-variable 'nrepl-buffer-name-show-port t)
  ;; (set-variable 'cider-repl-display-in-current-window t)
  ;;(when (functionp 'subword-mode)
  ;;  (add-hook 'cider-repl-mode-hook 'subword-mode))
  ;;(when (functionp 'smartparens-strict-mode)
  ;;  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))
  )

;;;; clojure-cheatsheet
;; nrepl で nREPLサーバと接続されている必要がある。
;; - clojure-cheatsheet :: helmで選択されたコマンドのドキュメントを表示
(use-package clojure-cheatsheet :no-require t :defer t :ensure t)

;;;; clojure-mode
;; http://github.com/jochu/clojure-mode
;; 通常はclojureを直接使わず、lein を経由してプログラムする。
;; % lein new tkw-project
;; % cd tkw-project
;; (auto-mode-alist)
(use-package clojure-mode :no-require t :defer t :ensure t
  ;; :mode
  ;; ("\\.\\(clj[sx]?\\|dtm\\|edn\\)\\'" . clojure-mode) ; 設定済
  :config
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode))

;;;; clojure-test-mode
;; http://clojure-doc.org/articles/tutorials/emacs.html
;; Wikiから削除。
;;(use-package clojure-test-mode :defer t)

;;;; cmake-flymake (abstain)
;; https://github.com/seanfisk/cmake-flymake/
;; - Installation
;;   % git clone https://github.com/seanfisk/cmake-flymake
;;   % cd cmake-flymake
;;   % ln -s "$PWD"/cmake-flymake-{generate,remove} ~/bin
;; - usage
;;   % cd your_cmake_project
;;   % mkdir build # your build directory
;;   % cd build
;;   % cmake .. # create the CMake configs
;;   % cd ..
;;   % cmake-flymake-generate build
;; (use-package cmake-flymake :defer t)

;;;; cmake-mode
;; http://www.cmake.org/CMakeDocs/cmake-mode.el
(use-package cmake-mode :no-require t :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;;;; cobol-mode
;; http://www.emacswiki.org/emacs/cobol-mode.el
(use-package cobol-mode :no-require t
  :mode ("\\.cob" . cobol-mode))

;;;; coffee-mode
;; - autoloaded-mode :: .coffee, .iced, Cakefile, coffee
(use-package coffee-mode :no-require t :defer t :ensure t)

;;;; cpputils-cmake
;; https://github.com/redguardtoo/cpputils-cmake
(use-package cpputils-cmake :no-require t :ensure t
  :commands (cppcm-get-exe-path-current-buffer)
  :init
  (add-hook 'c-mode-hook (lambda () (cppcm-reload-all)))
  (add-hook 'c++-mode-hook (lambda () (cppcm-reload-all)))
  ;; OPTIONAL, somebody reported that they can use this package with Fortran
  (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
  ;; OPTIONAL, avoid typing full path when starting gdb
  (global-set-key (kbd "C-c M-g")
                  '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
  :config
  ;; OPTIONAL, some users need specify extra flags forwarded to compiler
  (set-variable 'cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG")))

;;;; crontab-mode
(use-package crontab-mode :no-require t :ensure t
  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode)))

;;;; csharp-mode
;; Mono Development Kit が必要。
;; インストールすると、原則、/usr/bin にパスが張られるので、PATH設定は不要。
(use-package csharp-mode :no-require t :defer t :ensure t
  ;; :if (executable-find "mcs")
  )

;;;; d-mode
;; http://prowiki.org/wiki4d/wiki.cgi?EditorSupport/EmacsEditor
;; http://www.emacswiki.org/emacs/FlyMakeD
;; http://qiita.com/tm_tn/items/1d01c4500e1ca7632140
;; auto-mode-alist などは autoloadで設定済み
(use-package d-mode :no-require t :defer t :ensure t
  :config
  ;; flymake -> flycheck へ移行。
  ;;(require 'flymake)
  ;;(defun flymake-D-init ()
  ;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
  ;;                     'flymake-create-temp-inplace))
  ;;         (local-file (file-relative-name
  ;;                      temp-file
  ;;                      (file-name-directory buffer-file-name))))
  ;;    (list "dmd" (list "-c" local-file))))
  ;;(pushnew '(".+\\.d$" flymake-D-init
  ;;           flymake-simple-cleanup flymake-get-real-file-name)
  ;;         flymake-allowed-file-name-masks)
  ;;(pushnew '("^\\([^ :]+\\)(\\([0-9]+\\)): \\(.*\\)$" 1 2 nil 3)
  ;;         flymake-err-line-patterns)
  )

;;;; dedukti-mode
;; Dedukti is a universal proof checker, based on the λΠ-calculus modulo.
;; https://www.rocq.inria.fr/deducteam/Dedukti/
(use-package dedukti-mode :no-require t :ensure t
  :mode ("\\.dk$" . deduki-mode)
  :config
  (set-variable 'dedukti-path (executable-find "dkcheck")))

;;;; docbook (abstain)
;; M-x docbook-find-file

;;;; dockerfile-mode
;; sh-mode の拡張
;; [autoload設定済] auto-mode-alist
(use-package dockerfile-mode :no-require t :defer t :ensure t)

;;;; dokuwiki-mode
;; `dokuwiki-mode' is autoloaded.
(use-package dokuwiki-mode :no-require t :defer t :ensure t)

;;;; doxymacs
;; doxygen mode for emacs.
;;(if (locate-library "doxymacs")
;;    (require 'doxymacs))

;;;; emacs-eclim
;; https://github.com/senny/emacs-eclim
;; Installation : http://eclim.org/install.html
(use-package eclim :no-require t :ensure emacs-eclim
  :commands (global-eclim-mode)
  :config
  (help-at-pt-set-timer)
  (set-variable 'eclim-auto-save nil)
  ;;(when (require 'ac-emacs-eclim-source nil :no-error)
  ;;  (ac-emacs-eclim-config))
  ;;(when (and (require 'company nil :no-error)
  ;;           (require 'company-emacs-eclim nil :no-error))
  ;;  (company-emacs-eclim-setup)
  (require 'eclimd))

(use-package eclimd :no-require t :defer t
  :config
  (let ((workspace "~/Documents/workspace"))
    (when (file-directory-p workspace)
      (set-variable 'eclimd-default-workspace workspace))))

;;;; egison-mode
;; - 参照 :: http://www.egison.org/
;; * Install
;;   : % cabal update
;;   : % cabal install egison
(use-package egison-mode :no-require t :ensure t
  :mode ("\\.egi$" . egison-mode))

;;;; eldoc-eval (obsolete)
;; ミニバッファのeldoc表示を行なう。
;; また、ミニバッファでの評価結果は新しいバッファで表示する。
;; 注意！ Emacs 24.4 では不要。
;;(when (version< emacs-version "24.4")
;;  (lazyload (;;eldoc-eval-expression
;;             ;;(bind-key "M-:" 'eldoc-eval-expression)
;;             ) "eldoc-eval"
;;    (set-variable 'eldoc-in-minibuffer-mode-lighter
;;          (if (eq window-system 'mac) "💁" "評"))))
;;
;;(when-interactive-and (functionp 'eldoc-in-minibuffer-mode)
;;  (eldoc-in-minibuffer-mode t))

;;;; elisp-slime-nav
;; M-. で定義に行って、M-, で戻る。
(use-package elisp-slime-nav :no-require t :defer t :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;;;; elixir-mode
;; http://elixir-lang.org/
;; - autoload :: ("\\.exs$" . elixir-mode) ("\\.exs$" . elixir-mode)
;;               ("\\.ex$" . elixir-mode)
(use-package elixir-mode :no-require t :defer t :ensure t)

;;;; elixir-mix (obsolete)
;; melpaから削除
;; elixir package management
;;(use-package elixir-mix :defer t
;;  :init
;;  (add-hook 'elixir-mode-hook 'global-elixir-mix-mode))

;;;; elm-mode
;; % cabal install elm
(use-package elm-mode :no-require t :ensure t
  :commands run-elm-repl
  :if (or (executable-find "elm")
          (executable-find "elm-repl"))
  :config (require 'elm-repl))

;;;; eww-lnum
;; ewwのリンクのショートカット番号をつけたり、またはリンク先を
;; ダウンロード・URLコピーする機能。
(defvar eww-mode-map)
(use-package eww-lnum :no-require t :defer t :ensure t
  :init
  (with-eval-after-load "eww"
    (define-key eww-mode-map "f" 'eww-lnum-follow)
    (define-key eww-mode-map "F" 'eww-lnum-universal)))

;;;; emathica (obsolete)
;; Mathematica開発モード。 → math++.elに移行。
;;(defvar emathica-comint-program-name
;;  (executable-find "/Applications/Mathematica.app/Contents/MacOS/MathKernel"))
;;(when emathica-comint-program-name
;;  (lazyload (emathica-m-mode
;;             (add-to-auto-mode-alist '("\\.m\\'" . emathica-m-mode)))
;;      "emathica"))

;;;; epsilon
;; プログラミング言語 GNU epsilon
;; http://arxiv.org/pdf/1212.5210v5.pdf
;; http://www.gnu.org/software/epsilon
(use-package epsilon :no-require t
  :mode ("\\.e$" . epsilon-mode))

;;;; enh-ruby-mode
;; auto-mode-alist は gemspec, Rakefile, rb を設定済。
;; % cd ~/.emacs.d/elpa/yasnippet-201*/snippets
;; % cp -pr ruby-mode enh-ruby-mode
(use-package enh-ruby-mode :no-require t :ensure t
  :mode (("Gemfile" . enh-ruby-mode)
         ("Capfile" . enh-ruby-mode)
         ("Vagrantfile" . enh-ruby-mode))
  :config
  ;;(when (require 'auto-complete nil :no-error)
  ;;  (add-to-list 'ac-modes 'enh-ruby-mode))
  (set-variable 'enh-ruby-bounce-deep-indent t)
  (set-variable 'enh-ruby-hanging-brace-indent-level 2))

;;;; ensime
;; * 注意
;;   2013.10 現在、きちんと動作しない。
;; * マニュアル
;;   http://aemoncannon.github.io/ensime/index.html
;; * 導入ブログ
;;   http://blog.recursivity.com/post/21844929303/using-emacs-for-scala-development-a-setup-tutorial
;;   http://rktm.blogspot.com/2012/12/emacsscala.html
;;   http://d.hatena.ne.jp/tototoshi/20100927/1285595939
;; * SBT の利用
;;   $ mkdir hello
;;   $ cd hello
;;   $ echo 'object Hi { def main(args: Array[String]) = println("Hi!") }' > hw.scala
;;   $ sbt
;;   > run
;;   (慣れたら project/plugin/src ディレクトリに分割し、build.sbt ファイルを作成する。)
;; * セットアップ手順
;;   - ~/.sbt/plugins/plugins.sbt または /PROJECT/project/plugins.sbt に、
;;      'addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.2")' を記述。
;;     （ここの 0.1.2 の数字は https://github.com/aemoncannon/ensime-sbt-cmd を参照して適宜変更）
;; * 利用手順
;;   (1) SBTプロジェクトの流儀に従いソースコードを配置し、フォルダ内で、
;;       % sbt "ensime generate"
;;       を実行すると、デバッガに必要な情報を収集した .ensime ファイルが生成される。
;;   (2) M-x ensime を実行すると、 <ensime>/bin/server → <ensime>/lib/ensime-XX.jar が起動して、通信を開始。

(use-package ensime :no-require t :ensure t
  :commands (ensime-scala-mode-hook)
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;;;; erefactor
;; autoloaded : erefactor-map
(use-package erefactor :no-require t :defer t :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
  (add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)
  :config
  (with-eval-after-load 'lisp-mode
    (bind-key "C-c C-v" 'erefactor-map emacs-lisp-mode-map)))

;;;; erlang
;; autoload にて、 .escript, .erl, .hrl のauto-mode-alistへの追加が自動的に行われる。
(use-package erlang :no-require t :defer t :ensure t
  :config
  ;; flyamek -> flycheck へ移行。
  ;; (require 'erlang-flymake)
  )

;;;; ess
;; ドキュメント : http://ess.r-project.org/Manual/ess.html#Command_002dline-editing
;; M-x R で起動
(use-package ess :no-require t :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (require 'ess-site))

;;;; feature-mode
;; Cucumber/Ecukes の編集用モード
;; :mode ("\\.feature\\'" . feature-mode)
(use-package feature-mode :no-require t :defer t :ensure t)

;;;; flycheck-ghcmod
(use-package flycheck-ghcmod :no-require t :defer t :ensure t
  :if (and (fboundp 'flycheck-mode)
           (executable-find "ghcmod"))
  :init
  (add-hook 'haskell-mode-hook
            (command (require 'flycheck-ghcmod))))

;;;; flymake-clang-c++ (abstain)
;; -> irony-mode 系統に移行。
;; https://github.com/necaris/emacs-config/blob/master/.emacs.d/flymake-clang-c%2B%2B.el
;;(use-package flymake-clang-c++
;;  :commands (flymake-clang-c++-load)
;;  :init
;;  (add-hook 'c++-mode-hook 'flymake-clang-c++-load))

;;;; flymake-elixir (abstain)
;; -> flycheck へ移行。
;;(use-package flymake-elixir :defer t :ensure t
;;  :init
;;  (add-hook 'elixir-mode-hook 'flymake-elixir-load))

;;;; flymake-go (abstain)
;; -> flycheck へ移行。
;;(use-package flymake-go :defer t
;;  :init
;;  (with-eval-after-load 'go-mode
;;    (require 'flymake-go nil :no-error)))

;;;; flymake-json (obsolete)
;; → flycheck へ移行。
;; % npm install jsonlint -g
;; TODO json-schema への対応
;;(use-package flymake-json :defer t
;;  :init
;;  (add-hook 'json-mode 'flymake-json-load))

;;;; flymake-ruby (obsolete)
;; → flycheck へ移行
;; (use-package flymake-ruby :defer t
;;   :init
;;   (add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;   (add-hook 'enh-ruby-mode-hook 'flymake-ruby-load))

;;;; flymake-shell (obsolete)
;; → flycheck へ移行。
;; (use-package flymake-shell :defer t
;;   :init
;;   (add-hook 'sh-set-shell-hook 'flymake-shell-load))

;;;; fsharp-mode
;; https://github.com/fsharp/fsharpbinding/tree/master/emacs
;; - autoload :: '("\\.fs[iylx]?$" . fsharp-mode)
;; コンパイラは Mac の場合は Mono Development Kit に同梱。
;; （インストールすると /usr/bin にシンボリックリンクが設定される。）
;; - fsharpi :: インタプリタ
;; - fsharpc :: コンパイラ
(use-package fsharp-mode :no-require t :defer t :ensure t)

;;;; fuel
;; Factor Programming Language (http://factorcode.org/)
;; - auto-mode-alist :: ".factor"
;; - interpreter :: factor
(use-package fuel :no-require t :defer t :ensure t)

;;;; gauche-manual
;; https://code.google.com/p/gauche-manual-el/
(use-package gauche-manual :no-require t
  :commands gauche-manual
  :defines scheme-mode-map
  :config
  (add-hook 'scheme-mode-hook
            (lambda ()
              (bind-key "C-c C-f" 'gauche-manual scheme-mode-map))))

;;;; gccsense (obsolete)
;; → auto-complete-clang-async に移行
;; http://cx4a.org/software/gccsense/manual.ja.html
;; <2013/02>
;; gcc-code-assist をコンパイルしようとすると、
;; mpfr がインストールされているにも関わらず、
;;   ld: library not found for -lmpfr
;; と出てくる。ライブラリのインストール方法の問題か。
;; (and
;;  (executable-find "gcc-code-assist")
;;  (executable-find "g++-code-assist")
;;  (lazyload
;;      (gccsense-complete
;;       gccsense-flymake-setup
;;       ac-complete-gccsense
;;       (add-hook
;;        'c-mode-common-hook
;;        (lambda ()
;;           (local-set-key (kbd "C-c .") 'ac-complete-gccsense))))
;;      "gccsense"))

;;;; geben
;; DBGp プロトコルを使ったデバッガ
;; https://code.google.com/p/geben-on-emacs/
;; PHPでは XDEBUG_SESSION_START を設定
(use-package geben :no-require t :ensure t
  :commands (geben))

;;;; geiser-mode (obsolete)
;; Scheme REPL and documentation browser.
;; following is defined in autoload.
;; (add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)

;;;; ghc
;; haskell-mode の文書参照等の強化
;; % cabal install ghc-mod
(use-package ghc :no-require t :defer t :ensure t
  :if (executable-find "ghc-mod")
  :init
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

;;;; git-commit-mode （利用禁止）

;;;; git-gutter-fringe+
;; fringe 版を使う。（本家は遅い）
(use-package git-gutter-fringe+ :no-require t :ensure t
  :bind ("C-c M" . git-gutter+-toggle-fringe)
  :config
  (set-variable 'git-gutter+-lighter
        (propertize "溝" 'face '(:foreground "blue"))))

;;;; gnu-apl-mode
;; % brew install gnu-apl
;; quail input method "APL-Z" is also registered.
;; http://commons.wikimedia.org/wiki/File:GNU_APL_keyboard_layout.png
(use-package gnu-apl-mode :no-require t :ensure t
  ;; :init
  ;; (register-input-method "APL-Z" "Input mode for APL"
  ;;                        'apl-mode "APL-Z")
  :mode ("\\.apl\\'" . gnu-apl-mode)
  :interpreter ("gnu-apl" . gnu-apl-mode))

;;;; go-autocomplete
(use-package go-autocomplete :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete)))

;;;; go-eldoc
;; http://golang.org/
;; http://golang.org/doc/gdb
;; http://unknownplace.org/archives/golang-gdb-osx.html
(use-package go-eldoc :no-require t :defer t :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;;;; go-mode
;; - autoload :: ("\\.go\\'" . 'go-mode)
;; M-x helm-go-packages
(use-package go-mode :no-require t :defer t :ensure t
  :defines (go-mode-map)
  ;;:config
  ;; quickrun へ移行。
  ;;(with-eval-after-load 'smart-compile
  ;;  (add-to-list 'smart-compile-alist '("\\.go$" . "go build %f")))
  )

;;;; gradle-mode
;; Ant/Maven に代わる groovy DSL 型ビルドツール
(use-package gradle-mode :no-require t :ensure t
  :mode ("\\.gradle$" . gradle-mode))

;;;; graphviz-dot-mode
;; - autoload :: ("\\.dot\\'" . graphviz-dot-mode) ("\\.gv\\'" . graphviz-dot-mode)
(use-package graphviz-dot-mode :no-require t :defer t :ensure t)

;;;; grizzl
;; Emacs Lisp の検索ツール
(use-package grizzl :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'projectile
    (set-variable 'projectile-completion-system 'grizzl)))

;;;; groovy-mode
;; http://groovy.codehaus.org/
;; auto-mode-alist 追加は自動で行われる。
(use-package groovy-mode :no-require t :defer t :ensure t)

;;;; haml-mode
;; HTMLやそのテンプレートをより見やすい構文で扱う。
;; RoRやmerbで利用。
(use-package haml-mode :no-require t :defer t :ensure t)

;;;; haskell-mode
;; ヘルパー類
;;   % cabal update
;;   % cabal install happy alex # 常に必要
;;   % cabal install stylish-haskell present
;; - hlint で文法チェックする場合 (C-c C-c)
;;   % cabal install hlint
;; - hoogle で検索する場合 (C-c C-h)
;;   % cabal install hoogle
;;   % hoogle data
;; - 保存時にjump tag生成 (haskell-tags-on-save)
;;   % cabal insgtall hasktags
(use-package haskell-mode :no-require t :ensure t
  :defines (haskell-mode-map haskell-cabal-mode-map)
  :mode ("\\.purs\\'" . haskell-mode) ;; PureScript でも haskell-mode を利用する。
  :config
  (bind-key "C-c C-l" 'haskell-process-load-or-reload  haskell-mode-map)
  (bind-key "C-`" 'haskell-interactive-bring           haskell-mode-map)
  (bind-key "C-c C-z" 'haskell-interactive-switch      haskell-mode-map)
  (bind-key "C-c C-n C-t" 'haskell-process-do-type     haskell-mode-map)
  (bind-key "C-c C-n C-i" 'haskell-process-do-info     haskell-mode-map)
  (bind-key "C-c C-n C-c" 'haskell-process-cabal-build haskell-mode-map)
  (bind-key "C-c C-n c" 'haskell-process-cabal         haskell-mode-map)
  (bind-key "SPC" 'haskell-mode-contextual-space       haskell-mode-map)
  ;; タグジャンプ
  (bind-key "M-." 'haskell-mode-jump-to-def            haskell-mode-map)
  ;; メッセージバッファに カーソル上の関数の API を表示。
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (with-eval-after-load 'haskell-customize
    ;; cabal sandbox で haskell を動かす
    (set-variable 'haskell-process-type 'cabal-repl)
    (set-variable 'haskell-process-suggest-remove-import-lines t)
    (set-variable 'haskell-process-auto-import-loaded-modules t)
    (set-variable 'haskell-process-log t))
  (with-eval-after-load 'haskell-font-lock
    (set-variable 'haskell-font-lock-symbols 'unicode))
  ;; 保存時にtag生成
  (when (executable-find "hasktags")
    (set-variable 'haskell-tags-on-save t))
  ;; cabal sandbox での動作
  (with-eval-after-load 'haskell-cabal
    (bind-key "C-c C-z" 'haskell-interactive-switch     haskell-cabal-mode-map)
    (bind-key "C-c C-k" 'haskell-interactive-mode-clear haskell-cabal-mode-map)
    (bind-key "C-c C-c" 'haskell-process-cabal-build    haskell-cabal-mode-map)
    (bind-key "C-c c" 'haskell-process-cabal            haskell-cabal-mode-map)
    ))

;;;; hack
;; http://hacklang.org/
;; Macでのインストール： https://github.com/facebook/hhvm/wiki/Building-and-installing-HHVM-on-OSX-10.9
;; /usr/share/hhvm/hack/emacs,

;;;; html5-el
;; https://github.com/mavit/html5-el/
;; 時々、 make relaxng で最新版のスキーマに更新する。
;; data-*="value" には RelaxNGは対応しきれないので注意。
(use-package whattf-dt :no-require t :defer t
  :init
  (with-eval-after-load 'nxml-mode
    (require 'whattf-dt))
  (with-eval-after-load 'rng-loc
    (pushnew (expand-file-name
              "schemas.xml"
              (file-name-directory (locate-library "rng-loc")))
             rng-schema-locating-files)))

;;;; idris-mode
;; - web :: http://idris-lang.org/
;; - source :: https://github.com/idris-hackers/idris-mode
;; - requirements :: boehmgc, libffi, gmp, llvm
;; - install :: % cabal install idris
;; - memo :: http://mandel59.hateblo.jp/entry/2013/09/02/184831
;; - tutorial :: http://eb.host.cs.st-andrews.ac.uk/writings/idris-tutorial.pdf
;; - autoload :: '("\\.idr$" . idris-mode)
(use-package idris-mode :no-require t :defer t :ensure t)

;;;; inf-ruby
;; M-x run-ruby
(use-package inf-ruby :no-require t :defer t :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

;;;; irony
;; libclang を使ったチェックモード
;; https://github.com/Sarcasm/irony-mode
(use-package irony :no-require t :defer t :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

;;;; irony-eldoc
(use-package irony-eldoc :no-require t :defer t :ensure t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;;;; j-mode
;; https://github.com/zellio/j-mode
(use-package j-mode :no-require t :ensure t
  :mode ("\\.ij[rstp]$" . j-mode)
  :config
  (let ((j-console "/Applications/j64-701/bin/jconsole"))
    (when (file-exists-p j-console)
      (set-variable 'j-console-cmd j-console))))

;;;; javacc-mode
;; EmacsWiki
(use-package javacc-mode :no-require t
  :mode ("\\.jj$" . javacc-mode)
  :config
  (with-eval-after-load 'quickrun
    ;; まだ暫定。次使う機会（？）に整備。
    ;;(quickrun-add-command "javacc"
    ;;                      '((:command "javacc")
    ;;                        (:exec . ("%c %s")))
    ;;                      :mode javacc-mode)
    ;;(quickrun-add-command "jjtree"
    ;;                      '((:command "jjtree")
    ;;                        (:exec . ("%c %s")))
    ;;                      :mode javacc-mode)
    ;;(setf (alist-get "\\.jj$" smart-compile-alist) "javacc %f")
    ;;(setf (alist-get "\\.jjt$" smart-compile-alist) "jjtree %f"))
  ))

;;;; javap-mode
;; Java のディスアセンブルファイル用色付けモード
(use-package javap-mode :no-require t :ensure t
  :mode ("\\.javap$" . javap-mode))

;;;; jinja2-mode
;; Python用テンプレート言語
;; auto-mode-alist は自動設定。

;;;; js-comint
;; M-x run-js
;;(use-package js-comint :no-require t :defer t :ensure t
;;  :config
;;  (set-variable 'inferior-js-program-command
;;                (when (executable-find "node")
;;                  "node --interactive")))

;;;; js-doc
;; Document :: http://d.hatena.ne.jp/mooz/20090820/p1
(use-package js-doc :no-require t :ensure t
  :commands (js-doc-insert-function-doc
             js-doc-insert-tag)
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key "\C-ci" 'js-doc-insert-function-doc)
              (local-set-key "@" 'js-doc-insert-tag)))
  :config
  (set-variable 'js-doc-mail-address user-mail-address)
  (set-variable 'js-doc-author (format "KAWABATA Taichi <%s>" user-mail-address))
  (set-variable 'js-doc-url "http://github.com/kawabata/")
  (set-variable 'js-doc-license "The MIT License"))

;;;; js2-mode
(use-package js2-mode :no-require t :ensure t
  :mode ("\\.js$" . js2-mode)
  ;;:config
  ;;(set-variable 'js2-consistent-level-indent-inner-bracket-p 1)
  ;;(add-hook 'js2-mode-hook
  ;;          (lambda ()
  ;;            (js2-indent-line js2-basic-offset) ))
  ;;(add-hook 'js2-mode-hook
  ;;          (lambda ()
  ;;            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
  ;;; 後で確認
  ;;(when (locate-library "smart-tabs-mode")
  ;;  (add-hook js2-mode-hook 'smart-tabs-advice)
  )

;;;; js2-refactor
;; js2-mode 用リファクタリングツール
;; https://www.youtube.com/watch?v=-7yMWD1wUu4
(use-package js2-refactor :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'js2-mode
    (require 'js2-refactor)))

;;;; js3-mode (abstain)
;; しばらく利用中止。

;;;; json-mode
;; - autoload :: ("\\.json$" . json-mode)
;; flycheck-define-checker
;; % npm install jsonlint -g
;; jsonlint -V schema...

;;;; json-reformat
;; M-x json-reformat-region

;;;; json-snatcher
;; M-x jsons-print-path （値へのパスを表示）

;;;; jss
;; interface to webkit and mozilla debuggers

;;;; julia-mode
;; http://julialang.org/
(use-package julia-mode :no-require t :ensure t
  :mode ("\\.jl\\'" . julia-mode))

;;;; kibit-mode
;; clojure用の文法チェッカ
;; lein のインストールが必要。

;;;; less-css-mode
;; sass に一本化

;;;; lilypond-mode
(use-package lilypond-mode :no-require t
  :mode (("\\.ly$" . LilyPond-mode)
         ("\\.ily$" . LilyPond-mode)))

;;;; lua-mode
;; - autoload :: ("\\.lua$" . lua-mode)
(use-package lua-mode :no-require t :defer t :ensure t)

;;;; malabar (obsolete)
;; （$ git clone https://github.com/espenhw/malabar-mode.git）
;; 上記は2012年現在、メンテナンスされていない。
;; $ git clone https://github.com/buzztaiki/malabar-mode
;; $ git pull
;; $ mvn package → 完成したファイルをsite-lisp へインストール

;; * mvn は、version 3.04以降 を使うこと。
;; * emacs は、23.2 以降を使うこと。
;;(defvar malabar-dir
;;  (expand-file-name "~/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT"))
;;(when (file-directory-p malabar-dir)
;;  (add-to-list 'load-path (concat malabar-dir "/lisp")))
;;(when (locate-library "malabar-mode")
;;  (autoload 'malabar-mode "malabar-mode" nil t)
;;  (add-to-auto-mode-alist '("\\.java\\'" . malabar-mode))
;;  (eval-after-load 'malabar-mode
;;    '(progn
;;       (require 'cedet)
;;       (set-variable 'malabar-groovy-lib-dir (concat malabar-dir "/lib"))
;;       ;; 普段使わないパッケージを import 候補から除外
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^java\\.awt\\..*$")
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^com\\.sun\\..*$")
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^org\\.omg\\..*$")
;;       ;; **** Malabar Groovy : Customization of malabar-mode's inferior Groovy.
;;       ;; 日本語だとコンパイルエラーメッセージが化ける
;;       (set-variable 'malabar-groovy-java-options '("-Duser.language=en"))
;;       (add-hook 'malabar-mode-hook
;;                 (lambda ()
;;                   (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                             nil t))))))

;;;; markdown-mode
;; 詳細は http://jblevins.org/projects/markdown-mode/
;; 原則は GitHub Flavored Markdown Mode (gfm-mode) にする。
(use-package markdown-mode :no-require t :ensure t
  :mode (("\\.md$" . gfm-mode)
         ("\\.commonmark$" . gfm-mode))
  :config
  (set-variable 'markdown-coding-system 'utf-8)
  (set-variable 'markdown-content-type " ")
  (set-variable 'markdown-command
                (or (executable-find "multimarkdown")
                    (executable-find "markdown"))))

;;;; mathematica-mode (obsolete)
;; → emathica.el に移動
;;(defvar mathematica-command-line
;;  (executable-find "/Applications/Mathematica.app/Contents/MacOS/MathKernel"))
;;(when mathematica-command-line
;;  (lazyload (mathematica
;;             mathematica-mode
;;             (add-to-auto-mode-alist '("\\.m\\'" . mathematica-mode)))
;;      "mathematica-mode"))

;;;; maxima
;; /opt/local/share/maxima/5.28.0/emacs/
;; imaxima で必要なもの：dvips + breqn.sty
;; macports でインストールする場合は、 port install imaxima とすると
;; TeXLive がまるまるインストールされようとするのでダメ。
;; maxima だけインストールしておけば、下記ですぐに imaxima が使える。
(let ((maxima-path
       (car (file-expand-wildcards "/opt/local/share/maxima/*/emacs"))))
  (when (and maxima-path (file-directory-p maxima-path))
    (add-to-load-path maxima-path)
    (autoload 'maxima-mode "maxima" "Maxima mode" t)
    (autoload 'maxima "maxima" "Maxima interaction" t)
    (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
    (autoload 'maxima "maxima" "Maxima interaction" t)
    (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
    (pushnew '("\\.max" . maxima-mode) auto-mode-alist :test 'equal)))

;;;; math++ (obsolete)
;; → wolfram-mode に移行。

;;;; mediawiki
;; リンクや文字修飾などのMediaWikiを編集するための便利機能が多数。
;; M-x mediawiki-draft
;; M-x mediawiki-draft-page
;; M-x mediawiki-draft-buffer

;; M-x mediawiki-mode
;; M-x mediawiki-open

;;;; mustache
;; 比較的、言語に中立的なテンプレート。JavaScript等で便利。
;; Emacs の場合は、キー (:key) ＋非括弧関数 (,) の方が便利。
;; c.f. tkw-org-publish-gepub-script

;;;; mustache-mode
;; Logic-less template (http://mustache.github.io/)
(use-package mustache-mode :no-require t :ensure t
  :mode ("\\.mustache$" . mustache-mode))

;;;; nim-mode
;; (nimrod-mode)
;; http://nim-lang.org/
(use-package nim-mode :no-require t :ensure t
  :mode ("\\.nim$" . nim-mode))

;;;; nrepl <epla> (abstain)
;; → cider に移行
;; - M-x nrepl-jack-in で接続 (lein が必要)
;; - コマンドラインで "lein repl" で起動して M-x nrepl で 58794 に接続。

;;;; nxhtml-mode (abstain)
;; 本ディレクトリには".nosearch"があるので、本来は読み込めない。
;;(when (locate-library "nxhtml/autostart")
;;  (load-library "nxhtml/autostart"))

;;;; omn-mode
(use-package omn-mode :no-require t :ensure t
  :mode (("\\.pomn\\'" . omn-mode)
         ("\\.omn\\'" . omn-mode)))

;;;; omnisharp
;; C# Omnicompletion
;; - build
;; % git clone https://github.com/nosami/OmniSharpServer.git
;; % cd OmniSharpServer
;; % git submodule update --init --recursive
;; % xbuild
;; - run
;; 開発ディレクトリで、 mono /path_to/OmniSharpServer.git/OmniSharp/bin/Debug/OmniSharp.exe を実行
(use-package omnisharp :no-require t :defer t :ensure t
  :diminish " O#"
  :defines (csharp-mode-map)
  :init
  (defun tkw-omnisharp-port-listen-p ()
    (when (= 0 (shell-command "netstat -an | grep 2000"))
      (omnisharp-mode)))
  (add-hook 'csharp-mode-hook 'tkw-omnisharp-port-listen-p)
  :config
  (define-key csharp-mode-map (kbd "M-.") 'omnisharp-auto-complete))

;;;; php-mode
(use-package php-mode :no-require t :defer t :ensure t)

;;;; php+-mode (abstain)
;; 同梱されている string-utils.el は名前空間規則に従っていない、Roland
;; Walker氏の同名ツールと衝突する、等の理由でインストール禁止。

;;;; powershell
;; - 参照 :: http://blogs.msdn.com/b/dotnetinterop/archive/2008/04/10/run-powershell-as-a-shell-within-emacs.aspx
;; - 参考 :: http://www.emacswiki.org/emacs/PowerShell
(use-package powershell :no-require t :ensure t
  :commands (powershell))

;;;; processing-mode
;; - autoload :: '("\\.pde$" . processing-mode)
(use-package processing-mode :no-require t :defer t :ensure t)

;;;; rd-mode / rd-mode-plus (obsolete)
;; RubyDoc は現在、ほとんど使われなくなったため使用中止。
;;(if (locate-library "rd-mode-plus")
;;    (autoload 'rd-mode "rd-mode-plus")
;;  (if (locate-library "rd-mode")
;;      (autoload 'rd-mode "rd-mode")))
;;(when (or (locate-library "rd-mode") (locate-library "rd-mode-plus"))
;;  (add-to-auto-mode-alist '("\\.rd$" . rd-mode))
;;  ;; howm を RDで管理するのはやめる。
;;  ;;(add-to-auto-mode-alist '("\\.howm$" . rd-mode))
;;  )
;;(eval-after-load 'rd-mode
;;  '(progn
;;     (defvar rd-mode-hook nil)          ; =endの“...”への置換防止
;;     (add-hook 'rd-mode-hook 'rd-show-other-block-all)))

;;;; review-mode
;; ただいま改良中。
(use-package review-mode :no-require t :defer t :ensure t)

;;;; rnc-mode
;; http://www.pantor.com
(use-package rnc-mode :no-require t :ensure t
  :mode ("\\.rnc\\'" . rnc-mode)
  :config
  (set-variable 'rnc-indent-level 2) ;; trangの出力にあわせる。
  (let ((jing (executable-find "jing.jar")))
    (when jing
      (set-variable 'rnc-enable-flymake t)
      (set-variable 'rnc-jing-jar-file jing))))

;;;; rinari
;; Ruby on Rails 開発環境、最近は使ってない。
;;(use-package rinari :ensure t
;;  :commands (global-rinari-mode))

;;;; rsense (abstain)
;; Rubyのための開発援助ツール.
;;(defvar rsense-home (expand-file-name "~/src/rsense-0.3"))
;;(when (file-directory-p rsense-home)
;;  (load-library "rsense")
;;  ;; 随時、http://www.ruby-lang.org/ja/man/archive/ からダウンロードして最新版をインストール。
;;  ;; e.g. ~/Resources/ruby/ruby-refm-1.9.2-dynamic-20100929/
;;  (set-variable 'rsense-rurema-home "~/Resources/ruby")
;;  (set-variable 'rsense-rurema-refe "refe-1_9_2") ;; 上記ディレクトリ下にあること！
;;  (add-hook 'ruby-mode-hook
;;            (lambda ()
;;              (local-set-key (kbd "C-c .") 'ac-complete-rsense))))

;;;; robe
;; Code navigation, documentation lookup and completion for Ruby

;;;; rubocop
;; % gem install rubocop
;; flycheck が自動的に行うので不要。
(use-package rubocop :no-require t :defer t :ensure t
  :if (executable-find "rubocop")
  :init
  (add-hook 'ruby-mode-hook 'rubocop-run-on-current-file)
  (add-hook 'enh-ruby-mode-hook 'rubocop-run-on-current-file))

;;;; ruby-block
;; begin ～ end の対応関係をハイライトする。
(use-package ruby-block :no-require t :defer t :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-block-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-block-mode)
  (with-eval-after-load 'enh-ruby-mode
    (require 'ruby-block)))

;;;; ruby-electric
;; 括弧の自動挿入・インデント
(use-package ruby-electric :no-require t :defer t :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-electric-mode)
  (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

;;;; rubydb3x (abstain)
;; realgud に移行したので使用中止。
;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/rubydb3x.el
;; (lazyload (rubydb) "rubydb3x")

;;;; rust-mode
;; - autoload :: '("\\.rs\\'" . rust-mode)
;; http://gifnksm.hatenablog.jp/entry/2013/07/15/170736 (Rust 基礎文法最速マスター)
(use-package rust-mode :no-require t :defer t :ensure t
  :config
  ;; flycheck へ移行
  ;;(require 'flymake)
  ;;(defun flymake-rust-init ()
  ;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
  ;;                     'flymake-create-temp-inplace))
  ;;         (local-file (file-relative-name
  ;;                      temp-file
  ;;                      (file-name-directory buffer-file-name))))
  ;;    (list "rustc" (list "--no-trans" local-file))))
  ;;(pushnew '(".+\\.r[cs]$" flymake-rust-init
  ;;           flymake-simple-cleanup flymake-get-real-file-name)
  ;;            flymake-allowed-file-name-masks)
  )

;;;; sass-mode
(use-package sass-mode :no-require t :defer t :ensure t)

;;;; scala-mode2
;; - autoload :: ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)
(use-package scala-mode2 :no-require t :defer t :ensure t)

;;;; sclang
;; 【注意】パッケージに含まれる tree-widget.el は削除すること！
;; SuperCollider は直接使わず、overtone をできるだけ使う。
;; http://sourceforge.net/projects/supercollider/files/Source/3.6/
;; からソースをダウンロードして、
;; $ cp -r ./SuperCollider-Source/editors/scel ~/.emacs.d/site-lisp
;; sclang, scsynth の両方を
;; ./SuperCollider-Source/platform/mac/MOVED_STUFF.txt
;; に書かれた通りに設定してパスを通す。
;; TODO sclang-vars.el.in の編集して sclang-vars.el として保存。
(use-package sclang :no-require t
  :if (and (executable-find "sclang")
           (executable-find "scsynth"))
  :mode ("\\.scd$" . sclang-mode))

;;;; sclang-snippets (obsolete)
;; yasnippet ロード時に勝手に読み込まれるため、インストール中止。

;;;; shm
;; Structured Haskell Mode
;; % cabal install structured-haskell-mode
(use-package shm :no-require t :defer t :ensure t
  :if (executable-find "structured-haskell-mode")
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;;;; skewer-mode
;; JavaScript統合開発環境。
;; Webページからのジャックインには、skewer-start 後、以下のBookmarkletを用いる。
;; javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
;; https://github.com/magnars/.emacs.d/blob/master/setup-skewer.el
(use-package skewer-mode :no-require t :ensure t
  :commands (skewer-demo)
  :config
  (progn
    (defvar css-mode-hook nil) ;; for compatibility with skewer-css, which assumes nonstandard css-mode.
    (require 'skewer-repl)
    (require 'skewer-html)
    (require 'skewer-css)

    (defvar httpd-port nil)
    (defun skewer-start ()
      (interactive)
      (let ((httpd-port 8023))
        (httpd-start)
        (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

    (defun skewer-demo ()
      (interactive)
      (let ((httpd-port 8024))
        (run-skewer)
        (skewer-repl)))

    ;;(when (require 'mouse-slider-mode nil :no-error)
    ;;  (pushnew '(js2-mode . skewer-eval-defun)
    ;;           mouse-slider-mode-eval-funcs))
    ))

;;;; slime
(use-package slime :no-require t :defer t :ensure t
  :init (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  :config
  (slime-setup '(slime-repl)))

;;;; slime-js (abstain)
;; skewer-mode に移行するため使用中止。
;; % cd /path/to/npm-project
;; (prepare package.json)
;; % npm install -g swank-js
;;    → /usr/local/bin/swank-js がインストールされる。（必須）
;; % swank-js
;; M-x slime-connect
;; M-x slime-repl
;;(lazyload () "slime-js"
;;  (bind-key "C-x C-e" 'slime-js-eval-current slime-js-minor-mode-map)
;;  (bind-key "C-c C-e" 'slime-js-eval-and-replace-current slime-js-minor-mode-map))
;;(lazyload () "css-mode"
;;  (add-hook ‘css-mode-hook
;;              (lambda () (define-key css-mode-map “\M-\C-x” ‘slime-js-refresh-css)
;;                (define-key css-mode-map “\C-c\C-r” ‘slime-js-embed-css))))
;; https://raw.github.com/magnars/.emacs.d/master/setup-slime-js.el
;;(lazyload (slime-js-jack-in-node slime-js-jack-in-browser) "setup-slime-js"
;;  (bind-key [f5] 'slime-js-reload)
;;  (add-hook 'js2-mode-hook
;;          (lambda ()
;;            (slime-js-minor-mode 1))))

;;;; smalltalk-mode
;; Gnu Smalltalk に付属。 loadpath追加。
(use-package smalltalk-mode :no-require t
  :mode ("\\.st$" . smalltalk-mode))

;;;; sparql-mode
(use-package sparql-mode :no-require t :ensure t
  :mode ("\\.sparql$" . sparql-mode))

;;;; sql-indent
(use-package sql-indent :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'sql
    (require 'sql-indent))
  :config
  (set-variable 'sql-indent-offset 4)
  (set-variable 'sql-indent-maybe-tab t))

;;;; squirrel-mode
;; https://launchpadlibrarian.net/59321067/squirrel-mode.el
(use-package squirrel-mode :no-require t
  :mode ("\\.nut\\'" . squirrel-mode)
  )

;;;; taskjuggler-mode
;; http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode (official)
;; org-taskjuggler is better replacement.
;; % gem install taskjuggler
;; ~/.rbenv/versions/2.0.0-p195/lib/ruby/gems/2.0.0/gems/taskjuggler-3.5.0/bin/tj3
(use-package taskjuggler-mode :no-require t
  :mode (("\\.tjp\\'" . taskjuggler-mode)
         ("\\.tji\\'" . taskjuggler-mode)
         ("\\.tjsp\\'" . taskjuggler-mode)))

;;;; tern
;; javascript analyzer
;; % git clone https://github.com/marijnh/tern
;; % cd tern
;; % sudo npm install
;; M-.  Jump to the definition of the thing under the cursor.
;; M-,  Brings you back to last place you were when you pressed M-..
;; C-c C-r  Rename the variable under the cursor.
;; C-c C-c  Find the type of the thing under the cursor.
;; C-c C-d  Find docs of the thing under the cursor. Press again to open the associated URL (if any).
(use-package tern :no-require t :ensure t
  :commands tern-mode
  :if (let ((tern-dir "~/cvs/tern"))
        (when (file-directory-p tern-dir)
          (pushnew (concat tern-dir "/emacs") load-path)
          (pushnew (concat tern-dir "/bin")   exec-path)))
  :init
  (add-hook 'js2-mode-hook (command (tern-mode t))))

;;;; terraform-mode
;; - autoload : '("\\.tf\\'" . terraform-mode)
(use-package terraform-mode :no-require t :defer t :ensure t)

;;;; textile-mode
;; http://dev.nozav.org/textile-mode.html
(use-package textile-mode :no-require t :ensure t
  :commands (textile-mode)
  :mode (("\\.textile\\'" . textile-mode)))

;;;; tss
;; https://github.com/aki2o/emacs-tss
;; typescript-tools (https://github.com/clausreinke/typescript-tools)
;; を使って、補完・ドキュメント表示・文法チェックを行なう。
(use-package tss :no-require t :defer t :ensure t
  :if (executable-find "tss")
  :init
  (add-hook 'typescript-mode-hook 'tss-setup t)
  :config
  (set-variable 'tss-jump-to-definition-key "C->"))

;;;; ttcn-mode
;; オリジナルファイルの (kill-all-local-variables) は削除すること。
;; https://github.com/dholm/ttcn-el/
(use-package ttcn3 :no-require t
  :mode ("\\.ttcn3?\\'" . ttcn-3-mode))
;; Test Managers は必要ないのと、 provide 文がないのでコメントアウト。
;;(lazyload (forth-mode) "forth")
;;(lazyload (tm-functions) "tm")

;;;; typescript
;; http://www.typescriptlang.org/
;; typescript.el は、公式Webの "tools" からダウンロード
;; reference は https://developers.google.com/chrome-developer-tools/ を参考に。
;; （旧）TypeScript.el は、typescript.el とリネームする。
;; （新）typescript.el は、emacs-tss に含まれているものを使用する。
(use-package typescript :no-require t
  :mode ("\\.ts$" . typescript-mode))

;;;; typed-clojure-mode
;; TODO 次 clojure を使う時に試用。

;;;; tuareg
;; Ocaml の実行・編集・デバッガ
;; - Manual :: http://www.typerex.org/files/cheatsheets/tuareg-mode.pdf
;; - autoload :: ("\\.ml[iylp]?\\'" . tuareg-mode)
;; - tuareg-run-ocaml で実行
;; - C-x SPC (M-x ocamldebug) :: debugger
(use-package tuareg :no-require t :defer t :ensure t
  :config
  (set-variable 'tuareg-use-smie t))

;;;; visual-basic-mode
(use-package visual-basic-mode :no-require t
  :mode ("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" . visual-basic-mode))

;;;; web-mode
;; http://web-mode.org/
;; http://fukuyama.co/web-mode
(use-package web-mode :no-require t :ensure t
  :defines (web-mode-map)
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ;; デフォルトでは、HTMLは web-mode にせず、手動で設定する。
         ;; 巨大HTMLだと web-mode はフリーズすることがあるため。
         ;;("\\.html?$"     . web-mode)
         )
  :config
  ;; 臨時！臨時！
  (setf (alist-get "\\.html?$" auto-mode-alist) 'fundamental-mode)
  (setf (alist-get "\\.svgz?\\'" auto-mode-alist) 'fundamental-mode)
  (set-variable 'web-mode-markup-indent-offset 2)
  ;;(set-variable 'web-mode-html-offset   2)
  ;;(set-variable 'web-mode-css-offset    2)
  ;;(set-variable 'web-mode-script-offset 2)
  ;;(set-variable 'web-mode-php-offset    2)
  ;;(set-variable 'web-mode-java-offset   2)
  ;;(set-variable 'web-mode-asp-offset    2)
  ;;(with-eval-after-load 'auto-complete
  ;;  (add-to-list 'ac-modes 'web-mode))
  ;; quickrun へ移行。
  (add-hook 'web-mode-hook
            (lambda () (set-variable 'tab-width 2)))
  (bind-key "C-M-n" 'web-mode-element-end web-mode-map)
  (bind-key "C-M-p" 'web-mode-element-beginning web-mode-map))

;;;; wsd-mode
;; https://www.websequencediagrams.com/
;; ("\\.wsd$" . wsd-mode)
(use-package wsd-mode :no-require t :defer t :ensure t)

;;;; yaml-mode
(use-package yaml-mode :no-require t :defer t :ensure t
  :defines (yaml-indent-offset)
  :config
  (set-variable 'yaml-indent-offset 4)
  (defun tkw-yaml-indent-level ()
    (looking-at " *")
    (1+ (/ (- (match-end 0) (match-beginning 0)) yaml-indent-offset)))
  (defun tkw-yaml-setup ()
    (set-variable 'outline-regexp "\\( \\)*.+: *\\(&.+?\\)?\\(#.*?\\)?$")
    (set-variable 'outline-level 'tkw-yaml-indent-level)
    (outline-minor-mode 1))
  (add-hook 'yaml-mode-hook 'tkw-yaml-setup))

;;;; yang-mode
;; - 参照 :: http://www.yang-central.org/twiki/pub/Main/YangTools/yang-mode.el
(use-package yang-mode :no-require t
  :mode ("\\.yang\\'" . yang-mode))

;;; 非標準アプリケーション
;;;; 2048-game
(use-package 2048-game :no-require t :ensure t
  :commands (2048-game))

;;;; ac-inf-ruby (abstain)
;;(use-package ac-inf-ruby :defer t
;;  :init
;;  (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))

;;;; ag
;; * agのインストール
;;   : % brew install the_silver_searcher (Mac)
;;   : % apt-get install the_silver_searcher (Ubuntu 13.10)
(use-package ag :no-require t :ensure t
  :bind (("M-s a" . ag)
         ;; M-s g を grep から上書きする。
         ("M-s g" . ag)))

;;;; all
;; occur (list-matching-lines) の拡張版。検索結果を編集したら元ファイルに反映される。
;; - autoload :: all

;;;; all-ext (abstain)
;; http://d.hatena.ne.jp/rubikitch/20130202/all
;; 対象行を絞り込んでからまとめて編集する
;;(use-package all-ext
;;  :init
;;  (with-eval-after-load 'all
;;    (require 'all-ext nil :no-error)))

;;;; alpha (abstain)
;; フレームの透明度設定ツール
;; (X-Window版では利用不可)
;; * 問題点
;;   勝手に、"C-?" に transparency-set-value を割り当てている。
;;(use-package alpha :defer t
;;  :commands (transparency-increase
;;             transparency-decrease)
;;  :bind (("M-C->" . transparency-increase)
;;         ("M-C-<" . transparency-decrease))
;;  :config
;;  ;; 初期値は 90% にしておく。
;;  (transparency-set-value 90))

;;;; apropos-fn+var
;; M-x apropos を、function と variables に分離する。

;;;; apt-utils
;; ※ melpa から消失
;; make-local-hook を emacswiki から削除。
;; - autoloads :: apt-utils-search, apt-utils-show-package
;;(use-package apt-utils :defer t
;;  :if (executable-find "apt-get"))

;;;; ascii-art-to-unicode (abstain)
;; M-x aa2u
;; Box Drawing の UCS化 (u2500-u251f)
;; カラムが日本語では揃わないので不要。

;;;; back-button (abstain)
;; "戻る" ボタン

;;;; bbdb
;; 人名・住所管理システム。
;;;;; Recordの構造

;; | フィールド   | ラベル               | 構成・内容                                   |
;; |--------------+----------------------+----------------------------------------------|
;; | firstname    | なし                 | string                                       |
;; | lastname     | なし                 | string                                       |
;; | affix        | なし                 | string                                       |
;; | aka          | なし                 | (string ...)                                 |
;; | organization | なし                 | (string ...)                                 |
;; | phone        | home,work.cell,other | ([label phone] ...)                          |
;; | address      | home,work,other      | ([label (street ..) city state country zip]) |
;; | mail         | なし                 | (mail ...)                                   |
;; | xfields      | なし                 | ((symbol . "value") ...)                     |

;; xfield の例
;; | フィールド  |                         |
;; |-------------+-------------------------|
;; | www         | Web Home Page           |
;; | name-format | first-last / last-first |
;; | name-face   |                         |
;; | twitter     | ツイッター              |
;; | skype       |                         |

;;;;; 名前のソート
;; BBDB での名前のソーティングを日本語ベースにする（mecabを使用）
;; 全ての日本語（半角カナ含む）を「ひらがな」に直す関数
(defconst japanese-to-kana-buffer "*jp2kana*")
(defvar japanese-to-kana-process nil)
(defvar japanese-to-kana-hash (make-hash-table :test 'equal))
;; 以下の関数は青空文庫用のyasnippetでも使用する。
(defun japanese-to-kana-string (str)
  "Convert STR to Japanese Kana."
  (if (null (executable-find "mecab")) str)
  (when (null japanese-to-kana-process)
    (set-variable 'japanese-to-kana-process
                  (start-process
                   "mecab" japanese-to-kana-buffer
                   ;; kakasi に必ず "-u" (fflush) を入れておかないと、バッファリングして
                   ;; 答えが返ってこなくなるので注意する。
                   "mecab" "-Oyomi")))
  (or (gethash str japanese-to-kana-hash)
      (with-current-buffer japanese-to-kana-buffer
        ;;(set-buffer-process-coding-system 'euc-jp-unix 'euc-jp-unix)
        (erase-buffer)
        (process-send-string japanese-to-kana-process (concat str "\n"))
        (while (= (buffer-size) 0)
          (accept-process-output nil 0 50))
        (puthash str (substring (buffer-string) 0 -1)
                 japanese-to-kana-hash))))

;;;;; カスタマイズ部分
(use-package bbdb :no-require t :ensure t
  :bind ("C-:" . bbdb)
  :mode ("\\.bbdb" . emacs-lisp-mode)
  :commands (bbdb-create)
  :defines (bbdb-default-country bbdb-separator-alist ;; 要改良
            bbdb-legal-postcodes bbdb-legal-postcodes bbdb-mode-map)
  :functions (bbdb-error-retry bbdb-parse-postcode bbdb-read-string bbdb-address-postcode
              bbdb-address-state bbdb-address-city bbdb-edit-address-street
              bbdb-address-streets bbdb-address-country bbdb-record-xfield
              bbdb-record-lastname bbdb-record-firstname bbdb-cache-sortkey
              bbdb-record-cache bbdb-cache-set-sortkey bbdb-japanese-sortkey)
  :config
  (require 'bbdb-com)
  (defun bbdb-edit-address-japan (address)
    "Function to use for address editing for Japanese."
    (let ((postcode (bbdb-error-retry
                     (bbdb-parse-postcode
                      (bbdb-read-string "郵便番号: "
                                        (bbdb-address-postcode address)))))
          (state (bbdb-read-string "県名（州名）: " (bbdb-address-state address)))
          (city (bbdb-read-string "市町村名: " (bbdb-address-city address)))
          (streets (bbdb-edit-address-street (bbdb-address-streets address)))
          (country (bbdb-read-string "国名（英語）: " (or (bbdb-address-country address)
                                                          bbdb-default-country))))
    (list streets city state postcode country)))

  (defun bbdb-format-address-japan (address)
    "Return formatted ADDRESS as a string.
This is the default format; it is used in the Japan.
This function is a possible formatting function for
`bbdb-address-format-list'."
    (let ((country (bbdb-address-country address))
          (streets (bbdb-address-streets address)))
      (concat (bbdb-address-postcode address) "\n"
              (bbdb-address-state address)
              (bbdb-address-city address) "\n"
              (if streets
                  (concat (mapconcat 'identity streets "\n") "\n"))
              (unless (or (not country) (string= "" country))
                (concat "\n" country)))))
  (set-variable 'bbdb-separator-alist
                (cons '(name-last-first "[ ,;]" " ")
                      (assq-delete-all 'name-last-first bbdb-separator-alist)))
  (set-variable 'bbdb-file (locate-user-emacs-file "bbdb.gpg"))
  ;; 日本人主体なので、Last+Firstの順番で表示する。
  (set-variable 'bbdb-name-format 'last-first)
  (set-variable 'bbdb-mail-name-format 'last-first)
  ;; Gnus連携時、サマリバッファの横にBBDB情報を表示する。
  (set-variable 'bbdb-mua-pop-up 'horiz) ;; バグにより２重クオートが必要。（作者報告・対処済）
  ;; BBDBのポップアップ時のウィンドウサイズ。
  ;; 注：MUA使用時のサイズは別途、 `bbdb-mua-pop-up-window-size' で設定するので注意。
  (set-variable 'bbdb-pop-up-window-size 0.3) ;; 0.5 / 4
  ;; 写真画像は、パスに "Last, First.jpg" 名で入っている。
  (set-variable 'bbdb-image 'lf-name)
  (set-variable 'bbdb-image-path
                (list (locate-user-emacs-file "bbdb-images/")))
  (set-variable 'bbdb-message-mail-as-name nil)
  ;; 全てのフィールドを表示する。
  (set-variable 'bbdb-layout 'full-multi-line)
  ;; NANP (north american numbering plan) は使用しない。
  (set-variable 'bbdb-phone-style nil)
  ;; 日本の住所フォーマット
  (set-variable 'bbdb-address-format-list
        '((("USA") "scSpC" "@%s\n@%c@, %S@ %p@\n%C@" "@%c@")
          (t bbdb-edit-address-japan bbdb-format-address-japan "@%c@")))
  (set-variable 'bbdb-default-country "Japan") ;; Japan, Emacs, etc
  ;; 日本の〒番号フォーマットを追加
  (pushnew "^〒?[0-9]\\{3\\}-[0-9]\\{4\\}$"
           bbdb-legal-postcodes :test 'equal)
  (bbdb-initialize 'gnus)
  (bind-key "M-w" 'kill-ring-save bbdb-mode-map)
  ;; (bind-key "O" 'bbdb-insert-new-field bbdb-mode-map) ;; bbdb2
  (bind-key "O" 'bbdb-insert-field bbdb-mode-map) ;; bbdb3
  (set-variable 'bbdb-complete-mail-allow-cycling t)
  ;; -------
  (defun tkw-bbdb-name-add-title (name-addr)
    (save-match-data
      (let* ((ml-title (string-match " ML\" <" name-addr)))
        ;; BBDB の名前の最後が " ML " で終わるならば、様は付けない。
        (if (and (null ml-title)
                 (string-match "\\(\".+?\\)\\(\" <.+\\)" name-addr))
            (concat (match-string 1 name-addr) " 様" (match-string 2 name-addr))
          name-addr))))
  ;; BBDBから名前に変換するとき、敬称を付加する。
  (defadvice bbdb-dwim-mail (after bbdb-dwim-add-title nil activate)
    (setq ad-return-value (tkw-bbdb-name-add-title ad-return-value)))
  ;; キャンセル時は以下の命令を実行する。
  ;; (ad-disable-advice 'bbdb-dwim-mail 'after 'bbdb-dwim-add-title)
  ;; (ad-activate 'bbdb-dwim-mail)
  ;; -------
  ;; Fromの名前を処理するとき、“様”の敬称を消去する。
  ;;(defadvice bbdb-annotate-message-sender
  ;;  (before bbdb-annotate-message-sender
  ;;          (from &optional loudly create-p prompt-to-create-p) activate)
  ;;  (if (and (consp from) (car from))
  ;;      (let* ((name (car from)) (pos (string-match " ?様$" name)))
  ;;        (if pos (setq from (cons (substring name 0 pos) (cdr from)))))))
  ;; キャンセル時の命令
  ;;(ad-disable-advice 'bbdb-annotate-message-sender 'before 'bbdb-annotate-message-sender)
  ;;(ad-activate 'bbdb-annotate-message-sender)
  ;; -------
  ;; BBDB record から、アルファベットまたは平仮名化した日本語を取り出す。
  (defun bbdb-japanese-sortkey (record)
    (downcase
     ;;(let ((furigana (bbdb-get-field record 'furigana)))
     (let ((furigana (bbdb-record-xfield record 'furigana)))
       (japanese-to-kana-string
        (if (= 0 (length furigana))
            (concat (bbdb-record-lastname record)
                    " "
                    (bbdb-record-firstname record))
          furigana)))))
  ;; bbdb.el にある、bbdb-record-sortkey を上書きする。(要kakasi)
  (when (executable-find "mecab")
    (defun bbdb-record-sortkey (record)
      (or (bbdb-cache-sortkey (bbdb-record-cache record))
          (bbdb-cache-set-sortkey
           (bbdb-record-cache record)
           (bbdb-japanese-sortkey record)))))
  ;; 実際にソートする場合は（一回で十分）、emacsを再起動（キャッシュを消
  ;; 去）して、.bbdbのバッファが無いのを確認した上で、
  ;; (bbdb-resort-database)を実行する。

  (defadvice bbdb-rfc822-addresses
    (after remove-honorable-title last (&optional arg) activate)
    "This advice removes honorable titles from the result."
    (dolist (elem ad-return-value)
      (let ((name (car elem)))
        (if (and (stringp name) (string-match " ?様$" name))
            (setcar elem (substring name 0 (match-beginning 0)))))))
  (with-eval-after-load 'bbdb-anniv
    ;; bbdb-anniv
    ;; bbdb には diary-date-forms (1月10日は "1/10" で書き込むこと)
    (set-variable 'bbdb-anniv-alist
                  '((birthday . "%n さんの %d 回目の誕生日")
                    (wedding  . "%n さんの %d 回目の結婚記念日")
                    (anniversary))))
  (with-eval-after-load 'diary-lib
    ;; bbdb-anniv-alist のエントリ内容を、 diary-list-entries に反映させる。
    (add-hook 'diary-list-entries-hook 'bbdb-anniv-diary-entries))
  )

;;;; bibeltex
;; ox-org を使い、
;; #+BIBLIOGRAPHY: example plain
;; を展開する。変換は、C-c C-e O O で確認可能。
;; * トラブルシューティング
;;   ox-org したときに、listp で array が帰るエラーが起きる場合は、
;;   ox.el のソースを読み込み直してみる。
;;(use-package bibeltex :defer t
;;  :init
;;  (with-eval-after-load 'ox-org
;;    (require 'bibeltex)))

;;;; bibretrieve
;  :config
;  ;; bug: bibretrieve-baseに、(provide 'bibretrieve) とあるので
;  ;; 本家が読み込まれないことがある。
;  (load-library "bibretrieve"))

;;;; bookmark+
;; - 参照 :: http://emacswiki.org/emacs/BookmarkPlus
;; - メニュー :: edit 配下の bookmarks に一覧あり。
;; |----------------+--------------------------------------------+------------------------------------|
;; | 通常バッファ   |                                            |                                    |
;; | C-x p m (r m)  | bookmark-set                               |                                    |
;; | C-x p g (r b)  | bookmark-jump                              |                                    |
;; | C-x p e (r l)  | bookmark-bmenu-list                        |                                    |
;; |----------------+--------------------------------------------+------------------------------------|
;; | C-x p x        | bmkp-set-bookmark-file-bookmark            |                                    |
;; | C-x p H        | bmkp-light-bookmarks                       |                                    |
;; | C-x p c a      | bmkp-autofile-set                          | ファイル登録、ファイル名をBM名に。 |
;; |                |                                            | （同名ファイル利用可）             |
;; | C-x p c u      | bmkp-url-target-set                        | URL登録                            |
;; | C-x p c f      | bmkp-file-target-set                       | ファイル登録                       |
;; |----------------+--------------------------------------------+------------------------------------|
;; | C-x p L        | bmkp-switch-bookmark-file-create           | BMファイル切り替え                 |
;; | C-x p l        | bookmark-load                              | 他BMファイルの内容を読み込み       |
;; | C-x p y        | bmkp-set-bookmark-file-bookmark            | BMファイルのブックマーク           |
;; |----------------+--------------------------------------------+------------------------------------|
;; | リストバッファ |                                            |                                    |
;; | r              | bmkp-bmenu-edit-bookmark-name-and-location | 名前の変更                         |
;; | a              | command bmkp-bmenu-show-or-edit-annotation | アノテーションの表示               |
;; | C-u a          | command bmkp-bmenu-show-or-edit-annotation | アノテーションの編集               |
;; | e              | bmkp-bmenu-edit-bookmark-record            | レコード全体の変更                 |
;; | T +            | bmkp-add-tags                              |                                    |
;; | M-d >          | bmkp-bmenu-dired-marked                    |                                    |
;; | L              | bmkp-switch-bookmark-file-create           |                                    |
;; | M-m            | bmkp-bmenu-mark-all                        |                                    |
;; | D              | bmkp-bmenu-delete-marked                   |                                    |
;; |----------------+--------------------------------------------+------------------------------------|
;; | Dired          |                                            |                                    |
;; | M-b (M-A-b)    | diredp-do-bookmark                         |                                    |

;; ソースコードリーディング
;; (1) C-x p L で専用ブックマークを作成・開く。
;; (2) ソースコードを読む。
;; (3) メモしたい場所で、C-x p m
;; (4) 名前とメモを記入
;; (5) 一覧は C-x r l
;; (6) ハイライトされた部分のメモを読むには C-x j h

;; "open" で開くファイル一覧

(defvar openwith-associations)
(defvar bmkp-default-handlers-for-file-types)
(let* ((exts '("asta" "jude" "mm" "zip" "nb" "mp3" "mp4" "djvu";; "pdf"
               "ppt" "pptx" "potx" "doc" "docx" "xls" "xlsx"
               "odt" "enc" "wav" "sxi" "rtf" "tif" "cap" "dmg"))
       (regexp (concat
                "\\." (regexp-opt exts) "$")))
  (set-variable 'openwith-associations
        `((,regexp
           "open" (file)) ("/$" "open" (file))))
  ;; pdf は別扱い。下のコメントで除去できる。
  (pushnew '("\\.pdf\\'" "open" (file))
           openwith-associations)
  ;; (callf2 remove-if (lambda (x) (equal (car x) "\\.pdf\\'")) openwith-associations)
  (set-variable 'bmkp-default-handlers-for-file-types
        `((,regexp . "open"))))

(use-package bookmark+ :no-require t :ensure t
  :bind ("M-E" . bookmark-edit-annotation)
  :mode ("\\.bmk$" . emacs-lisp-mode)
  :init
  (when (not (eq system-type 'windows-nt)) ; windows NT では ACL関係でエラー
    (add-hook 'find-file-hook 'bmkp-light-this-buffer))
  (defalias 'list-bookmarks 'bookmark-bmenu-list)
  :config
  (with-eval-after-load 'bookmark+-bmu
    (set-variable 'bmkp-bmenu-state-file
          (locate-user-emacs-file ".emacs-bmk-bmenu-state.el")))
  (with-eval-after-load 'bookmark+-lit
    (set-variable 'bmkp-light-style-autonamed 'lfringe)
    (set-variable 'bmkp-auto-light-when-set 'any-bookmark)
    (set-variable 'bmkp-auto-light-when-jump 'any-bookmark)))

;; bookmark への機械的登録
;; 追加する場合は、既存のブックマーク画面で "S" で保存すること!
;; bmkp-propertize-bookmark-names-flag が t の場合は同じ名前で複数登録可能。
(defun tkw-bmkp-target-set (alist file &optional kind)
  "Register ALIST to bookmark FILE, assuming it is of KIND."
  (bmkp-switch-bookmark-file-create file t)
  (dolist (item alist)
    (if (equal kind 'url)
        (bmkp-url-target-set (cdr item) nil (car item))
      ;; kind=file
      (bmkp-file-target-set (cdr item) nil (car item)))))

;; (tkw-bmkp-target-set tkw-css-documents-alist
;;                      "~/.emacs.d/bookmarks/w3.bmk" 'url)
;; (tkw-bmkp-target-set tkw-css-sources-alist
;;                      "~/.emacs.d/bookmarks/w3.bmk" 'url)

;;;; boxquote (abstain)
;; 使用中止。→ rebox2 を利用

;;;; browse-kill-ring (abstain)
;; helm-kill-rings に移行。

;;;; browse-url-dwim (abstain)
;; Context-sensitive external browse URL

;;;; buffer-move
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;; bundler
;; Ruby Gem Bundler 操作ユーティリティ
;; * 関数一覧 (autoload)
;;   - bundle-open :: 指定bundleのソースコード・ディレクトリを開く
;;   - bundle-console :: 指定bundleを取り入れたrubyのプロセスを開く

;;;; cask
;; http://cask.readthedocs.org/en/latest/
;; * テスト時に便利なパッケージ
;;   - noflet … 一時的な関数の変更
;;   - ecukes/espuds … Cumcumber-Like testing
;;   - commander … 引数の処理
;;
;; % curl -fsSkL https://raw.github.com/cask/cask/master/go | python
;; で、~/.cask 以下に専用のツールをインストールする。
;; % mkdir <project>
;; % cd <project>
;; % cask init --dev
;; % cask
;; (Caskを編集)
;; % cask exec ecukes -h
;; % cask exec ecukes new
;; (<project>/feature/<project>.featureを編集)

;;;; cacoo
;; http://d.hatena.ne.jp/kiwanami/20100507/1273205079
(use-package cacoo :no-require t :ensure t
  :commands (toggle-cacoo-minor-mode)
  :bind ("M--" . toggle-cacoo-minor-mode))

;;;; calfw
;; doc: https://github.com/kiwanami/emacs-calfw

(use-package calfw :no-require t :ensure t
  :commands (tkw-calendar)
  :functions (cfw:org-create-source cfw:cal-create-source
              cfw:ical-create-source cfw:open-calendar-buffer)
  :config
  (require 'calfw-org)
  (require 'calfw-cal)
  (require 'calfw-ical)
  (defvar tkw-calfw-content-sources)
  (set-variable 'tkw-calfw-content-sources
                `(,(cfw:org-create-source "Green")  ; orgmode source
                  ,(cfw:cal-create-source "Orange") ; diary source
                  ,@(let ((secret (plist-get (nth 0 (auth-source-search :host "calendar.google.com"))
                                             :secret)))
                      (list ; `(..,@(...)) なので、listで囲まないとエラーになる。
                       (when (functionp secret)
                         (cfw:ical-create-source
                          "gcal"
                          (concat
                           "https://www.google.com/calendar/ical/kawabata.taichi%40gmail.com/"
                           (funcall secret) "/basic.ics") "IndianRed")))) ; google calendar ICS
                  ))
  (defun tkw-calendar ()
    (interactive)
      (cfw:open-calendar-buffer
       :view 'month
       :contents-sources
       tkw-calfw-content-sources))
  (with-eval-after-load 'calfw-ical
    (when (getenv "http_proxy")
      (unless (executable-find "wget") (error "You need `wget'!"))
      (set-variable 'cfw:ical-url-to-buffer-get 'cfw:ical-url-to-buffer-external))))

;;;; charmap
;; Unicode Map for Emacs.
;;   * M-x charmap to display a unicode block.
;;   * M-x charmap-all to display entire unicode blocks but it's slow.
;;   * C-f / C-b / C-n / C-p to navigate the characters.
;;   * RET will copy a character on current cursor to kill-ring.
(use-package charmap :no-require t :ensure t
  :commands (charmap charmap-all))

;;;; color-moccur
;; http://www.bookshelf.jp/soft/meadow_49.html#SEC669
;; replace.el list-matching-lines → occur
;;            → color-moccur
;; regexp という変数を定義している。（名前空間ルールに反して行儀が悪い）
;;
;; (lazyload (occur-by-moccur isearch-moccur list-matching-lines
;;            (defalias 'occur 'occur-by-moccur))
;;     ;; occur を moccur 版に置き換える。
;;     "color-moccur"
;;   ;(set-variable 'moccur-use-migemo t)
;;   ;(defun occur-outline ()
;;   ;  (interactive)
;;   ;  (cond ((eq major-mode 'emacs-lisp-mode) (occur "^;;;+ " nil))
;;   ;        ((eq major-mode 'sh-mode) (occur "^###+ " nil))
;;   ;        ((eq major-mode 'rd-mode) (occur "^=+ " nil))))
;;   ;; moccur-edit を使用する。（検索結果全てで編集が可能になる。）
;;   (when (locate-library "moccur-edit")
;;     (set-variable 'moccur-use-ee nil) ;; ee-autloadsは使用中止。
;;     (set-variable 'moccur-split-word t)
;;     (set-variable '*moccur-buffer-name-exclusion-list*
;;           '(".+TAGS.+" "*Completions*" "*Messages*")))
;;   ;; dmoccur (directory moccur)
;;   (set-variable 'dmoccur-recursive-search t) ; ディレクトリを再帰的に検索
;;   ;; 開いた大量のバッファを片付ける。
;;   (bind-key "C-c k" 'clean-dmoccur-buffers)
;;   ;;(set-variable 'dmoccur-use-list t)
;;   ;;(set-variable 'dmoccur-list
;;   ;;      '(
;;   ;;        ("dir" default-directory ("\\.el$") dir)
;;   ;;        ))
;;   ;;(bind-key "O" 'dired-do-moccur dired-mode-map)
;;   ;;(bind-key "O" 'Buffer-menu-moccur Buffer-menu-mode-map))
;;   )

;;;; commander
;; http://tuxicity.se/emacs/2013/06/11/command-line-parsing-in-emacs.html
;; ecukes 等で使用。

;;;; cygwin-mount
;; cygwin風のファイル名をWindows風に加えて使えるようにする。
;;(declare-function cygwin-mount-activate "cygwin-mount")
;;(when (equal system-type 'windows-nt)
;;  (lazyload (cygwin-mount-activate) "cygwin-mount"
;;    (set-variable 'cygwin-mount-cygwin-bin-directory
;;          (concat (getenv "CYGWIN_DIR") "\\bin")))
;;  (when (require 'cygwin-mount nil t)
;;    (cygwin-mount-activate)))

;;;; dired+
(use-package dired+ :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'dired
    (require 'dired+ nil t))
  :config
  ;; dired+のM-<小文字>定義キーの一部を、M-A-<小文字>へ移動する。
  (defun tkw-dired-reset-keys ()
    (bind-key "M-c" nil dired-mode-map)
    (bind-key "M-b" nil dired-mode-map)
    (bind-key "M-p" nil dired-mode-map)
    (bind-key "M-A-c" 'diredp-capitalize-this-file dired-mode-map)
    (bind-key "M-A-b" 'diredp-do-bookmark          dired-mode-map)
    (bind-key "M-A-p" 'diredp-print-this-file      dired-mode-map))
  (add-hook 'dired-mode-hook 'tkw-dired-reset-keys))

;;;; dired-details (abstain)
;; → dired+ に移行
;; dired で余計な mode 情報などを不可視にする.
;; "(", ")" で切り替え
;; (lazyload () "dired"
;;   (when (require 'dired-details nil :no-error)))

;;;; dired-details+ (abstain)
;; → dired+ に移行
;;(lazyload () "dired"
;;  (when (require 'dired-details+ nil :no-error)))

;;;; dired-k
;; highlight dired buffer by file size, modified time, git status
;; https://github.com/syohex/emacs-dired-k
(use-package dired-k :no-require t :defer t :ensure t
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  :config
  (define-key dired-mode-map (kbd "K") 'dired-k))

;;;; direx
;; Explorerライクなファイルブラウザ
(use-package direx :no-require t :ensure t
  :bind ("C-x A-d" . direx:jump-to-directory))

;;;; doremi
(use-package doremi :no-require t :defer t :ensure t
  :config
  (set-variable 'doremi-up-keys '(?p up))
  (set-variable 'doremi-down-keys '(?n down))
  (set-variable 'doremi-boost-up-keys '(?P M-up))
  (set-variable 'doremi-boost-down-keys '(?N M-down)))

;;;; doremi-cmd
(use-package doremi-cmd :no-require t :ensure t
  :commands (doremi-custom-themes+)
  :init
  (bind-key "b" 'doremi-buffers+        tkw-rotate-map)
  (bind-key "g" 'doremi-global-marks+   tkw-rotate-map)
  (bind-key "m" 'doremi-marks+          tkw-rotate-map)
  (bind-key "t" 'doremi-custom-themes+  tkw-rotate-map)
  (bind-key "r" 'doremi-bookmarks+      tkw-rotate-map) ; reading books?
  (bind-key "w" 'doremi-window-height+  tkw-rotate-map)
  :config
  (set-variable 'doremi-themes-update-flag t)
  (add-hook 'doremi-custom-theme-hook 'tkw-reset-fontset)
  (set-variable 'doremi-custom-themes
                (cons nil (cl-set-difference
                           (sort (custom-available-themes)
                                 (lambda (x y) (string< (symbol-name x) (symbol-name y) )))
                           ;; 不要なテーマ一覧
                           '()))))

;;;; doremi-frm
(declare-function rotate-fonts "rotate-fonts")
(use-package doremi-frm :no-require t :defer t :ensure t
  :init
  (bind-key "a" 'doremi-all-faces-fg+ tkw-rotate-map)    ; "All"
  (bind-key "c" 'doremi-bg+ tkw-rotate-map)              ; "Color"
  ;;(bind-key "f" 'doremi-face-fg+ tkw-rotate-map)         ; Face"
  ;;(bind-key "h" 'doremi-frame-height+ tkw-rotate-map)
  ;;(bind-key "t" 'doremi-font+ tkw-rotate-map)            ; "Typeface"
  (bind-key "u" 'doremi-frame-configs+ tkw-rotate-map)   ; "Undo"
  (bind-key "x" 'doremi-frame-horizontally+ tkw-rotate-map)
  (bind-key "y" 'doremi-frame-vertically+ tkw-rotate-map)
  (bind-key "z" 'doremi-font-size+ tkw-rotate-map)       ; "Zoom"
  (bind-key "w" 'doremi-window-height+ tkw-rotate-map)
  (bind-key "s" (command (doremi-font-size+ 2)) tkw-rotate-map)       ; "Zoom"
  (bind-key "S" (command (doremi-font-size+ 1)) tkw-rotate-map)       ; "Zoom"
  (bind-key "l" (command (rotate-fonts ?l)) tkw-rotate-map)
  (bind-key "k" (command (rotate-fonts ?k)) tkw-rotate-map)
  (bind-key "h" (command (rotate-fonts ?h)) tkw-rotate-map)
  (bind-key "p" (command (rotate-fonts ?p)) tkw-rotate-map)
  )

;;;; dsvn
;; Subversion インタフェース
;; 注意：MacでSubversion を使う場合は、ver. 18ではなく ver. 17 を使わないと
;; Unicode ファイル名が正しくハンドルされない。(2014/1 現在)
;; $ brew uninstall subversion
;; $ brew tap homebrew/versions
;; $ brew install subversion17 --unicode-path
(use-package dsvn :no-require t :ensure t
  :commands (svn-status svn-update))

;;;; durendal (abstain)
;; slime-clj が必要。
;; A bucket of tricks for Clojure and Slime.
;;(when (require 'durendal nil :no-error)
;;  (durendal-enable))

;;;; e2wm
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
;; simple window manager for emacs
;; | C-c ; | action                 |
;; |-------+------------------------|
;; | Q     | 中止                   |
;; | M     | メインウィンドウ最大化 |
;; | C     | 時計切り替え           |
;; | d     | 同一バッファを２画面   |
;; | 1     | e2wm:dp-code           |
;; | 2     | e2wm:dp-two            |
;; | 3     | e2wm:dp-doc            |
;; | 4     | e2wm:dp-array          |
;; | 5     | e2wm:dp-dashboard      |
;; | p     | history-up             |
;; | n     | history-down           |
;; | l     | history-update         |
;; |-------+------------------------|
;; |       |                        |
(use-package e2wm :no-require t :ensure t
  :bind ("M-+" . e2wm:start-management))

;;;; e2wm-svg-clock (abstain)
;; svg-clock plug-in for svg.

;;;; ebib
;; 設定は ~/.emacs.d/ebibrc.el に書く。
;; （browse-reftex で十分か？）
;; [[info:ebib]]
(use-package ebib :no-require t :defer t :ensure t
  :config
  (set-variable 'ebib-rc-file                 "~/.emacs.d/ebibrc.el")
  (set-variable 'ebib-preload-bib-search-dirs tkw-bibtex-directories)
  (set-variable 'ebib-preload-bib-files       tkw-bibtex-files))

;;;; eimp
;; Emacs Image Manipulation Utility
;; 画像を小さくしたい場合は、"-" を押して、適当に小さくした後、
;; C-x C-s で保存する。
(use-package eimp :no-require t :defer t :ensure t
  :init
  (add-hook 'image-mode-hook 'eimp-mode))

;;;; ein
;; IPython Notebook interface for Emacs.
;; - document :: http://tkf.github.io/emacs-ipython-notebook/#quick-try
;; - autload :: ein:connect-to-notebook, etc.
;; % ipython notebook ← start at http://127.0.0.1:8888/
(use-package ein :no-require t :defer t :ensure t)

;;;; ejacs
(use-package js-console :no-require t
  :commands js-console
  :config
  ;; compatibility with old emacs (warning will appear)
  (with-no-warnings
    (defvar e float-e "The value of e (2.7182818...).")))

;;;; elfeed
;; 高速RSSブラウザ。便利なので、W3CのML閲覧に利用してみる。
;; ~/.elfeed に DBがある。必要ならこれをリセット。
;; elfeed-db.el は AVL木を使ってランダムに挿入されるエントリの時間順ソートを効率化している。
(use-package elfeed :no-require t :defer t :ensure t
  :config
  ;; elfeed-feeds を設定する。
  (set-variable 'elfeed-feeds
                '(
                  "http://www.w3.org/blog/International/feed/rdf/"
                  "http://lists.w3.org/Archives/Public/www-international/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-css-bugzilla/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-css-commits/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-css-testsuite/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-cssacc/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-html-comments/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-i18n-cjk/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-i18n-core/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-i18n-its-ig/feed.rss"
                  "http://lists.w3.org/Archives/Public/public-lod/feed.rss"
                  "http://lists.w3.org/Archives/Public/www-style/feed.rss"
                  )))

;;;; elscreen
;;(setq elscreen-prefix-key "\C-c\C-c") ; Old copy-to-register
;;(require 'elscreen nil :no-error)

;;;; ert-runner
;; http://tuxicity.se/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html
;; - Cask に以下のファイルを書く。
;; | (source "melpa")
;; | (development
;; |  (depends-on "ert-runner"))
;; -実行
;; % cask install
;; % cask exec ert-runner init
;; test/ids-test.el にテストを書く。
;; % cask exec ert-runner
(use-package ert-runner :no-require t :defer t :ensure t)

;;;; esh-buf-stack
(use-package esh-buf-stack :no-require t :defer t :ensure t
  :defines (eshell-mode-map)
  :config
  (setup-eshell-buf-stack)
  (bind-key "M-q" 'eshell-push-command eshell-mode-map))

;;;; esh-help
;; M-x eldoc-mode
(use-package esh-help :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'eshell
    (setup-esh-help-eldoc)))

;;;; esup
;; Emacs Startup Profiler
;; M-x esup だけで全てが実施される。

;;;; esxml
;; XML/XHTML の動的生成

;;;; expand-region
(use-package expand-region :no-require t :ensure t
  :bind ("M-R" . er/expand-region))

;;;; findr
;; 巾優先探索によるファイル検索
(use-package findr :no-require t :ensure t
  :commands (findr findr-search findr-query-replace))

;;;; flx-ido
;; more powerful alternative to `ido-mode''s built-in flex matching.
(use-package flx-ido :no-require t :defer t :ensure t
  :config
  (flx-ido-mode 1))

;;;; frame-cmds
(use-package frame-cmds :no-require t :ensure t
  :commands (frame-to-right)
  :bind (("C-c x" . maximize-frame-vertically)
         ("C-c X" . maximize-frame))
  ;; 下記の機能は、OSに任せる。
  ;; - Windows :: Windowsキー＋カーソル
  ;; - MacOS :: Spectacle をインストールして利用。
  ;; - Ubuntu :: xdotool ツールで、windowmove を設定。
  ;;:config
  ;;(defun frame-to-right ()
  ;;  "現在のフレームの大きさを全画面の半分にして左に配置する。"
  ;;  (interactive)
  ;;  (maximize-frame)
  ;;  (set-frame-parameter nil 'width
  ;;                       (/ (frame-parameter nil 'width) 2))
  ;;  (call-interactively
  ;;   'move-frame-to-screen-left))
  )

;;;; free-keys
;; 空いているキーを検索する。
(use-package free-keys :no-require t :defer t :ensure t
  :config
  ;; C-c C-x M-g M-s M-r
  (set-variable 'free-keys-modifiers
                '("" "C" "M" "C-M" "C-S"
                  "A" "A-C" "A-M" "A-C-M")))

;;;; geeknote (abstain)
;; 【注意！】 geeknote では Proxy 配下では利用不可！
;; cf. https://github.com/VitaliyRodnenko/geeknote/issues/75
;; geeknote 側の設定
;; % git clone git://github.com/VitaliyRodnenko/geeknote.git
;; % cd geeknote
;; % sudo python setup.py install
;; 利用にあたっては、スマートフォンに、"Google Authenticator" をインストールして、
;; evernote -> security で、two-factor authentication を enable にする必要がある。
(use-package geeknote :no-require t :ensure t :defer t
  :if
  (and (executable-find "geeknote")
       (null (getenv "HTTP_PROXY"))))

;;;; genrnc
;; trang.jar を使った RNC ファイルの自動生成
(use-package genrnc :no-require t :ensure t
  :commands (genrnc-regist-url genrnc-regist-file genrnc-update-user-schema))

;;;; google-maps
;; ,/. で地図を拡大・縮小。
;; m (mark), h (home), c (center), C (center remove)
(use-package google-maps :no-require t :defer t :ensure t)

;;;; google-this

;; | C-c r    |                             |
;; |----------+-----------------------------|
;; | [return] | google-search               |
;; |          | google-region               |
;; | t        | google-this                 |
;; | g        | google-lucky-search         |
;; | i        | google-lucky-and-insert-url |
;; | w        | google-word                 |
;; | s        | google-symbol               |
;; | l        | google-line                 |
;; | e        | google-error                |
;; | f        | google-forecast             |
;; | r        | google-cpp-reference        |
;; | m        | google-maps                 |

;; M-x google-this-mode で起動
;; (when (require 'google-this nil :no-error)
;;   (google-this-mode 1)) ; マイナーモードの起動

;;;; google-translate
(use-package google-translate :no-require t :ensure t
  :bind (("C-c g" . google-translate-at-point)
         ("C-c G" . google-translate-query-translate))
  :config
  (set-variable 'google-translate-default-target-language "French")
  (set-variable 'google-translate-default-source-language "English"))

;;;; goto-chg
(use-package goto-chg :no-require t :ensure t
  :bind (("C-c ." . goto-last-change)
         ;; howm と衝突するので下記のキーバインドは禁止。
         ;;("C-c ," . goto-last-change-reverse)
         ))

;;;; haskell-emacs
;; % cabal install attoparsec atto-lisp
;; M-x haskell-emacs-init
(use-package haskell-emacs :no-require t :defer t :ensure t
  :config
  ;; ディレクトリ指定の末尾に"/"がないとエラーになるので注意。
  ;; ディレクトリ名を haskell-emacs にすると、find-file のファイル名補完で
  ;; ライブラリ名とかぶるので、デフォルトの `haskell-fun' に揃える。
  (set-variable 'haskell-emacs-dir
                (locate-user-emacs-file "haskell-fun/")))

;;;; helm
;;;;; * 解説等
;;   - https://github.com/emacs-helm/helm/wiki
;;   - http://d.hatena.ne.jp/syohex/20121207/1354885367
;;   - http://qiita.com/items/d9e686d2f2a092321e34
;; * helm のコマンド一覧
;; | M-X       | (helm-command-prefix)           | target                            | action |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | C-x C-f   | helm-find-files                 | file                              |        |
;; | f         | helm-for-files                  | file                              |        |
;; | C-c f     | helm-recentf                    | file                              |        |
;; | l         | helm-locate                     | file                              |        |
;; | /         | helm-find                       | file                              |        |
;; | C-x r b   | helm-bookmarks                  | file                              |        |
;; | B (user)  |                                 |                                   |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | C-x C-b   | helm-buffers-list               | buffers                           |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | M-x       | helm-M-x                        | command                           |        |
;; | C-c C-x   | helm-run-external-command       | command                           |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; |           | helm-org-keywords               | org file                          |        |
;; |           | helm-org-headlines              | org file                          |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | a         | helm-apropos                    | elisp function                    |        |
;; | <tab>     | helm-lisp-completion-at-point   | elisp function                    |        |
;; | C-:       | helm-eval-expression-with-eldoc | elisp evaluation                  |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | M-y       | helm-show-kill-ring             | kill-ring                         |        |
;; | C-c <SPC> | helm-all-mark-rings             | mark-ring                         |        |
;; | C-x r i   | helm-register                   | register                          |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | i         | helm-imenu                      | funcs & vars                      |        |
;; |           | helm-semantic-or-imenu          | funcs & vars                      |        |
;; | C-c C-b   | helm-browse-code                | funcs & vars                      |        |
;; | e         | helm-etags-select               | functs & vars (etags)             |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | M-s o     | helm-occur                      | search                            |        |
;; | M-g s     | helm-do-grep                    | search                            |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | w         | helm-w3m-bookmarks              | w3m bookmarks                     |        |
;; | x         | helm-firefox-bookmarks          | ~/.mozilla/firefox/bookmarks.html |        |
;; | #         | helm-emms                       | Emacs Mutli-Media System          |        |
;; | m         | helm-man-woman                  | man pages                         |        |
;; | t         | helm-top                        | process                           |        |
;; | p         | helm-list-emacs-process         | process                           |        |
;; | C-,       | helm-calcul-expression          |                                   |        |
;; | c         | helm-colors                     | color                             |        |
;; | F         | helm-select-xfont               | font                              |        |
;; | 8         | helm-ucs                        | character                         |        |
;; | s         | helm-surfraw                    | web search                        |        |
;; | C-c g     | helm-google-suggest             | web search                        |        |
;; | h i       | helm-info-at-point              | info                              |        |
;; | h r       | helm-info-emacs                 | info (emacs)                      |        |
;; | h g       | helm-info-gnus                  | info (gnus)                       |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | b         | helm-resume                     | Previous Helm                     |        |
;; | r         | helm-regexp                     |                                   |        |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | G         | helm-ag                         |                                   |        |
;; |           | helm-flymake                    |                                   |        |
;; |           | helm-go-package                 |                                   |        |
;; |           | helm-moccur                     |                                   |        |
;; |           | helm-descrinds                  |                                   |        |
;; |           | helm-emmet                      |                                   |        |
;; | g         | helm-git-find-files             |                                   |        |
;; |           | helm-git-grep                   |                                   |        |
;; |           | helm-gist                       |                                   |        |
;; | M-t       | helm-gtags                      |                                   |        |
;; | ----      | helm-descbinds                  |                                   |        |
;; |           | helm-orgcard                    |                                   |        |
;; |           | helm-rails                      |                                   |        |
;; |           | helm-sheet                      |                                   |        |
;; |           | helm-spaces                     |                                   |        |
;; | T         | helm-themes                     |                                   |        |

;;;;; helm/helm
;; * helm のkey binding の問題
;;   helm は、 (where-is-internal 'describe-mode global-map) で、
;;   describe-mode の全キーバインディングを取って、helm-key にマップす
;;   る。しかし、global-map で定義された prefix-key 付きのキーとhelmで
;;   定義済のキーが衝突すると、エラーになる。回避策として、global-map
;;   から一時的にhelp-map を除去し、読み込み後復活する。
;; → この方法は、helm が別の機会で load されると通用しないので、
;;   helpキーのリバインドを最後に行う。
;; → 単にtranslate-key を使えばすむ話なのでこの対策は不要に。
;; (when-interactive-and t
;;   ;; helmキーのリバインドを最後に行う。
;;   ;;(global-set-key (kbd "C-z") nil) ; 退避
;;   (require 'helm nil :no-error)
;;   (require 'helm-config nil :no-error)
;;   ;;(global-set-key (kbd "C-z") help-map) ; 復旧
;;   )
;; * helm で quail を使うには
;;   helm は、override-keymaps を設定するがこれが設定されていると
;;   quail はキーイベントを素通ししてしまうので使えない。以下で修正。

;; === modified file 'lisp/international/quail.el'
;; --- lisp/international/quail.el 2012-08-15 16:29:11 +0000
;; +++ lisp/international/quail.el 2013-01-21 13:17:01 +0000
;; @@ -1330,8 +1330,10 @@

;;  (defun quail-input-method (key)
;;    (if (or buffer-read-only
;; -         overriding-terminal-local-map
;; -         overriding-local-map)
;; +         (and overriding-terminal-local-map
;; +              (lookup-key overriding-terminal-local-map (vector key) t))
;; +         (and overriding-local-map
;; +              (lookup-key overriding-local-map (vector key) t)))
;;        (list key)
;;      (quail-setup-overlays (quail-conversion-keymap))
;;      (let ((modified-p (buffer-modified-p))

(use-package helm :no-require t :defer t :ensure t
  :defines (helm-map))

;;;;; helm/helm-bibtex
(use-package helm-bibtex :no-require t :defer t :ensure t
  :config
  (set-variable 'helm-bibtex-bibliography nil) ; list of bibtex files.
  )

;;;;; helm/helm-config
(defvar helm-command-prefix) ;; compile-error 避け
(use-package helm-config :no-require t :defer t :ensure helm
  :defines (helm-command-map)
  :init
  ;; helm-command-prefix は、helm-config 中で fset してある。
  (autoload 'helm-command-prefix "helm-config" nil t 'keymap)
  (bind-key "M-X" 'helm-command-prefix) ; <hcp>=helm-command-prefix
  (bind-key "C-M-;" 'helm-recentf)
  (bind-key* "C-;" 'helm-for-files) ; <hcp> f
  (bind-key "C-M-y" 'helm-show-kill-ring) ; <hcp> M-y
  :config
  (require 'helm)
  ;; Infoファイルへジャンプ
  (bind-key "B"   'helm-bookmarks helm-command-prefix)
  (bind-key "h e" 'helm-info-elisp helm-command-prefix)
  (bind-key "h c" 'helm-info-cl helm-command-prefix)
  (bind-key "h o" 'helm-info-org helm-command-prefix))

;;;;; helm/helm-files
(use-package helm-files :no-require t :defer t
  :config
  (set-variable 'helm-for-files-preferred-list
                `(helm-source-buffers-list
                  helm-source-recentf
                  helm-source-bookmarks
                  helm-source-file-cache
                  helm-source-files-in-current-dir
                  ;; Mac は spotlight を使って、Linux は locate を使う。
                  ,(if (eq system-type 'darwin)
                       'helm-source-mac-spotlight
                     'helm-source-locate))))

;;;;; helm/helm-net
(use-package helm-net :no-require t :defer t
  :config
  (set-variable 'helm-google-suggest-use-curl-p (executable-find "curl"))
  (set-variable 'helm-google-suggest-search-url
        "http://www.google.co.jp/search?hl=ja&num=100&as_qdr=y5&lr=lang_ja&ie=utf-8&oe=utf-8&q=")
  (set-variable 'helm-google-suggest-url
                "http://google.co.jp/complete/search?ie=utf-8&oe=utf-8&hl=ja&output=toolbar&q="))

;;;;; helm/helm-sys
(use-package helm-sys :no-require t :defer t
  :if (eq system-type 'darwin)
  :config
  (set-variable 'helm-top-command "env COLUMNS=%s top -l 1"))

;;;; helm-ag
(use-package helm-ag :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "G" 'helm-ag helm-command-map)))

;;;; helm-aws
(use-package helm-aws :no-require t :defer t :ensure t)

;;;; helm-c-moccur
;; (lazyload (helm-c-moccur-occur-by-moccur) "helm-c-moccur"))
;; (when (functionp 'helm-c-moccur-occur-by-moccur)
;;   (lazyload () "helm-config"
;;     (bind-key "o" 'helm-c-moccur-occur-by-moccur helm-command-map)))

;;;; helm-c-yasnippet
(use-package helm-c-yasnippet :no-require t :ensure t
  :commands (helm-c-yas-complete)
  :init
  (with-eval-after-load 'helm-config
    (bind-key "y" 'helm-c-yas-complete helm-command-map)))

;;;; helm-descbinds
;; describe-bindings をhelmで行なう。
;; helm-descbinds-mode で、describe-bindigins 命令を置換する。
(use-package helm-descbinds :no-require t :ensure t
  :commands (helm-descbinds helm-descbinds-mode)
  :init
  (with-eval-after-load 'helm-config
    (bind-key "y" 'helm-descbinds helm-command-map))
  :config
  (helm-descbinds-mode))

;;;; helm-firefox
(use-package helm-firefox :no-require t :defer t :ensure t
  :config
  ;; Chrome の Bookmark を exportして、
  ;; ~/.emacs.d/.mozilla/firefox/r5jqkmvp.default/bookmarks.html に入れておく。
  (set-variable 'helm-firefox-default-directory "/.emacs.d/.mozilla/firefox/")
  (require 'helm-net))

;;;; helm-git
(use-package helm-git :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "g" 'helm-git-find-files helm-command-map)))

;;;; helm-github-stars
(use-package helm-github-stars :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "g" 'helm-github-stars helm-command-map))
  :config
  (set-variable 'helm-github-stars-username "kawabata"))

;;;; helm-go-package
(use-package helm-go-package :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'go-mode
    (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

;;;; helm-gtags
(use-package helm-gtags :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "M-t" 'helm-gtags-select helm-map)))

;;;; helm-open-github
(use-package helm-open-github :no-require t :defer t :ensure t)

;;;; helm-projectile
(use-package helm-projectile :no-require t :ensure t
  :bind ("C-c h" . helm-projectile))

;;;; helm-themes
(use-package helm-themes :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "T" 'helm-themes helm-command-map)))

;;;; help+
;; where-is と describe-key を置換する。

;;;; hide-lines
;; M-x hide-lines REGEXP ... hide lines that matches REGEXP.
;; C-u M-x hide-lines REGEXP ... hide lines that doesn't match REGEXP.

;;;; hfyview (obsolete)
;; htmlize へ移行。
;; TODO 印刷に phantomjs を使えないだろうか？
;; hfyview = htmlfontify-view.
;; htmlfontify を使って、バッファをブラウザで開く。（印刷向け）
;; commands `hfyview-buffer', `hfyview-region', `hfyview-frame'
;;(use-package hfyview
;;  :commands (hfyview-buffer hfyview-region hfyview-frame)
;;  :config
;;  (set-variable 'hfyview-quick-print-in-files-menu t))

;;;; howm
;; haskell-mode との衝突に注意。haskell-mode のあとで、howmを読み込むこと。
(use-package howm :no-require t
  :commands (howm-list-all tkw-howm-concatenate-all-isearch)
  :functions (howm-view-summary-to-contents)
  :bind (("M-H" . howm-list-all))
  ;; :bind で設定すると、org-mode が howm と自動的に結び付けられてしまう。
  :init
  (pushnew '("\\.howm$" . org-mode) auto-mode-alist :test 'equal)
  :config
  ;; TITLEの変更（org-modeにも対応）
  (set-variable 'howm-directory "~/share/howm/")
  (set-variable 'howm-view-title-header "#+TITLE:") ;; ← howm のロードより前に書くこと
  (set-variable 'howm-view-title-regexp "^\\(\\(#\\+TITLE:\\)\\|=\\)\\( +\\(.*\\)\\|\\)$")
  (set-variable 'howm-view-title-regexp-pos 4)
  (set-variable 'howm-view-title-regexp-grep "^\\(\\(#\\+TITLE:\\)\\|=\\) ")
  (set-variable 'howm-list-title t)
        ;; 時間のかかるhowmのscanningは行わない。
  (set-variable 'howm-menu-allow nil)
  (set-variable 'howm-menu-lang 'ja)
  (set-variable 'howm-list-all-title t)
  (set-variable 'howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
  ;; 一気に全てを繋げての表示を行う。
  (defun tkw-howm-concatenate-all-isearch ()
    (interactive)
    (howm-list-all)
    (howm-view-summary-to-contents)
    (isearch-forward)))

;;;; html-script-src
;; jquery などのライブラリの最新版を素のHTMLソースに挿入する。

;;;; htmlize
;; htmlfontify があるがこちらが便利。
(use-package htmlize :no-require t :defer t :ensure t
  :config
  ;;(set-variable 'htmlize-output-type 'font)
  ;;(set-variable 'htmlize-convert-nonascii-to-entities nil)
  ;;(set-variable 'htmlize-html-charset 'utf-8)
  )

;;;; hydra (abstain)
;; doremi を利用する

;;;; icicles
;; ミニバッファ補完ユーティリティ
;; - 参照 :: http://www.emacswiki.org/emacs/Icicles
;; * Completion
;;   - TAB (icicle-prefix-complete) :: 前置補完
;;     + C-( :: FUZZY/BASIC/VANILLA を切り替え
;;   - S-TAB (icicle-apropos-complete) :: 部分補完
;;     +  M-( :: JARO-WRINKLER/SCATTER/APRPOS を切り替え
;;   - C-# :: icicle-incremental-completion を切り替え
;;     + nil :: *Completions* を自動更新しない
;;     + t :: *Completion* がある場合のみ自動更新する。
;;     + always :: つねに *Completions* を自動更新する。
;; * 補完
;; - S-space (icicle-apropos-complete-and-narrow) :: apropos補完＆ナロー
;; - M-R (icicle-multi-inputs-act) :: multi-input （複数をスペースで区切って一括入力）
;; - C-M-j (icicle-insert-list-join-string) ::
;; * 拡張コマンド
;; - Info :: g (icicle-Info-goto-node)
;;  - Emacs を使う場合
;; * TODO
;;   - ヒストリ補完補完の方法の調査
;;(lazyload () "icicles-mode"
;;  (add-hook
;;   'icicle-mode-hook
;;   (lambda ()
;;     (define-key icicle-mode-map "M-O" 'icicle-insert-history-element))))
;;(when (functionp 'icy-mode)
;;  (icy-mode))
;;(use-package icicles-opt :defer t
;;  :config
;;  (setf (alist-get (kbd "M-r") icicle-minibuffer-key-bindings)
;;        (list 'icicle-other-history t)))

;;;; icomplete+
;; icomplete を拡張して、色付け・マッチ数の表示など
(use-package icomplete+ :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'icomplete
    (require 'icomplete+)))

;;;; ido-ubiquitous
;; ido-mode を拡張する。
(use-package ido-ubiquitous :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-ubiquitous-mode)))

;;;; image+
;; 画像の拡大・縮小（要ImageMagick）
(use-package image+ :no-require t :defer t :ensure t)

;;;; interaction-log
;; *Emacs Log* バッファにEmacsとのインタラクションのログ情報を書き込む。
(use-package interaction-log :no-require t :ensure t
  :commands interaction-log-mode)

;;;; info+
(use-package info+ :no-require t :defer t :ensure t
  :config
  (with-eval-after-load 'info
    (require 'info+ nil :no-error)))

;;;; ipa <emacswiki>
;; In-place annotations
(use-package ipa :no-require t :defer t)

;;;; itail
;; 指定されたファイルをフィルタして tail で追跡する。
;; itail-toggle-filter でフィルタ文字列を指定する。
(use-package itail :no-require t :ensure t
  :commands (itail))

;;;; itunes-bgm
;; https://github.com/syohex/emacs-itunes-bgm
;; M-x itunes-bgm
(use-package itunes-bgm :no-require t
  :if (executable-find "mplayer")
  :commands (itunes-bgm)
  )

;;;; japanlaw
;; 日本の法律の閲覧・検索エンジン
(use-package japanlaw :no-require t :ensure t
  :commands japanlaw)

;;;; jd-el
;; http://julien.danjou.info/google-maps-el.html
;;(when (locate-library "google-maps")
;;  (autoload 'google-maps "google-maps" "" t))

;;;; keisen-mule
;; 罫線描画モード。Emacs 22以降は利用不可能。
;;(when nil ; (locate-library "keisen-mule")
;;  (if window-system
;;      (autoload 'keisen-mode "keisen-mouse" "MULE 版罫線モード + マウス" t)
;;    (autoload 'keisen-mode "keisen-mule" "MULE 版罫線モード" t)))

;;;; kill-summary (abstain)
;; Emacs 22 からは、truncate-string を、truncate-string-to-width に変更。
;;(when (locate-library "kill-summary")
;;  (if (functionp 'truncate-string-to-width)
;;      (defalias 'truncate-string 'truncate-string-to-width))
;;  (autoload 'kill-summary "kill-summary" nil t))

;;;; kite-mode
;; Linux :: chromium-browser --remote-debugging-port=9222
;; Mac :: open /Applications/Google\ Chrome.app --args

;;;; kurecolor
;; color editor for emacs.
(use-package kurecolor :no-require t :defer t :ensure t)

;;;; lib-requires
;; packageの依存関係を調査する。

;;;; list-packages-ext
;; https://github.com/laynor/list-packages-ext/blob/master/list-packages-ext.el
;; Emacs 24.4 (2013-06-12) 以降の list-packages 1.01 が必要。
(use-package list-packages-ext :no-require t :defer t :ensure t
  :config
  ;; to avoid error
  (defvar list-packages-ext-mode-hook nil)
  (add-hook 'package-menu-mode-hook (lambda () (list-packages-ext-mode 1))))

;;;; list-register
;; レジスタを見やすく一覧表示。
;;(when (locate-library "list-register")
;;  (autoload 'tkw-jump-to-register "list-register" "list-register." t)
;;  (autoload 'list-register "list-register" "list-register." t)
;;  (bind-key "C-c C-r" 'data-to-resgister)
;;  (bind-key "C-x r j" 'tkw-jump-to-register)
;;  (bind-key "C-c i" 'list-register)
;;  )

;;;; lookup
;; Emacs終了時にLookupでエラーが出る場合は、
;;   (remove-hook 'kill-emacs-hook 'lookup-exit)
;; を実行する。
;; 要再設計。
;; ndtext(grep one line), nddsl(grep), ndeb(eblook), ndict(dict), ndstar(sdcv) がサポートされていれば十分か。
;; support ファイルは、それが存在しなくても動作することを必須とすること。
;; それ以外の辞書は原則として、xdxf_makedict
(use-package lookup :no-require t
  :bind (("C-c M-/" . lookup-pattern)
         ("A-?" . lookup-pattern)
         ("C-c M-;" . lookup-word)
         ("M-\"" . lookup-select-dictionaries)
         ("M-'" . lookup-list-modules )
         ("C-c M-\"" . lookup-restart))
  ;; emacsclient org-protocol:/lookup:/testimony
  ;; javascript:location.href='org-protocol://lookup://'+encodeURIComponent(window.getSelection())
  :defines org-protocol-protocol-alist
  :config
  (with-eval-after-load 'ndtext
    (when (executable-find "ggrep")
      (setenv "GREP" "ggrep")))
  (with-eval-after-load 'org-protocol
    (pushnew '("Lookup"
               :protocol "lookup"
               :function lookup-word)
             org-protocol-protocol-alist)))

;;;; mac-print-mode.el (obsolete)
;; → hfyview.el へ移行。

;;;; magic-buffer
;; Emacs の持つバッファの様々な機能のデモ
;; (defun magic-buffer ()
;;   (interactive)
;;   (let ((try-downloading
;;          (lambda ()
;;            (let ((lexical-binding t))
;;              (with-current-buffer
;;                  (url-retrieve-synchronously
;;                   "https://raw.github.com/sabof/magic-buffer/master/magic-buffer.el")
;;                (goto-char (point-min))
;;                (search-forward "\n\n")
;;                (delete-region (point-min) (point))
;;                (set-variable 'lexical-binding t)
;;                (eval-buffer))))))
;;     (condition-case nil
;;         (funcall try-downloading)
;;       (error (funcall try-downloading))))
;;   (magic-buffer))

;;;; magit
;;;;; Unicodeファイル名問題
;; % git config --global core.precomposeunicode true
;;;;; 1.4.0 の仕様変更
;; https://raw.githubusercontent.com/magit/magit/next/Documentation/RelNotes/1.4.0.txt
;;;;; ドキュメント ([[info:magit#Top]] 参照)
;; http://matome.naver.jp/odai/2136491451473222801 が一番良いまとめ
;; - ワークツリー <-(checkout) ステージングエリア <-(reset) Gitレポジトリ
;; - HEAD :: Gitのレポジトリが見ている最新のcommit位置。
;; - original :: githubのリモートの典型的なレポジトリ名
;; - master :: デフォルトのブランチ名
;; M-x magit-status (.git がなければ git init をすることが可能)
;; C-u で、ファイル名入力などが可能。
;; | コマンド | gitコマンド・その他             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | -status  | git init, git status            |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | 1,2,3,4  | 表示レベル                      |                                                                               |
;; | M-1,2,.. | 全ファイルで対象                |                                                                               |
;; | M-H      | 全て隠す                        |                                                                               |
;; | TAB      | ファイルのdiff表示              |                                                                               |
;; | S-TAB    | diffレベルの切り替え            |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | s        | git add <file>                  | ステージング                                                                  |
;; | C-u S    | git add .                       | 全ステージの登録                                                              |
;; | u        | git -- reset <file>             | アンステージング                                                              |
;; | i        | .gitignore へ追加（無視）       |                                                                               |
;; | C-- i    | .gitignore へワイルドカード追加 |                                                                               |
;; | I        | .git/info/exclude へ追加        |                                                                               |
;; | k        | rm / git rm <file>              |                                                                               |
;; |          | git mv <file>                   |                                                                               |
;; | c        | git commit <file>               |                                                                               |
;; | C        | git commit <file> / changelog   |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | l        | git log                         | l (short) L (Long) f (File log) rl (ranged short)  rL (long)                  |
;; |          | Reflogs                         | h (Head Reflog)                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | v        | Show Commit                     |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | d        | git diff <rev>                  | git diff HEAD                                                                 |
;; | D        | git diff ???                    | s (set) d (set default) c (save default) r (reset to default) h (toggle hunk) |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | t        | Tagging                         | t (lightweight)  a (annotation)                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | X        | git reset --hard HEAD <file>    | ステージ前の状態に戻す。                                                      |
;; | x        | git reset --soft HEAD <file>    |                                                                               |
;; |          | git revert <commit>             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | z        | Stashing                        | z (save) s (snapshot)                                                         |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | P P      | git push <remote> <refspec>     | git push origin master                                                        |
;; | P t/T    | git push <remote> tag(s)        |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | f f      | git fetch <remote> <refspec>    | f (current) a (all) o (other)                                                 |
;; | F F      | git pull <remote> <refspec>     | git pull origin master -r (--rebase)                                          |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | b v      | git branch -a                   |                                                                               |
;; | b c      | git branch <branch>             | ブランチを作成                                                                |
;; | b b      | git checkout <branch>           | ブランチを切り替え。(HEADを<branch>へ移動)                                    |
;; |          | git checkout -b <new> <old>     | ブランチの作成＋切り替え                                                      |
;; | b r      |                                 | ブランチ名を変更                                                              |
;; | b k      |                                 | ブランチの削除                                                                |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | m m      | git merge <branch>              | -ff (fast-forward) -nf (non fast-forward)                                     |
;; |          | git rebase <branch>             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | M a      | git remote add <name> <url>     |                                                                               |
;; | y        | git cherry-pick                 | 狙ったコミットの変更内容だけを現在のブランチに取り込む操作                    |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | B        | Bisecting                       | b (bad) g (good) k (skip) r (reset) s (start) r (reset) u (run)               |
;; | c        | Committing                      | c (commit) a (amend) e (extend) r (reword) f (fixup) s (squash)               |
;; | g        | Refresh Buffers                 |                                                                               |
;; | o        | Submoduling                     |                                                                               |
;; | r        | Rewriting                       |                                                                               |
;; | s        | Show Status                     |                                                                               |
;; | S        | Stage all                       |                                                                               |
;; | U        | Unstage all                     |                                                                               |
;; | V        | Show File                       |                                                                               |
;; | w        | Wazzup                          |                                                                               |
;; | !        | Running                         |                                                                               |
;; | $        | Show Process                    |                                                                               |

;; - git remote add origin https://github.com/kawabata/hoge.git ..
;;  "origin" という名前で "http://.../" をアップストリームリポジトリに
;;   追加
;; - git push -u origin master
;;;;; その他
;; - タグの追加とPush
;;   + t <tag_name>
;;   + P t <tag_name>
(use-package magit :no-require t :ensure t
  :bind (("M-g s" . magit-status)
         ("M-g b" . magit-blame-mode))
  :config
  (set-variable 'magit-process-find-password-functions
                '(magit-process-password-auth-source))
  )

;;;; magithub (obsolete)
;; magit の仕様変更に追随できない場合が多いので使用停止。
;;(lazyload () "magit-key-mode"
;;  (set-variable 'magit-log-edit-confirm-cancellation nil))

;;;; malyon
;; Z-Machine Interpreter for Emacs
;; M-x malyon で Z仮想機械ファイルを指定してゲーム開始。
(use-package malyon :no-require t :defer t)

;;;; mark-multiple (obsolete)
;; → multipel-cursors に名称変更。

;;;; mdfind-dired
;; https://gist.github.com/Kouzuka/900452
;; TODO check

;;;; melpa (obsolete)
;; パッケージ管理システム。ブラックリストパッケージの管理等。
;;(lazyload () "melpa"
;;  (add-to-list 'package-archive-exclude-alist '(("melpa" bbdb-vcard))))
;;(require 'melpa nil :no-error)

;;;; mercurial (obsolete)
;; /opt/local/share/mercurial/contrib/mercurial.el
;; not well implemented.  We will use `monkey'.
;;(lazyload (hg-help-overview hg-log-repo hg-diff-repo) "mercurial")

;;;; mode-info (obsolete)
;; ruby reference manual などは別途取得する。
;; emacs 23 で、 make install-index すると、
;; /usr/local/share/emacs/23.0.60/etc/mode-info/ にインデックスファイルが入る。
;;(if (file-exists-p "/usr/local/share/emacs/23.0.90/etc/mode-info/")
;;    (set-variable 'mode-info-index-directory "/usr/local/share/emacs/23.0.90/etc/mode-info/")
;;  (set-variable 'mode-info-index-directory "/usr/local/share/emacs/site-lisp/mode-info/"))
;;(when nil ;;(locate-library "mode-info")
;;  (autoload 'mode-info-describe-function "mode-info" nil t)
;;  (autoload 'mode-info-describe-variable "mode-info" nil t)
;;  (autoload 'mode-info-find-tag "mode-info" nil t)
;;  (bind-key "C-h f" 'mode-info-describe-function)
;;  (bind-key "C-h v" 'mode-info-describe-variable)
;;  (bind-key "M-." 'mode-info-find-tag)
;;  (defadvice help-for-help
;;    (before activate-mi activate)
;;    (when (locate-library "mi-config")
;;      (require 'mi-config)
;;      (require 'mi-fontify))))

;;;; monky
;; mercurial (hg) management
;; manual: http://ananthakumaran.in/monky/index.html
;; M-x monky-status
;; 'l' for log view.
(use-package monky :no-require t :defer t :ensure t)

;;;; multi-term
;; (1) たくさんのバッファを同時に開くことができる。
;; (2) zshの補完機能とshell の標準出力編集機能を同時に使うことができる。
;; ただし、独自の`eterm-color'と呼ばれるターミナルを使うので、以下のコ
;; マンドを事前に実行しておく。
;; % tic -o ~/.terminfo ~/cvs/emacs/etc/e/eterm-color.ti
;; 環境変数：TERM=eterm
;;(use-package multi-term :defer t
;;  :config
;;  (add-hook 'term-mode-hook
;;         (lambda ()
;;           ;; C-h を term 内文字削除にする
;;           (bind-key "C-h" 'term-send-backspace term-raw-map)
;;           ;; C-y を term 内ペーストにする
;;           (bind-key "C-y" 'term-paste term-raw-map)
;;           ;; 幾つかのキーをバインドから外す。
;;           (add-to-list 'term-unbind-key-list "M-x")
;;           (add-to-list 'term-unbind-key-list "M-v")
;;           (add-to-list 'term-unbind-key-list "M-o")
;;           (add-to-list 'term-unbind-key-list "C-o")
;;           (add-to-list 'term-unbind-key-list "C-e")
;;           (add-to-list 'term-unbind-key-list "M-f"))))

;;;; multiple-cursors
;; カーソルを分身させ同時に同じ環境で編集できる。
;; - Emacsでリファクタリングに超絶便利なmark-multiple
;;   http://d.hatena.ne.jp/tuto0621/20121205/1354672102
;;   http://emacsrocks.com/e13.html
(use-package multiple-cursors :no-require t :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ;;("C-M-c" .  mc/edit-lines)
         ;;("C-M-r" . mc/mark-all-in-region))
         ("C-c M-8" . mc/mark-all-like-this))
  :config
  (when (functionp 'smartrep-define-key)
    (global-unset-key (kbd "M-t")) ; TODO transpose に代わる良いキーを探す。
    (smartrep-define-key global-map "M-t"
      '(("M-t"      . 'mc/mark-next-like-this)
        ("n"        . 'mc/mark-next-like-this)
        ("p"        . 'mc/mark-previous-like-this)
        ("m"        . 'mc/mark-more-like-this-extended)
        ("u"        . 'mc/unmark-next-like-this)
        ("U"        . 'mc/unmark-previous-like-this)
        ("s"        . 'mc/skip-to-next-like-this)
        ("S"        . 'mc/skip-to-previous-like-this)
        ("*"        . 'mc/mark-all-like-this)
        ("d"        . 'mc/mark-all-like-this-dwim)
        ("i"        . 'mc/insert-numbers)
        ("o"        . 'mc/sort-regions)
        ("O"        . 'mc/reverse-regions)))))

;;;; nav
;; http://d.hatena.ne.jp/wocota/20091001/1254411232
;; ディレクトリナビゲータ
(use-package nav :no-require t :ensure t
  :commands (nav-toggle))

;;;; navi (abstain)
;; navi はバッファの概略を表示するパッケージ。
;; outline-mode で代替できるので不要。
;;(when (locate-library "navi")
;;  (autoload 'navi "navi" "navi." t nil)
;;  (bind-key [f11]  'call-navi)
;;  (bind-key "C-x C-l" 'call-navi)
;;  (defun call-navi ()
;;    (interactive)
;;    (navi (buffer-name))))

;;;; navi2ch
;; 開発サイト
;; https://github.com/naota/navi2ch/
;; * インストール
;;   % cp *.el *.elc ~/.emacs.d/site-lisp/navi2ch/
;;   % cp icons/* ~/.emacs.d/etc/navi2ch/icons
;; * 板の追加方法 :: ~/.emacs.d/navi2ch/etc.txt に以下を記入
;;     板の名前
;;     板の URL
;;     板の ID
;; * JBBS
;;   URLに jbbs.livedoor.jp が入っていれば、 navi2ch-jbbs-shitaraba.el で処理
;; TODO: 書き込みバッファが read-only で書き込めない。
;; - navi2ch-message-insert-header 関数で read-only 文字列を入れている。
;;
;; - delete-windows-on 問題
;; navi2ch は、スレのバッファを閉じるのに delete-windows-on 関数を用い
;; るが、frame-cmds.el で再定義される delete-windows-on 関数は、バッファ
;; に一つしかwindow を出さない場合はておく。
;;
;; - 専ブラ問題
;; 2ch用のプロクシで対処する。
;; http://prokusi.wiki.fc2.com/wiki/2chproxy.pl
;; https://github.com/yama-natuki/2chproxy.pl
;; 設定方法
;; % git clone https://github.com/yama-natuki/2chproxy.pl
;;   - 2chproxy.pl 内の設定
;;     DEDICATED_BROWSER=Navi2ch
;;     DAT_DIRECTORY => "$ENV{HOME}/.navi2ch/"
;;     （ perl ./2chproxy.pl で起動）
;;   - emacs での設定
;;     (set-variable 'navi2ch-net-http-proxy "localhost:8080")
;;
;; - 検索
;;   - C-c C-s :: navi2ch-search-web
(use-package navi2ch :no-require t :ensure t
  :bind ("C-c N" . navi2ch)
  :config
  (set-variable 'navi2ch-directory (locate-user-emacs-file "navi2ch"))
  (autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
  (set-variable 'navi2ch-icon-directory
        (locate-user-emacs-file "etc/navi2ch/icons"))
  ;; 検索結果の最大取得数（デフォルトは30）
  (set-variable 'navi2ch-search-find-2ch-search-num 50)
  ;; 2chproxy での接続
  (set-variable 'navi2ch-net-http-proxy "localhost:8080")
  ;; モナーフォントを使用する。
  ;; DONE navi2ch-mona-setup.el で、mac でも使えるように変更が必要。
  (set-variable 'navi2ch-mona-enable t)
  (set-variable 'navi2ch-mona-use-ipa-mona t)
  (when (find-font (font-spec :family "IPAMonaPGothic"))
    (set-variable 'navi2ch-mona-ipa-mona-font-family-name "IPAMonaPGothic"))
  ;; 既読スレはすべて表示
  (set-variable 'navi2ch-article-exist-message-range '(1 . 1000))
  ;; 未読スレもすべて表示
  (set-variable 'navi2ch-article-new-message-range '(1000 . 1))
  ;; navi2ch 検索メソッドの変更
  (set-variable 'navi2ch-search-web-search-method 'navi2ch-search-union-method)
  (set-variable 'navi2ch-search-union-method-list '(navi2ch-search-hula-method))
  ;; Oyster (2ch viewer)
  (add-hook
   'navi2ch-before-startup-hook
   (lambda ()
     (let ((secret (plist-get (nth 0 (auth-source-search :host "2chv.tora3.net"))
                              :secret)))
       (when (functionp secret) (callf funcall secret))
       (when secret
         (set-variable 'navi2ch-oyster-use-oyster t)
         (set-variable 'navi2ch-oyster-password secret)
         (set-variable 'navi2ch-oyster-id "kawabata@clock.ocn.ne.jp")))))
  (add-hook 'navi2ch-bm-exit-hook
            (lambda () (set-variable 'navi2ch-oyster-password nil))))

;;;; nethack
;; "6x10" フォントを要求するので当面コメントアウトする。
;;(lazyload (nethack) "nethack"
;;  (when (executable-find "jnethack")
;;    (add-to-list 'process-coding-system-alist
;;                 '("nethack" euc-jp . euc-jp))
;;    (set-variable 'nethack-program "jnethack")))

;;;; oneliner
;; http://oneliner-elisp.sourceforge.net
;;(require 'oneliner nil :no-error)

;;;; nyan-prompt (abstain)
;; eshell-bol がうまく動かなくなるので使用中止。
;; (when (functionp 'nyan-prompt-enable)
;;   (lazyload () "eshell"
;;     (add-hook 'eshell-load-hook 'nyan-prompt-enable)))
;; 使用をやめるときは、
;; eshell-prompt-function と eshell-prompt-regexp をもとに戻す。

;;;; org
;;;;; org/org

;; git の最新版を使う際にはかならず make して org-autoloads を生成すること。

;; * 基本記法 (refcard: /opt/local/share/emacs/24.2.50/etc/refcards/orgcard.pdf)
;;   *bold*, /italic/, _underlined_, =code=, ~verbatim~,  +strike-through+.
;;   [[link][title]] [[*link in org-file]] [[./fig/hogehoge.png]]
;;   画像は org-toggle-inline-images (C-c C-x C-v）にて切り替え。
;;     #+CAPTION: ...
;;     #+NAME: fig:hoge → [[fig:hoge]] で参照。
;; * スケジュール設定
;;   C-c C-d (org-deadline)
;;   C-c C-s (org-schedule)
;;   C-'     (org-cycle-agenda-files)
;; * Easy Templates (snippets)
;;   行頭で `<' `<TAB>' `s' 等。 org-structure-template-alist 参照
;; * org-mode の ELPA版・Emacs標準添付版のパスを削除する。
;;  （contrib に含まれているツールをつかうため。）
;(set-variable 'load-path
;      (cl-delete-if (lambda (x) (or (string-match "/elpa/org-20" x)
;                                    (string-match "/lisp/org" x))) load-path))

;; org-mode で使用する数学記号パッケージの一覧
;; 一覧は http://milde.users.sourceforge.net/LUCR/Math/unimathsymbols.pdf 参照

(defvar tkw-org-latex-math-symbols-packages-alist
    '(("" "amssymb"   t)
      ("" "amsmath"   t)
      ("" "amsxtra"   t) ; MathJax未対応
      ;;("" "bbold"     t)
      ;; 形式的論理のスタイルファイル
      ;; http://www.logicmatters.net/resources/pdfs/latex/BussGuide2.pdf
      ;; http://www.logicmatters.net/latex-for-logicians/nd/
      ("" "bussproofs" t) ; 自然推論
      ("all" "xypic" t) ; ダイヤグラム
      ("" "isomath"   t) ; MathJax未対応
      ("" "latexsym"  t) ; MathJax未対応
      ("" "marvosym"  t) ; Martin Vogel's Symbols Font
      ;;("" "mathdots"  t) ; MathJax未対応
      ("" "stmaryrd"  t) ; MathJax未対応
      ("" "textcomp"  t) ; 特殊記号
      ("" "wasysym"   t) ; Waldi symbol font. bussproofs と衝突。
      ))

(use-package org :no-require t :defer t :ensure org-plus-contrib
  :init
  (let ((git-dir "~/cvs/org-mode/lisp"))
    (when (file-directory-p git-dir)
      (pushnew git-dir load-path)))
  :config
  ;; orgモジュール
  (set-variable 'org-modules
        '(
          ;;org-w3m
          org-bbdb
          org-bibtex
          ;;org-docview
          ;;org-gnus
          org-info
          ;;org-mhe
          ;;org-irc
          ;;org-rmail
          ))
  (set-variable 'org-footnote-tag-for-non-org-mode-files "脚注:")
  (set-variable 'org-format-latex-options
        '(:foreground default :background default :scale 1.5
          :html-foreground "Black" :html-background "Transparent"
          :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (set-variable 'org-list-demote-modify-bullet
        ;;'(("+" . "-") ("-" . "+") ("*" . "+")))
        '(("+" . "-") ("-" . "*") ("*" . "+")))
  ;; * 数式を生成する際に、 `latex' 命令で利用できるパッケージを仮設定す
  ;;   るアドバイス
  (defadvice org-create-formula-image-with-dvipng
    (around org-reset-default-packages activate)
    (let ((org-export-latex-default-packages-alist
           `(;("" "fixltx2e" nil)
             ,@tkw-org-latex-math-symbols-packages-alist
             "\\tolerance=1000"))
          (org-export-latex-packages-alist nil))
      ad-do-it))
  (ad-activate 'org-create-formula-image-with-dvipng))

;;;;; org/org-agenda
(use-package org-agenda :no-require t :defer t
  :config
  (set-variable 'org-agenda-include-diary t))

;;;;; org/org-bbdb
;; %%(org-bbdb-anniversaries)

;;;;; org/org-capture
(use-package org-capture :no-require t :defer t
  :defines (org-capture-templates)
  :config
  ;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
  (set-variable 'org-capture-templates
        '(("w" "work/今日の仕事" entry
           (file+headline "work/work.org.txt" "今日の仕事")
           "** TODO %?\n   %i\n   %a\n   %t" :prepend t)
          ("c" "work/調査・確認" entry
           (file+headline "work/work.org.txt" "調査・確認")
           "** TODO %?\n   %i\n   %a\n   %t" :prepend t)
          ("s" "今日の作業" entry
           (file+headline "home/home.org.txt" "今日の作業")
           "** TODO %?\n   %i\n   %a\n   %t" :prepend t)
          ("i" "home/アイデア" entry
           (file+headline "home/home.org.txt" "アイデア")
           "** %?\n   %i\n   %a\n   %t" :prepend t)
          ("b" "home/本" entry
           (file+headline "home/home.org.txt" "本")
           "** %?\n" :prepend t)
          ("k" "home/買い物" entry
           (file+headline "home/home.org.txt" "買い物")
           "** TODO %?\n" :prepend t)
          ("e" "home/Emacs" entry
           (file+headline "home/home.org.txt" "Emacs")
           "** %?\n" :prepend t)
          ("l" "home/Lookup" entry
           (file+headline "home/home.org.txt" "Lookup")
           "*** %?\n" :prepend t)
          ("j" "home/JavaScript" entry
           (file+headline "home/home.org.txt" "JavaScript")
           "** %?\n" :prepend t)
          ("k" "home/文字・言語" entry
           (file+headline "home/home.org.txt" "文字・言語")
           "** %?\n" :prepend t)
          ("t" "home/Twitter" item
           (file+headline "home/home.org.txt" "Twitter")
           "\n- %?\n\n" :prepend t)
          ("h" "Howm式メモ" entry
           (function
            (lambda ()
              (require 'howm-vars)
              (let ((file
                     (expand-file-name
                      (format-time-string
                       "%Y/%m/%Y-%m-%d-%H%M%S.howm"
                       (current-time)) howm-directory)))
                (make-directory (file-name-directory file) t)
                (set-buffer (org-capture-target-buffer file)))))
           ;; 注意。ここの文字列が org-capture-verify-tree のチェックを通らないとエラーになる
           "* #+TITLE: %t\n**%?" :prepend t))))

;;;;; org/org-clock
;; C-c C-x C-i     (org-clock-in)
;; C-c C-x C-o     (org-clock-out) ;; C-o が衝突するためM-oに変更
;; C-c C-x C-x     (org-clock-in-last)
;; C-c C-x C-e     (org-clock-modify-effort-estimate)
(use-package org-clock :no-require t :defer t
  :config
  (org-defkey org-mode-map (kbd "C-c C-x M-o") 'org-clock-out)
  (set-variable 'org-clock-persist 'history))

(defun remove-org-newlines-at-cjk-text (&optional _mode)
  "先頭が '*', '#', '|' でなく、改行の前後が日本の文字の場合はその改行を除去する。
org-mode で利用する。
U+3000 以降の文字同士が改行で分割されていた場合は改行を削除する。
XeTeX/LuaTeX や HTML, DocBook 等、日本語の改行が空白扱いになる箇所で利用する。"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^|#*\n].+\\)\\(.\\)\n *\\(.\\)" nil t)
    (if (and (> (string-to-char (match-string 2)) #x2000)
               (> (string-to-char (match-string 3)) #x2000))
        (replace-match "\\1\\2\\3"))
    ;;(replace-match "\\1\\2 \\3"))
    (goto-char (point-at-bol))))

(defun remove-org-newlines-at-cjk-kill-ring-save (from to)
  "org-mode の FROM から TO のテキストを PowerPoint などにコピーする際に
キルリングに入る日本語の改行・空白を削除する。"
  (interactive "r")
  (let ((string (buffer-substring from to)))
    (with-temp-buffer
      (insert string)
      (remove-org-newlines-at-cjk-text)
      (copy-region-as-kill (point-min) (point-max))))
  (if (called-interactively-p 'interactive)
      (indicate-copied-region)))

(bind-key "M-W" 'remove-org-newlines-at-cjk-kill-ring-save)

;;;;; org/org-feed
;; RSSリーダ
;; M-x org-feed-update-all
;; M-x org-feed-update
;; M-x org-feed-goto-inbox
(use-package org-feed :no-require t :defer t
  :config
  (set-variable 'org-feed-retrieve-method 'wget) ; wget でフィードを取得
  ;; テンプレート
  (set-variable 'org-feed-default-template "\n* %h\n  - %U\n  - %a  - %description")
  ;; フィードのリストを設定する。
  (defvar tkw-org-feeds-file (expand-file-name "feeds.org" org-directory))
  (set-variable 'org-feed-alist
        `(
          ("Slashdot"
           "http://rss.slashdot.org/Slashdot/slashdot"
           ,tkw-org-feeds-file
           "Slashdot" ))))

;;;;; org/org-mobile
(use-package org-mobile :no-require t :defer t
  :config
  (set-variable 'org-mobile-directory "~/share/MobileOrg"))

;;;;; org/org-protocol
;; 設定方法：(http://orgmode.org/worg/org-contrib/org-protocol.html)
;; - 仕組み
;; (1) OS で、 "org-protocol:" というプロトコルを設定し、起動アプリケー
;;     ションを emacsclient にする。
;; (2) Emacs側で (require 'org-protocol) する。
;; (3) ブラウザ側で、org-protocol:/sub-protocol: でEmacs Client に接続する。

;; - org-protocol:/org-protocol-store-link:  ...
;;     javascript:location.href='org-protocol://store-link://'+encodeURIComponent(location.href)
;; - org-protocol:/org-protocol-capture:     ...
;;     javascript:location.href='org-protocol://capture://'+
;;           encodeURIComponent(location.href)+'/'+
;;           encodeURIComponent(document.title)+'/'+
;;           encodeURIComponent(window.getSelection())
;; - org-protocol:/org-protocol-open-source: ...

;; - MacOSX … "EmacsClient.app"
;;             (https://github.com/neil-smithline-elisp/EmacsClient.app/blob/master/EmacsClient.zip)
;;             をインストールする。起動するEmacsClientのパスは
;;             /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
;;             になっている。変更するには、EmacsClient.app を単独起動し
;;             て、「このスクリプトを実行するには…」のダイアログボック
;;             スが出ている間に編集メニューから「スクリプトの編集」を行
;;             い、AppleScriptエディタでパスを
;;             /usr/local/bin/emacsclient など、既存のインストール済の
;;             ものに設定する。

;; - Windows … レジストリファイルを作成して起動。

;; org-protocol-capture （選択テキストをorg-mode にキャプチャ）
;; org-protocol-store-link （選択リンクをブックマークとしてコピー）
(use-package org-protocol :no-require t
  :commands (org-protocol-create))

;;;;; org/org-table
;; C-c { で設定する。

;;;;; org/ob
;; ソースコードの編集は C-c '
;; C-c で評価し、結果は #+RESULTに置かれる。
(defvar org-ditaa-jar-path
  (locate-user-emacs-file "program/jditaa.jar"))

(use-package ob :no-require t :defer t
  :config
  ;;(set-variable 'org-babel-load-languages
  ;;              `((C . t)
  ;;                (css . t)
  ;;                (emacs-lisp . t)
  ;;                ,@(and (file-exists-p org-ditaa-jar-path)
  ;;                       '((ditaa . t)))
  ;;                (sh . t)
  ;;                ,@(and (executable-find "clj")
  ;;                       '((clojure . t)))
  ;;                ,@(and (executable-find "dot")
  ;;                       '((dot . t)))
  ;;                ,@(and (executable-find "ghc")
  ;;                       '((haskell . t)))
  ;;                ,@(and (executable-find "gnuplot")
  ;;                       '((gnuplot . t)))
  ;;                ,@(and (executable-find "node")
  ;;                       '((js . t)))
  ;;                ,@(and (executable-find "javac")
  ;;                       '((java . t)))
  ;;                ,@(and (executable-find "latex")
  ;;                       '((latex . t)))
  ;;                ,@(and (executable-find "lilypond")
  ;;                       (locate-library "lilypond-init" t)
  ;;                       (pushnew '("lilypond" . LilyPond) org-src-lang-modes)
  ;;                       '((lilypond . t)))
  ;;                ,@(and (executable-find "mscgen")
  ;;                       '((mscgen . t)))
  ;;                ,@(and (executable-find "R")
  ;;                       '((R . t)))
  ;;                ,@(and (executable-find "ruby")
  ;;                       '((ruby . t)))
  ;;                ,@(and (executable-find "sqlite")
  ;;                       '((sqlite . t)))))
  )

;;;;; org/ox
;; html (deck/s5/md) → (pandoc)
;; odt (odf) → (pandoc)
;; latex (beamer) → (pandoc)
;; icalendar
;; taskjuggler
;; (pandoc) → docbook, latex, mediawiki, etc. etc...

(use-package ox :no-require t :defer t
  :defines (org-export-filter-final-output-functions)
  :config
  (add-hook 'org-export-before-processing-hook 'remove-org-newlines-at-cjk-text))

;;;;; org/ox-freemind
(use-package ox-freemind :no-require t :defer t
  :config
  ;;(set-variable 'org-export-headline-levels 6) ;; #+OPTIONS H:6
  (set-variable
   'org-freemind-styles
   '((default . "<node>\n</node>")
     (0 . "<node COLOR=\"#000000\">\n<font NAME=\"SansSerif\" SIZE=\"20\"/>\n</node>")
     (1 . "<node COLOR=\"#0033ff\" FOLDED=\"true\">\n<edge STYLE=\"sharp_bezier\" WIDTH=\"8\"/>\n<font NAME=\"SansSerif\" SIZE=\"18\"/>\n</node>")
     (2 . "<node COLOR=\"#00b439\" FOLDED=\"true\">\n<edge STYLE=\"bezier\" WIDTH=\"thin\"/>\n<font NAME=\"SansSerif\" SIZE=\"16\"/>\n</node>")
     (3 . "<node COLOR=\"#990000\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (4 . "<node COLOR=\"#0033ff\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (5 . "<node COLOR=\"#00b439\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (6 . "<node COLOR=\"#111111\">\n</node>")))
  (set-variable 'org-freemind-section-format 'inline))

;;;;; org/ox-html
;; org-html-head
(use-package ox-html :no-require t :defer t
  :config
  ;; (set-variable 'org-html-head
  ;; "<link rel='stylesheet' type='text/css' href='style.css' />")
  ;; org-html-head-extra
  ;; org-html-preamble-format
  ;; org-html-postamble-format
  (set-variable 'org-html-postamble t)
  (set-variable 'org-html-postamble-format
      '(("en" ""))))

;;;;; org/ox-icalendar
;; M-x org-icalendar-combine-agenda-files
(use-package ox-icalendar :no-require t :defer t
  :config
  ;; agenda を ICSにエクスポートして、Dropbox の公開フォルダにコピーする。
  (let ((secret (plist-get (nth 0 (auth-source-search :host "www.getdropbox.com"))
                           :secret)))
    ;; Dropbox の秘密公開ディレクトリの設定
    (if (functionp secret)
        (set-variable 'org-icalendar-combined-agenda-file
                      (expand-file-name
                       "org.ics"
                       (concat "~/share/Public/" (funcall secret))))
      (error "Dropbox Secret Folder not set!")))
  ;; iCal の説明文
  (set-variable 'org-icalendar-combined-description "OrgModeのスケジュール出力")
  ;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
  (set-variable 'org-icalendar-timezone "Asia/Tokyo")
  ;; DONE になった TODO は出力対象から除外する
  (set-variable 'org-icalendar-include-todo t)
  ;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
  (set-variable 'org-icalendar-use-scheduled '(event-if-todo))
  ;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
  (set-variable 'org-icalendar-use-deadline '(event-if-todo)))

;;;;; org/ox-latex

;; ソースコードに色を付ける方法 ::
;; org-export-latex-packages-alistに、"listings" と "color" が含まれる
;; こと。以下をorgファイルに入れる。
;; \definecolor{keywords}{RGB}{255,0,90}
;; \definecolor{comments}{RGB}{60,179,113}
;; \definecolor{fore}{RGB}{249,242,215}
;; \definecolor{back}{RGB}{51,51,51}
;; \lstset{
;;   basicstyle=\color{fore},
;;   keywordstyle=\color{keywords},
;;   commentstyle=\color{comments},
;;   backgroundcolor=\color{back}
;; }

(declare-function tkw-ox-latex-engine-set "init.el")
(use-package ox-latex :no-require t :defer t
  :functions (org-export-derived-backend-p)
  :config
  (add-hook 'org-export-before-processing-hook 'tkw-ox-latex-tex-engine-setup)
  (defun tkw-ox-latex-engine-set (latex)
    "Set up LATEX environments."

    ;; すべてのLaTeX出力に共通なパッケージ
    ;; [NO-DEFAULT-PACKAGES] 指定があると含まれない。
    (set-variable 'org-latex-default-packages-alist
                  `(,@(case latex ; 各 TeX に応じた日本語パッケージ設定
                        ('luatex '(("" "luacode" t)
                                   ("" "luatexja-otf" t)))
                        ('xetex  '(;; noCJKchecksiingle で、\meaning の非BMPでの分割を抑止
                                   ("AutoFallBack=true,noCJKchecksingle" "zxjatype" t)
                                   ;;("macros" "zxotf" t)
                                   ))
                        ('euptex '(("uplatex,multi" "otf" t)
                                   ("" "okumacro" t)))
                        (t nil))
                    ("" "fixltx2e" nil) ; 互換性より利便性を重視したLaTeX2eのバグ修正
                    ("" "fancyvrb" t) ; Verbatimで枠線を綺麗に出力
                    ("" "longtable" nil) ; ページをまたがるテーブルの作成
                    ("" "float" nil)
                    ;; ("" "wrapfig" nil) ; figureをwrapする。
                    ;; ("" "soul" t) ; ドイツ語用
                    ;; LaTeX標準文字記号マクロ
                    ,@tkw-org-latex-math-symbols-packages-alist
                    ("" "bigtabular" t)
                    ("" "multicol" t)
                    ;; その他のデフォルトで使用するLaTeX設定
                    ,(concat
                      "\\tolerance=1000\n"
                      "\\providecommand{\\alert}[1]{\\textbf{#1}}\n"
                      "\\fvset{xleftmargin=2em}\n")
                    ;; XeTeXの場合、1.9999 以降は IVSが使えるので、これらに glue が
                    ;; 入らないよう、キャラクタクラスを 256 にする。
                    ,@(when (equal latex 'xetex)
                        (list(concat
                              "\\setjamainfont{HanaMinA}\n"
                              "\\setCJKfallbackfamilyfont{rm}{HanaMinB}\n"
                              )))
                    ))

    ;; LaTeX出力用の非標準パッケージ.
    ;; [NO-PACKAGES] 指定があると含まれない。
    (set-variable 'org-latex-packages-alist
                  `(
                    ;; graphicx: jpeg や png を取り込む。
                    ;;   ebb *.png 命令を実行しないと Bounding Boxが生成されない。
                    ,(case latex
                       ('xetex  '("" "graphicx"  t))
                       ('euptex '("dvipdfmx" "graphicx"  t))
                       (t       '("pdftex" "graphicx"  t)))
                    ;; hyperref: PDFでハイパーリンクを生成
                    ;; colorlinks=true を入れると、graphicx が dvipdfmx で失敗するので注意。
                    ,(case latex
                       ('luatex '("pdftex,pdfencoding=auto" "hyperref" t))
                       ('euptex '("dvipdfm" "hyperref"  t))
                       ('xetex  '("xetex" "hyperref"  t))
                       (t       '("pdftex" "hyperref"  t)))
                    ;; biblatex は重いので、使用するorg-fileのみ、
                    ;; `+LATEX_HEADER: \usepackage[backend=biber]{biblatex}'
                    ;; で入れるのが良い。
                    ;; ("backend=biber", "biblatex" t)
                    ;; ↓これを入れると、includegraphics で png が入らないので注意。
                    ("" "listings")
                    ,(case latex
                       ('luatex '("pdftex,pdfencoding=auto" "color" t))
                       ('euptex '("dvipdfmx" "color"  t))
                       ('xetex  '("xetex" "color"  t))
                       (t       '("pdftex" "color"  t)))
                    ))

    (set-variable 'org-latex-classes ; LaTeX クラス名・ヘッダ・セクション
                  `(("article"
                     ,(case latex
                        ('luatex "\\documentclass{ltjsarticle}\n")
                        ('xetex  "\\documentclass[a4paper]{bxjsarticle}\n")
                        ('euptex "\\documentclass[a4j,uplatex]{jsarticle}\n")
                        (t       "\\documentclass[11pt]{article}"))
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                    ("report"
                     ,(case latex
                        ('luatex "\\documentclass{ltjsarticle}\n")
                        ('xetex  "\\documentclass[a4paper]{bxjsreport}\n")
                        ('euptex "\\documentclass[11pt,report,uplatex]{jsbook}\n")
                        (t       "\\documentclass[11pt]{article}"))
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                    ("book"
                     ,(case latex
                        ('luatex "\\documentclass{ltjsarticle}\n")
                        ('xetex  "\\documentclass[9pt,a4paper]{bxjsreport}\n")
                        ('euptex "\\documentclass[9pt,a5j,uplatex]{jsbook}\n")
                        (t       "\\documentclass[11pt]{book}"))
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                    ("beamer"
                     ;; #+LaTeX_CLASS_OPTIONS: [presentation,dvipdfm] をつけたサンプ
                     ;; #ルには注意すること。
                     ;;,(concat "\\documentclass[compress,dvipdfm]{beamer}\n[NO-PACKAGES]\n"
                     ,(concat
                       (case latex
                         ('xetex
                          "\\documentclass[compress,xdvipdfmx]{beamer}\n")
                         (t "\\documentclass[compress,dvipdfmx]{beamer}\n"))
                       "\\usetheme{AnnArbor}\n"
                       "\\setbeamertemplate{navigation symbols}{}\n"
                       "[NO-PACKAGES]\n"
                       "\\usepackage{graphicx}\n")
                     org-beamer-sectioning)))

    (set-variable 'org-latex-pdf-process (list (tkw-latexmk-command TeX-engine "-pv" "%f" "%o"))))

  (defun tkw-ox-latex-tex-engine-setup (backend)
    (message "backend=%s" backend)
    (when (equal backend 'latex)
      (tkw-ox-latex-engine-set TeX-engine)))

  ;; "verbatim" → "Verbatim" 置換 (fancyvrb)
  (defun tkw-org-latex-filter-fancyvrb (text backend _info)
    "Convert begin/end{verbatim} to begin/end{Verbatim}."
    (when (or (org-export-derived-backend-p backend 'beamer)
              (org-export-derived-backend-p backend 'latex))
      (replace-regexp-in-string
       "\\\\\\(begin\\|end\\){verbatim}"
       "\\\\\\1{Verbatim}" text)))

  (pushnew 'tkw-org-latex-filter-fancyvrb
           org-export-filter-final-output-functions)

  ;; "tabular" → "Tabular" 置換 (bigtabular)
  (defun tkw-org-latex-filter-bigtabular (text backend _info)
    "Convert begin/end{tabular} to begin/end{Tabular}."
    (when (or (org-export-derived-backend-p backend 'beamer)
              (org-export-derived-backend-p backend 'latex))
      (replace-regexp-in-string
       "\\\\\\(begin\\|end\\){tabular}"
       "\\\\\\1{Tabular}" text)))

  (pushnew 'tkw-org-latex-filter-bigtabular
           org-export-filter-final-output-functions)
)

;;;;; org/ox-odt.el
;; 出力されたodt を、MacのTextEditで開くとぐちゃぐちゃになる。
;; → Windows で開く。
;; → pandoc -o XYZ.docx XYZ.odt で変換する。
;; pandoc は、/usr/local/bin に入れる。（MacPortsは利用しない。）
;; % ports deps package-name
;; デフォルト出力ファイルのフォントの変更
;; →　MS Gothic SimSun → MS Mincho
;;(eval-after-load 'org-odt
;;  '(let ((style-dir "~/.emacs.d/etc/org/"))
;;     (if (file-directory-p style-dir)
;;         (set-variable 'org-odt-styles-dir style-dir))))

;;;;; org/ox-publish
(use-package ox-publish :no-require t :defer t
  :config
  ;;
  ;; ePub3 publishing
  ;;
  ;;         :org-sitemap-function
  ;; +------------+         +--------------+
  ;; | org-files  +-------->| sitemap.org  |
  ;; +------+-----+         +------+-------+
  ;;        | :publishing-function |
  ;;        v                      v
  ;; +------------+         +--------------+
  ;; | html-files |         | sitemap.html |
  ;; +------+-----+         +------+-------+
  ;;        | :completion-function |
  ;;        |    +-----------+     |
  ;;        +--->| genpub.rb |<----+
  ;;             +-----+-----+
  ;;                   |
  ;;                   v
  ;;             +-----------+
  ;;             | ePub file |
  ;;             +-----------+
  ;;
  ;; EPub3への出力は全て、pandoc を使うので下記設定は利用停止。
  ;;
  ;;(set-variable 'org-publish-project-alist
  ;;      ;; Sample ePUB3 publication project
  ;;      '(("epub3-sample"
  ;;         :base-directory "~/share/lessons/org-mode/publish/"
  ;;         :base-extension "org"
  ;;         :publishing-function org-html-publish-to-html
  ;;         :publishing-directory "~/share/Public/org-publish/"
  ;;         :auto-sitemap t
  ;;         :sitemap-title "タイトルのテスト"
  ;;         :sitemap-subtitle "サブタイトルのテスト"
  ;;         :sitemap-filename "sitemap.org"
  ;;         :with-author "山田 太郎 "
  ;;         :with-author-reading "やまだ たろう"
  ;;         :section-numbers nil
  ;;         :with-toc nil
  ;;         ;;:preparation-function tkw-org-publish-prep-function
  ;;         :completion-function tkw-org-publish-epub3-function)))
  ;;
  ;;(defun tkw-org-publish-epub3-function ()
  ;;  (let* ((plist (symbol-value 'project-plist))
  ;;         (publishing-directory (plist-get plist :publishing-directory))
  ;;         (default-directory publishing-directory))
  ;;    (with-temp-file (expand-file-name
  ;;                     "generate.rb" publishing-directory)
  ;;      (insert (tkw-org-publish-gepub-script plist)))
  ;;    (message "now publishing epubs in %s..." default-directory)))
  ;;
  ;;(defun tkw-org-publish-gepub-script (plist)
  ;;  (cl-flet ((\, (key) (plist-get plist key)))
  ;;    (let* ((sitemap-html (concat (file-name-sans-extension ,:sitemap-filename) ".html"))
  ;;           (content-files
  ;;            (nreverse
  ;;            (with-temp-buffer
  ;;              (insert-file-contents sitemap-html)
  ;;              (loop while (re-search-forward "<li><a href=\"\\(.+\\)\">" nil t)
  ;;                    collect (match-string 1))))))
  ;;      (concat
  ;;       "require 'gepub'\n"
  ;;       "builder = GEPUB::Builder.new {\n"
  ;;       "  language 'ja'\n"
  ;;       "  unique_identifier 'http:/example.jp/bookid_in_url', 'BookID', 'URL'\n"
  ;;       "  title '" ,:sitemap-title "'\n"
  ;;       (when ,:sitemap-subtitle
  ;;         (concat "  subtitle '" ,:sitemap-subtitle "'\n"))
  ;;       "  creator '" ,:with-author "'\n"
  ;;       "  contributors ''\n"
  ;;       "  date '" (format-time-string "%Y-%m-%dT%T") "Z'\n"
  ;;       "  #cover_image 'img/cover.jpg'\n"
  ;;       "  resources(:workdir => '.') {\n"
  ;;       "    nav '" sitemap-html "'\n"
  ;;       "    ordered {\n"
  ;;       (mapconcat
  ;;        (lambda (file)
  ;;          (concat "      file '" file "'\n"))
  ;;        content-files "")
  ;;       "    }\n"
  ;;       "  }\n"
  ;;       "}\n"
  ;;       "epubname = File.join(File.dirname(__FILE__), 'sample.epub')\n"
  ;;       "builder.generate_epub(epubname)\n"))))
  )

;;;;; org/ox-taskjuggler
;; プロパティ設定メモ
;; :PROPERTIES:
;; :task_id:  logic_1
;; :ORDERED:  t <-- blocker が順番に並べる
;; :BLOCKER:  logic_0
;; :Effort:   20d
;; :allocate: kawabata, yoshida
;; :END:
;; 注意点
;; - アイテムの下に下位アイテムを置く場合は、上位アイテムに :PROPERTIES: を
;;   書かないこと。"milestoneがない" エラーになる。
(use-package ox-taskjuggler :no-require t :defer t
  :config
  (let ((secret (plist-get (nth 0 (auth-source-search :host "www.ntt.co.jp"))
                           :secret)))
    (when secret
      (set-variable 'org-taskjuggler-reports-directory
                    (expand-file-name (concat "~/share/Public/"
                                              (funcall secret) "/tj-reports")))
      ;; `secret' is lexically bound.
      (defun tkw-taskjuggler-page ()
        (interactive)
        (browse-url (concat "https://dl.dropboxusercontent.com/u/463784/"
                            (funcall secret) "/tj-reports/Plan.html"))))))

;;;;; org/ox-texinfo
(use-package ox-texinfo :no-require t
  :commands (org-texinfo-export-to-texinfo))

;;;;; org/contrib/org-annotate-file
(use-package org-annotate-file :no-require t
  :bind ("C-c C-l" . org-annotate-file)
  :init
  (defun bookmark-show-org-annotations ()
  "Opens the annotations window for the currently selected bookmark file."
  (interactive)
  (bookmark-bmenu-other-window)
  (org-annotate-file)
  ;; or, if you're using the http://bitbucket.org/nickdaly/org-annotate-file fork,
  ;; (org-annotate-file-show-annotations)
  )
  :config
  (set-variable 'org-annotate-file-storage-file "~/.emacs.d/org-annotated.org")
  (add-hook 'bookmark-bmenu-mode-hook
            (lambda ()
              (local-set-key (kbd "a") 'bookmark-show-org-annotations))))

;;;;; org/contrib/org-bibtex-extras
(use-package org-bibtex-extras :no-require t :defer t
  :defines org-export-current-backend
  :functions obe-html-export-citations
  :config
  (set-variable 'obe-bibtex-file nil)
  (add-hook 'org-export-before-parsing-hook
            (lambda ()
              (when (equal org-export-current-backend 'html)
                (obe-html-export-citations)))))

;;;;; org/contrib/org-contacts
(use-package org-contacts :no-require t :defer t
  :config
  (pushnew
   `("c" "Contacts" entry (file "~/share/org/contacts.org")
     ,(concat "* %(org-contacts-template-name)\n"
              ":PROPERTIES:\n"
              ":EMAIL: %(org-contacts-template-email)\n"
              ":END:"))
   org-capture-templates))

;;;;; org/contrib/ox-deck
;; file:/Users/kawabata/share/Public/
;; → https://dl.dropboxusercontent.com/u/463784/
(use-package ox-deck :no-require t
  :commands (org-deck-export-as-html org-deck-export-to-html)
  :defines (org-deck-directories)
  :config
  (set-variable 'org-deck-directories (list (expand-file-name "~/share/Public/deck.js")))
  (set-variable 'org-deck-base-url (concat "file:" (car org-deck-directories))))
        ;;org-deck-base-url "http://imakewebthings.com/deck.js"))

;;;;; org/contrib/ox-s5
(use-package ox-s5 :no-require t
  :commands (org-s5-export-as-html org-s5-export-to-html))

;;;; org/contrib/org-bullets
(use-package org-bullets :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'org-bullets-mode)))

;;;; org-mac-iCal (abstain)
;; iCalendar → diary
;;(lazyload () "org"
;;  (when (require 'org-mac-iCal nil t)
;;    (add-to-list 'org-modules 'org-mac-iCal)))

;;;; org-magit
;; org-mode で magit のリンクを以下のように作成する。
;; - magit:/path/to/repo::commit@<hash>
;; - magit:/path/to/repo::status
;; - magit:/path/to/repo::log
;; magit-status バッファで org-store-link することでリンクが生成される。
;;(lazyload () "org"
;;  (when (executable-find "git")
;;    (require 'org-magit nil :no-error)))

;;;; org-octopress
;; * 参考文献
;;   - https://github.com/yoshinari-nomura/org-octopress （本家）
;;   - http://quickhack.net/nom/blog/2013-05-01-org-octopress.html （作者ブログ）
;; * Octopressの設定
;;   - 普通にインストールし、テーマを設定する。
;;   - _config.ymlを編集する。
;;     + permlink を "/blog/:year-:month-:day-:title.html" にする。
;;     + 名前・タイトル等を変更する。
;; * ブログの書き方：
;;   - M-x org-octopress
;;   - w を押してブログを書く。
;;   - 画像は blog/images 等のディレクトリを作り、相対リンクで参照する。
;;   - dot/ditaa のグラフは以下のように書く。
;;     | :NOT:
;;     | #+NAME: dottyExample
;;     | #+BEGIN_SRC dot :file ./images/2013/exampleDotty.png :exports results のように書く。
;;     | ...
;;     | #+END_SRC
;;     | :END:
;;     | #+RESULTS[6d2ac873c0fc0e0232e317a2a88e734600e5ac0d]: dottyExample
;;     | #+CAPTION: Example of Dotty Image
;;     | [[file:./images/2013/exampleDotty.png]]
;;   - C-c C-e P x octopress
;; * octopress のアップグレード
;;   - doc:: http://octopress.org/docs/updating/
;;   : git pull octopress master     # Get the latest Octopress
;;   : bundle install                # Keep gems updated
;;   : rake update_source            # update the template's source
;;   : rake update_style             # update the template's style
;; * 手順
;;   1. ~/share/org/octopress/blog (YYYY-MM-DD-title.org)
;;      Blogの原稿が入ったディレクトリ
;;   2. ~/cvs/octopress/source_posts (YYYY-MM-DD-title.html YAML)
;;      M-x org-publish によって変換されるディレクトリ
;;   3. ~/cvs/octopress/public/blog (YYYY-MM-DD-title.html)
;;      rake generate によって生成されるコンテンツ。
;;   4. git commit/push

;;  タイムスタンプは ~/.org-timestamps にあるのでうまく動かない場合は削除する。

(use-package org-octopress :no-require t :ensure t
  :defines (org-publish-project-alist)
  :commands (org-octopress)
  :config
  ;; オリジナルファイルのディレクトリ
  (set-variable 'org-octopress-directory-org-top "~/share/org/octopress")
  (set-variable 'org-octopress-directory-org-posts "~/share/org/octopress/blog")
  (set-variable 'org-octopress-setup-file "~/share/org/octopress.org")
  ;; export先のディレクトリ
  (set-variable 'org-octopress-directory-top "~/share/cvs/octopress/source")
  (set-variable 'org-octopress-directory-posts "~/share/cvs/octopress/source/_posts")
  ;; octopress-static も追加する。
  (add-hook
   'org-octopress-summary-mode-hook
   (lambda ()
     (setf (alist-get "octopress" org-publish-project-alist)
           '(:components ("octopress-posts" "octopress-org" "octopress-static"))))))
;; 残骸ノート（以下の情報はメモ）
;; 一時期、org-mac-linkパッケージの不在による org-octopressインストール失敗があったため、
;; 以下のようにして対処した。
;; org-octopress -> orglue -> org-mac-link への対応 (org-mac-link-grabbar が消滅)
;; 　初期化時のエラーメッセージが消えたら対処完了
;;(when (locate-library "org-mac-link")
;;  (add-to-list 'package--builtins
;;               '(org-mac-link . [(1 2) nil "Grab links and url from various mac"])))

;;;; org-table-comment
;; orgtbl-mode では対応できない 非ブロックコメント形式のプログラム言語
;; でのテーブル入力を実現。
;; コメントヘッダを入力後、M-x orgtbl-comment-mode
(use-package org-table-comment :no-require t :ensure t
  :bind ("C-c |" . orgtbl-comment-mode))

;;;; org-toodledo
;; 要 w3m。http-post-simple.el
(use-package org-toodledo :no-require t :ensure t
  :defines (org-toodledo-userid)
  :commands (org-toodledo-initialize)
  :config
  (set-variable 'org-toodledo-file "~/share/org/toodledo.org")
  ;; deferred error : (buffer-read-only #<buffer toodledo.org<org>>)
  ;; と出たら注意。
  (add-hook 'org-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-M-o"))
              (local-set-key (kbd "C-M-o d") 'org-toodledo-mark-task-deleted)
              (local-set-key (kbd "C-M-o s") 'org-toodledo-sync)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-M-o"))
              (local-set-key (kbd "C-M-o d") 'org-toodledo-agenda-mark-task-deleted)))
  ;; UserID & password
  (set-variable 'org-toodledo-userid "td525154436b560")
  (let ((secret (plist-get (nth 0 (auth-source-search :host "www.toodledo.com"
                                                      :user org-toodledo-userid))
                           :secret)))
    (if (functionp secret)
        (set-variable 'org-toodledo-password (funcall secret))
      (error "ToodleDo Passowrd not set!")))
  ;; Useful key bindings for org-mode
  ;; 自動保存
  (set-variable 'org-toodledo-sync-on-save "yes") ; 慣れたら "yes" に変更する。
  ;; フォルダをヘッディングとして認識。
  (set-variable 'org-toodledo-folder-support-mode 'heading)
  ;; スマホで設定した新しいタスクを、"ToodledoLastSync" にsyncする。
  ;; (set-variable 'org-toodledo-sync-import-new-tasks t) ; default
  ;; キー設定。 C-M-o d でタスク削除。
  )

;;;; osx-plist (obsolete)
;; MacOS 10.6 から environment.plist が未サポートになり利用中止。

;;;; ox-gfm
;; → pandoc へ移行。
;; http://orgmode.org/cgit.cgi/org-mode.git/plain/contrib/lisp/ox-gfm.el
;;(use-package ox-gfm :defer t
;;  :init
;;  (with-eval-after-load 'ox
;;    (require 'ox-gfm nil t)))

;;;; ox-hatena
;; https://github.com/akisute3/ox-hatena/
(use-package ox-hatena :no-require t
  :commands (org-hatena-export-as-hatena org-hatena-export-to-hatena))

;;;; osx-osascript (obsolete)
;; → mac-osa-script へ移行。
;;(when (locate-library "osx-osascript")
;;  (autoload 'osascript-run-str "osx-osascript" "Run the osascript STR."))

;;;; osx-org-clock-menubar
(use-package osx-org-clock-menubar :no-require t :defer t :ensure t)

;;;; paradox
(use-package paradox :no-require t :ensure t
  :bind ("C-c P" . paradox-list-packages)
  :config
  (pallet-init)
  (set-variable 'paradox-github-token
                (let ((secret
                       (plist-get (nth 0 (auth-source-search
                                          :host "paradox.kawabata.github.com"
                                          :user "kawabata"))
                                  :secret)))
                  (if (functionp secret) (funcall secret)
                    (error "Paradox Token not set!")))))

;;;; phi-rectangle
(use-package phi-rectangle :no-require t :ensure t
  :bind (("C-x r C-@" . rm-set-mark)
         ("C-x r C-\\" . rm-set-mark)
         ("C-x r C-x" . rm-exchange-point-and-mark)
         ("C-x r C-w" . rm-kill-region)
         ("C-x r M-w" . rm-kill-ring-save)))

;;;; ProofGeneral
;; http://proofgeneral.inf.ed.ac.uk/
;; - cf. [[info:ProofGeneral#Top]]
;; ProofGeneralの起動は時間がかかるので、下記の工夫で
;; 遅延読み込みを実現する。各モードは proof-site により設定される。
;; - C-c C-n :: 階梯を次に進む
;; - C-c C-u :: 階梯を前に戻る (proof-undo-last-successful-command)
;; tactics
;; - intros, exact, etc. (http://coq.inria.fr/refman/tactic-index.html)
;; tutoriasl
;; - Coq Cheetsheet :: https://gist.github.com/qnighy/4465660
;; - Tutorial :: http://www.slideshare.net/tmiya/coq-tutorial
;; - Tutorial :: http://alohakun.blog7.fc2.com/blog-entry-271.html
;; - まとめ :: http://www39.atwiki.jp/fm-forum/m/pages/17.html
;; Proof Tree
;; - http://askra.de/software/prooftree/
(use-package proof-site :no-require t
  :commands (tkw-proof-general-version
             pghaskell-mode pgocaml-mode
             pgshell-mode phox-mode coq-mode isar-mode)
  :mode (("\\.pghci\\'" . pghaskell-mode)
         ("\\.pgml\\'" . pgocaml-mode)
         ("\\.pgsh\\'" . pgshell-mode)
         ("\\.phx\\'" . phox-mode)
         ("\\.v\\'" . coq-mode)
         ("\\.thy\\'" . isar-mode))
  :defines (proof-general-version)
  :config
  (defun tkw-proof-general-version ()
    (interactive)
    (message "%s" proof-general-version))
  ;; 利用中にエラーが起こるので一旦 nilに。
  ;; (proof-unicode-tokens-set-global t)
  )

;;;; riece
;;;;; memo
;; doc: http://www.nongnu.org/riece/riece-ja/Commands.html#Commands
;; |         | erc     | riece     |
;; |---------+---------+-----------|
;; | join    | C-c C-j | C-c j     |
;; | part    | C-c C-p | C-c C-p   |
;; | quit    | C-c C-q | C-c q     |
;; | notice  |         | C-RET     |
;; | privmsg |         | C-c p     |
;; | nick    |         | C-c n     |
;; | whois   |         | C-c f     |
;; | kick    |         | C-c C-k   |
;; | invite  |         | C-c i     |
;; | list    |         | C-c l     |
;; | names   | C-c C-n | C-c C-n   |
;; | who     |         | C-c w     |
;; | topic   | C-c C-t | C-c t     |
;; | mode    |         | C-c C-m   |
;; | mode +o |         | C-c o     |
;; | mode -o |         | C-c C-o   |
;; | mode +v |         | C-c v     |
;; | mode -v |         | C-c C-v   |
;; | away    |         | C-c C-t a |
;; |---------+---------+-----------|
;; | any     |         | C-c /     |
;; 上記のコマンドを特定のIRCサーバへ向けて発出するには、C-c M <server> prefix を使う。
;; 特定のサーバを開いたり閉じたりするのは C-c O または C-c C.
;; 次のチャンネルへ移動 ・C-c >
;; 前のチャンネルへ移動 ・C-c <
;; 以下の変数は ~/.riece/init に記述する。
;; - riece-server-alist
;; - riece-server
;; - riece-startup-server-list
;; - riece-startup-channel-list
;;;;; riece/riece
(use-package riece :no-require t
  :if (file-exists-p "~/.riece/init")
  :bind ("C-c R" . riece)
  :config
  (set-variable 'riece-debug t)
  ;; addon は riece-XXXX-enable で起動
  (set-variable
   'riece-addons
   '(riece-highlight
     riece-ctcp
     riece-url            ; URL収集
     riece-guess
     riece-unread         ; 発言あったチャネルに印
     riece-ndcc           ; ファイル転送
     ;; riece-mini
     riece-log
     ;; riece-doctor
     riece-alias
     ;; riece-skk-kakutei ; △などの印の除去
     ;; riece-foolproof
     ;; riece-guess
     riece-history
     riece-button
     riece-keyword
     riece-menu
     ;; riece-async
     ;; riece-lsdb
     ;; riece-xface
     riece-ctlseq
     riece-ignore
     ;; riece-hangman
     riece-biff
     ;; riece-kakasi
     ;; riece-yank
     riece-toolbar
     ;; riece-eval
     ;; riece-google
     riece-keepalive
     ;; riece-eval-ruby
     riece-icon
     riece-shrink-buffer
     riece-mcat))
  (with-eval-after-load 'riece-log
    (set-variable 'riece-log-coding-system 'utf-8-emacs)))

;;;; rcodetools <gem>
;; コードの補完など
;; * 導入（elファイルも含む）
;;   : % gem install rcodetools
;;(when (locate-library "rcodetools")
;;  (eval-after-load 'ruby-mode
;;    '(progn (require 'rcodetools)
;;            (require 'anything-rcodetools))))

;;;; restclient
;; cf. http://emacsrocks.com/e15.html

;;;; ox-reveal
;; require すれば自動的に利用可能。
;; TODO サブノードを export する方法
;; #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/2.6.2/
(use-package ox-reveal :no-require t :defer t :ensure t
  :config
  (set-variable 'org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.6.2/")
  (set-variable 'org-reveal-hlevel 2))

;;;; pdf-tools
;; https://github.com/politza/pdf-tools (本家)
;; https://github.com/axot/pdf-tools (MacOS対応版)
;; * Macでのインストール方法
;; % brew install poppler (うまくいかなければ --with-glib を付けてみる)
;; % brew install zlib # keg only
;; % export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
;; % cd （パッケージの server ディレクトリ）
;; % make
;;
;; 生成された 'epdfinfo' は、/usr/local/bin あたりに動かして、
;; pdf-info-epdfinfo-program をそれに合わせてやれば、異なるアーキテク
;; チャでの共有時に混乱がない。
;;
;; * 搭載機能
;;  - pdf-history-minor-mode :: B/N キーでヒストリを移動。
;;  - pdf-isearch-minor-mode
;;  - pdf-links-minor-mode
;;  - pdf-misc-minor-mode :: C-w コピー、I メタデータ、s p (crop
;;    page), s w (crop window) C-c C-d (dark mode) M-s o (pdf-occur)
;;  - pdf-outline-minor-mode
;;  - pdf-misc-size-indication-minor-mode
;;  - pdf-misc-menu-bar-minor-mode
;;  - pdf-misc-tool-bar-minor-mode
;;  - pdf-annot-minor-mode
;;  - pdf-sync-minor-mode
;;  - pdf-misc-context-menu-minor-mode :: マウスボタン３でメニュー表示
;;  - pdf-info-auto-revert-minor-mode
(use-package pdf-tools :no-require t :defer t :ensure t
  :if (executable-find "epdfinfo")
  :init
  ;;(with-eval-after-load 'doc-view
  ;;  (pdf-tools-install)) ; doc-view-mode-hook に pdf-tools-enable を設定。
  :config
  ;; ELPAディレクトリをDropboxで共有している場合、そこに含まれるepdfinfoは、
  ;; 異なるプラットフォームでは動作しない。
  (with-eval-after-load 'pdf-info
    (set-variable 'pdf-info-epdfinfo-program (executable-find "epdfinfo")))
  ;;(define-key pdf-occur-buffer-mode-map (kbd "o") 'pdf-occur-view-occurrence)
  )

;;;; phi-rectangle
;; - autoload :: phi-rectangle-mode

;;;; pianobar (abstain)
;; 音楽配信サイト pandora のクライアント
;; 現在はアメリカからのみ利用可能。

;;;; popup-kill-ring (abstain)
;; M-x popup-kill-ring

;;;; popup-switcher
;; ミニバッファに視点を移さずにファイルやバッファを選択。
;; M-x psw-switch-recentf
;; M-x psw-switch-buffer
(use-package popup-switcher :no-require t :ensure t
  :bind ("C-x M-b" . psw-switch-buffer))

;;;; projectile
;; https://github.com/bbatsov/projectile
;; .git, .hg, .bzr ディレクトリ単位でファイル探索等をサポート
;; 対応するプロジェクト
;; - lein :: clojure
;; - maven :: java
;; - sbt :: scala
;; - scons (python)
;; - rebar :: erlang
;; - bundler :: ruby
;; projectile-file のコマンド一覧
;; （他にも便利な命令があるので、 C-c p <help> で確認）
;; C-c p f	Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p d	Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p T	Display a list of all test files(specs, features, etc) in the project.
;; C-c p l	Display a list of all files in a directory (that's not necessarily a project)
;; C-c p g	Run grep on the files in the project.
;; C-c p b	Display a list of all project buffers currently open.
;; C-c p o	Runs multi-occur on all project buffers currently open.
;; C-c p r	Runs interactive query-replace on all files in the projects.
;; C-c p i	Invalidates the project cache (if existing).
;; C-c p R	Regenerates the projects TAGS file.
;; C-c p k	Kills all project buffers.
;; C-c p D	Opens the root of the project in dired.
;; C-c p e	Shows a list of recently visited project files.
;; C-c p a	Runs ack on the project. Requires the presence of ack-and-a-half.
;; C-c p A	Runs ag on the project. Requires the presence of ag.el.
;; C-c p c	Runs a standard compilation command for your type of project.
;; C-c p p	Runs a standard test command for your type of project.
;; C-c p z	Adds the currently visited to the cache.
;; C-c p s	Display a list of known projects you can switch to.
(use-package projectile :no-require t :defer t :ensure t
  :config
  (projectile-global-mode)
  (set-variable 'projectile-enable-caching t)) ; ファイル検索を高速化

;;;; quickref
;; prefix = "C-c q"
;; C-c q e   :: 'quickref-in-echo-area
;; C-c q w   :: 'quickref-in-window
;; C-c q 0   :: 'quickref-dismiss-window
;; C-c q a   :: 'quickref-add-note
;; C-c q d   :: 'quickref-delete-note
;; C-c q v   :: 'quickref-describe-refs
;; C-c q C-s :: 'quickref-write-save-file
;; C-c q C-l :: 'quickref-load-save-file
(use-package quickref :no-require t :defer t :ensure t
  :config
  (quickref-global-mode 1))

;;;; quickrun
;; - https://github.com/syohex/emacs-quickrun
(use-package quickrun :no-require t :ensure t
  :bind ("C-x '" . quickrun)
  :config
  (quickrun-add-command "html"
                        '((:command . "chromium-browser")
                          (:exec    . "%c %s"))
                        :mode 'web-mode)
  (quickrun-add-command "html"
                        '((:command . "chromium-browser")
                          (:exec    . "%c %s"))
                        :mode 'html-mode))

;;;; rect-mark (obsolete)
;; 矩形選択を便利にする
;; phi-rectangle に移行。

;;;; realgud
;; GUD rennovated.
;; |---------------------+----------------------|
;; | Ruby 1.9 trepanning | M-x realgud-trepan   |
;; | Rubinius trepanning | M-x realgud-trepanx  |
;; | Bash                | M-x realgud-bashdb   |
;; | Z-shell             | M-x realgud-zshdb    |
;; | Korn Shell          | M-x realgud-kshdb    |
;; | Stock Python        | M-x realgud-pdb      |
;; | Stock Perl          | M-x realgud-perl     |
;; | Trepan Perl         | M-x realgud-trepanpl |
;; | Ruby-debug          | M-x realgud-rdebug   |
;; | GNU Make            | M-x realgud-remake   |
;; | Python pydbgr       | M-x realgud-pydbgr   |
;; | GDB                 | M-x realgud-gdb      |
;; |---------------------+----------------------|
;; 別途必要なライブラリ ::
;; - loc-changes <elpa>   (http://github.com/rocky/emacs-loc-changes)
;; - load-relative <elpa> (http://github.com/rocky/emacs-load-relative)
;; - test-simple <elpa>   (http://github.com/rocky/emacs-test-simple)
;; Ruby のデバッグには、gem install byebug を実行して、rdebugが入ってい
;; ることを確認した後でrealgud-rdebug を実行する。gdb はまだ本家のgudが便利か。
(use-package realgud :no-require t :ensure t
  :commands (realgud-bashdb realgud-gdb realgud-gub realgud-pdb
             realgud-perldb realgud-pydb realgud-remake realgud-rdebug
             realgud-zsh))

;;;; rebox2
;; http://www.youtube.com/watch?v=53YeTdVtDkU
;; http://www.emacswiki.org/emacs/rebox2
(use-package rebox2 :no-require t :ensure t
  :bind ("S-C-q" . rebox-dwim)
  :config
  (set-variable 'rebox-style-loop '(16 21 24 25 27)))

;;;; recursive-narrow (obsolete)
;; → wide-n.el に移行。
;;(lazyload (recursive-narrow-to-region recursive-widen
;;           (bind-key "C-x n n" 'recursive-narrow-to-region)
;;           (bind-key "C-x n w" 'recursive-widen))
;;  "recursive-narrow")

;;;; replace-colorthemes (abstain)
;; https://github.com/emacs-jp/replace-colorthemes
;;(let ((replace-colorthemes
;;       (locate-user-emacs-file "themes/replace-colorthemes")))
;;  (when (file-directory-p replace-colorthemes)
;;    (add-to-list 'custom-theme-load-path replace-colorthemes)))

;;;; sense-region (obsolete)
;; cua-mode で代替可能
;;(when (locate-library "sense-region")
;;  (autoload 'sense-region-toggle "sense-region"))

;;;; session
;; http://emacs-session.sourceforge.net/
;; 終了時にセッション関連変数を保存
;; 標準の"desktop"（バッファリストなどを保存）と併用。
;(use-package session :defer t
;  ;; :if (file-writable-p "~/.session")
;  :init
;  (add-hook 'after-init-hook 'session-initialize)
;  :config
;  (set-variable 'history-length t)
;  ;; 以下を設定すると、custom.el で強制的に session.el を読みこまされ、
;  ;; 結果として session.el がない環境でエラーになる。
;  ;; (session-initialize-and-set 'session-use-package t)
;  (setq session-globals-max-string 10240
;        session-registers-max-string 10240
;        session-use-package t
;        session-initialize '(de-saveplace session keys menus places)
;        session-globals-include '((kill-ring 50)
;                                  (session-file-alist 500 t)
;                                  (file-name-history 10000))
;        session-globals-exclude '(file-name-history load-history register-alist
;                                  vc-comment-ring flyspell-auto-correct-ring))
;  (add-hook 'after-init-hook 'session-initialize)
;  ;; 前回閉じたときの位置にカーソルを復帰
;  ;;(setq desktop-globals-to-save '(desktop-missing-file-warning))
;  (set-variable 'session-undo-check -1))

;;;; setup-cygwin
;; Windows用のシンボリックリンクの設定など
;(when (equal system-type 'windows-nt)
;  (require 'setup-cygwin nil :no-error))

;;;; shell-pop
;; 【注意】 shell-pop--cd-to-cwd-term の
;; (term-send-raw-string "\C-l") はコメントアウトすること。
(use-package shell-pop :no-require t :ensure t
  :bind ("M-c" . shell-pop)
  :config
  ;;(shell-pop--set-shell-type 'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  ;;(shell-pop--set-shell-type 'shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  ;;(shell-pop--set-shell-type 'shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
  (set-variable 'shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
  (when (executable-find "/bin/zsh")
    (set-variable 'shell-pop-term-shell "/bin/zsh"))
  (set-variable 'shell-pop-universal-key "M-c"))

;;;; shell-toggle (obsolete)
;; → shell-pop に移行。
;; パッチがあたった version 1.3 以降を使うこと。
;; http://www-verimag.imag.fr/~moy/emacs/shell-toggle-patched.el
;; 最後の行の (provide 'shell-toggle-patched) から "-patched) を削除する。
;; (lazyload (shell-toggle shell-toggle-cd
;;            (bind-key "M-c" 'shell-toggle-cd)) "shell-toggle"
;;   (set-variable 'shell-toggle-launch-shell 'shell-toggle-eshell)) ; 'shell-toggle-ansi-term

;;;; smartrep
;; 例：C-c C-n の繰り返しを、C-c C-n C-n ... ですませられるようにする。
(use-package smartrep :no-require t :ensure t
  :commands (smartrep-define-key)
  :init
  (when (boundp 'winner-mode-map)
    (smartrep-define-key winner-mode-map "C-c"
                         '(("M-p" . winner-undo)
                           ("M-n" . winner-redo))))
  ;; テーマの回転 (obsolete)
  ;;(smartrep-define-rotate-key tkw-rotate-map "t"
  ;;  (rotate-theme) (rotate-theme -1))
  (with-eval-after-load 'outline
    (smartrep-define-key
        outline-minor-mode-map "C-c"
      '(("C-n" . outline-next-visible-heading)
        ("C-p" . outline-previous-visible-heading))))
  (with-eval-after-load 'org
    (smartrep-define-key
        org-mode-map "C-c"
      '(("C-n" . outline-next-visible-heading)
        ("C-p" . outline-previous-visible-heading)))))

;;(declare-function smartrep-define-key "smartrep" (keymap prefix functions))
;;(defmacro smartrep-define-rotate-key (keymap prefix function-1 function-2)
;;  (declare (indent 2))
;;  `(smartrep-define-key
;;       ,keymap ,prefix
;;     '(("n" . ,function-1)
;;       ("p" . ,function-2))))

;;;; smex (abstain)
;; Smart Meta-X
;;(when (require 'smex nil :no-error)
;;  (smex-initialize)
;;  (bind-key "M-x" 'smex)
;;  (bind-key "M-X" 'smex-major-mode-commands)
;;  ;; This is your old M-x.
;;  (bind-key "C-c C-c M-x" 'execute-extended-command))

;;;; sml-modeline (abstain)
;;(when (require 'sml-modeline nil :no-error)
;;  (sml-modeline-mode t)
;;  (if (functionp 'scroll-bar-mode)
;;      (scroll-bar-mode nil))
;;  )

;;;; smooth-scrolling (abstain)
;; 重要な関数にアドバイスするのでインストール・使用中止。

;;;; sokoban (marmalade)
;; M-x sokoban game


;;;; sorter (obsolete)
;; "s" キーで、ファイル名・サイズ・日付・拡張子名順に並び替え。
;; dired-listing-switches が、"al" 以外だと動作しないため使用中止。
;; 独自に移植。 (dired-rotate-sort) 参照。

;;;; sublimity (abstain)
;; 動作が重くなるので利用中止。
;; sublimity-scroll sublimity-map

;;;; sudden-death
;; ＿人人人人人人＿
;; ＞　突然の死　＜
;; ￣ＹＹＹＹＹＹ￣
(use-package sudden-death :no-require t :ensure t
  :commands sudden-death)

;;;; sunrise-commander (abstain)
;; Two-pane file manager for Emacs based on Dired and inspired by MC
;; http://www.emacswiki.org/emacs/Sunrise_Commander
;; M-x sunrise

;;;; swoop
;; swoop, swoop-multi, swoop-pcre-regexp, swoop-migemo
(use-package swoop :no-require t :defer t :ensure t
  :init
  (define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch))

;;;; symon
;; https://github.com/zk-phi/symon
;; M-x symon-mode, symon-display で表示
(use-package symon :no-require t :defer t :ensure t
  :if (file-directory-p "/proc"))

;;;; TRR
;; https://code.google.com/p/trr22
(use-package trr :no-require t
  :commands trr)

;;;; text-adjust
;; 1) M-x text-adjust を実行すると文章が整形される.
;; 2) 使用可能な関数の概要.
;;     text-adjust-codecheck : 半角カナ, 規格外文字を「〓」に置き換える.
;;     text-adjust-hankaku   : 全角英数文字を半角にする.
;;     text-adjust-kutouten  : 句読点を「, 」「. 」に置き換える.
;;     text-adjust-space     : 全角文字と半角文字の間に空白を入れる.
;;     text-adjust           : これらをすべて実行する.
;;     text-adjust-fill      : 句読点優先で, fill-region をする.
;; Invalid function: define-obsolete-function-alias とエラーが出るので
;; コメントアウト
;;(lazyload (text-adjust-buffer text-adjust-region text-adjust) "text-adjust"
;;  (set-variable 'text-adjust-rule-kutouten text-adjust-rule-kutouten-zkuten))

;;;; twittering-mode
;; 使い方はマニュアル（http://www.emacswiki.org/emacs/TwitteringMode）を参照。
;; - 基本的な使い方
;;   M-x twit (autoload) で開始
;;   M-x twittering-icon-mode でアイコン表示
;;   M-x twittring-toggle-proxy でProxy使用
;; - タイムラインの開き方：
;;   "V" + spec (:home, :mentions, etc..)
;;   u (C-c C-s) で新規投稿。
;;   C-m リプライ
;; - 投稿
;;   ‘M-p’ 一つ前のツイートを辿る。(‘twittering-edit-previous-history’)
;;   ‘M-n’ 一つ次のツイートを辿る。(‘twittering-edit-next-history’)
;;   `<f4>’ カーソル下のURLを短縮URLに置換する。(‘twittering-edit-replace-at-point’)
;;   ‘C-c C-k’ 現在のツイートを消去する。(‘twittering-edit-cancel-status’)
;;   ‘C-c C-c’ 現在のツイートを送信する。(‘twittering-edit-post-status’)
;; - 投稿の削除
;;   + "C-c C-w"
;; - 投稿のリツイート
;;   + "C-u C-c RET" :: 公式
;;   + "C-c RET" :: 引用式
;; - ダイレクトメッセージ
;;   + d

;; RET     twittering-enter
;; C-v     twittering-scroll-up
;; ESC     Prefix Command
;; SPC     twittering-scroll-up
;; $       end-of-line
;; 0       beginning-of-line
;; F       twittering-friends-timeline
;; G       twittering-goto-last-status
;; H       twittering-goto-first-status
;; L       twittering-other-user-list-interactive
;; R       twittering-replies-timeline
;; U       twittering-user-timeline
;; V       twittering-visit-timeline
;; W       twittering-update-status-interactive
;; ^       beginning-of-line-text
;; a       twittering-toggle-activate-buffer
;; b       twittering-switch-to-previous-timeline
;; d*      twittering-direct-message
;; f       twittering-switch-to-next-timeline
;; g       twittering-current-timeline
;; h       backward-char
;; i       twittering-icon-mode
;; j       twittering-goto-next-status
;; k       twittering-goto-previous-status
;; l       forward-char
;; n       twittering-goto-next-status-of-user
;; p       twittering-goto-previous-status-of-user
;; q       twittering-kill-buffer
;; r       twittering-toggle-show-replied-statuses
;; t       twittering-toggle-proxy
;; u       twittering-update-status-interactive
;; v       twittering-other-user-timeline

;; C-M-i   twittering-goto-previous-thing
;; M-v     twittering-scroll-down

;; C-c C-d twittering-direct-messages-timeline
;; C-c C-e twittering-erase-old-statuses
;; C-c C-f twittering-friends-timeline
;; C-c C-l twittering-update-lambda
;; C-c RET twittering-retweet
;; C-c C-p twittering-toggle-proxy
;; C-c C-q twittering-search
;; C-c C-r twittering-replies-timeline
;; C-c C-s twittering-update-status-interactive
;; C-c C-t twittering-set-current-hashtag
;; C-c C-u twittering-user-timeline
;; C-c C-v twittering-view-user-page
;; C-c C-w twittering-delete-status
;; (C-c D   twittering-delete-status → これはルール違反)
;; Timeline-Spec
;; http://www.emacswiki.org/emacs/TwitteringMode-ja#toc17 参照

;; <RET> … reply
(use-package twittering-mode :no-require t :defer t :ensure t
  :functions (twittering-toggle-proxy)
  :defines (twittering-mode-map)
  :config
  ;; personal settings
  (set-variable 'twittering-username "kawabata")
  ;; general settings
  (set-variable 'twittering-icon-mode t)
  (set-variable 'twittering-jojo-mode t)
  ;;(add-hook 'twittering-mode-init-hook
  ;;          'twittering-icon-mode)
  ;;(add-hook 'twittering-mode-init-hook
  ;;          'twittering-jojo-mode)
  (set-variable 'twittering-timer-interval 6000)
  (set-variable 'twittering-use-master-password t)
  (when (getenv "https_proxy")
    ;; (set-variable 'twittering-proxy-use t)
    (twittering-toggle-proxy))
  ;; Timeline Spec
  ;; search API :: https://dev.twitter.com/docs/using-search
  (set-variable 'twittering-timeline-spec-alias
                '(("FRIENDS" . "my-account/friends-list")
                  ("related-to" .
                   (lambda (username)
                     (if username
                         (format ":search/to:%s OR from:%s OR @%s/"
                                 username username username)
                       ":home")))
                  ("related-to-twitter" . "$related-to(twitter)")))
  ;; initial timeline
  (set-variable 'twittering-initial-timeline-spec-string
                '(":home"
                  ":replies"
                  ":mentions"
                  ":favorites"
                  ":direct_messages"
                  ":search/emacs/"
                  "melpa_emacs"
                  ;;"kawabata/kanji"
                  ))
  ;; Shortcut
  (mapc (lambda (pair)
          (define-key twittering-mode-map (kbd (car pair)) (cdr pair)))
        '(("F" . twittering-friends-timeline)
          ("R" . twittering-replies-timeline)
          ("U" . twittering-user-timeline)
          ("W" . twittering-update-status-interactive)
          ("C-c f" . twittering-follow)
          ("C-c u" . twittering-unfollow))))
;; (set-variable 'twittering-password "Twitterのパスワード")

;;;; undo-tree
;; C-/ : undo
;; C-? : redo
;; C-x u : show tree (p/n/f/b)
(use-package undo-tree :no-require t :defer t :ensure t
  :config
  (set-variable 'undo-tree-mode-lighter
        (if (eq window-system 'mac) "🌲" "木"))
  (global-undo-tree-mode))

;;;; undohist
;; 拡張子が .gpg のファイルの undohist を保存する際、この拡張子で暗号化
;; ファイルと誤認するが、その際、epa-file-encrypt-to がバッファローカル変数に
;; ならないので、変なダイアログが出てしまうので利用中止。
;;(use-package undohist :ensure t
;;  :config
;;  (undohist-initialize)
;;  (set-variable 'undohist-ignored-files '("\\.gpg$"))
;;  )

;;;; unicode-fonts
;; M-x unicode-fonts-setup
;;(lazyload () "unicode-fonts"
;;  (defun tkw-push-font (range &rest fonts)
;;    (let ((default-fonts (car (alist-get range unicode-fonts-block-font-mapping))))
;;      (dolist (font fonts)
;;        (nconc default-fonts (list font)))
;;      (message "default-fonts=%s" default-fonts)
;;      (setf (alist-get range unicode-fonts-block-font-mapping)
;;            (list default-fonts))))
;;  (tkw-push-font "CJK Unified Ideographs" "Hiragino Mincho Pro"))

;;;; w3m
;; eww へ移行する。（一部ライブラリで使用するので、obsoleteにはしない。）
(use-package w3m :no-require t :defer t :ensure t
  :defines (w3m-compatible-encoding-alist)
  :if (executable-find "w3m")
  :config
  (when (coding-system-p 'cp51932)
    (pushnew '(euc-jp . cp51932) w3m-compatible-encoding-alist)))

;;;; wget (obsolete)

;;;; wgrep
;; You can edit the text in the *grep* buffer after typing C-c C-p.
;; After that the changed text is highlighted.
;; The following keybindings are defined:

;; C-c C-e : Apply the changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-d : Mark as delete to current line (including newline).
;; C-c C-r : Remove the changes in the region (these changes are not
;;           applied to the files. Of course, the remaining
;;           changes can still be applied to the files.)
;; C-c C-p : Toggle read-only area.
;; C-c C-k : Discard all changes and exit.
;; C-x C-q : Exit wgrep mode.
;; 実行前に turn-off-old-file-read-only の実行を推奨。
;;
;; - autoload :: (add-hook grep-setup-hook 'wgrep-setup)
(use-package wgrep :no-require t :defer t :ensure t)

;;;; wgrep-ag
(use-package wgrep-ag :no-require t :defer t :ensure t
  :init
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

;;;; wide-n
;; melpaから削除
;; ナローイングの履歴を、 wide-n-restrictions に入れることで記録するツール。
;; `C-x n n', `C-x n w' を更新。
;;(use-package wide-n :no-require t :defer t :ensure t)

;;;; zeal-at-point
;; http://zealdocs.org/
;; dash 型統合ドキュメントセンタ。
(use-package zeal-at-point :no-require t :defer t :ensure t
  :if (executable-find "zeal"))

;;;; zossima
;; Ruby で定義先メソッドへジャンプ
(use-package zossima :no-require t :defer t :ensure t
  :init
  (add-hook 'ruby-mode-hook 'zossima-mode))

;;;; zotelo
;; not zotero!
;; C-c z prefix.
(use-package zotelo :no-require t :defer t :ensure t
  :init
  (add-hook 'TeX-mode-hook 'zotelo-minor-mode))

;;; 個人用アプリケーション
;;;; aozora-proc
(use-package aozora-proc :no-require t
  :commands (aozora-proc aozora-proc-region aozora-proc-buffer))

;;;; aozora-view
(use-package aozora-view :no-require t
  :commands (aozora-view))

;;;; aozora-yasnippets
;;(use-package yasnippet
;;  :defer t :config
;;  (when (file-directory-p "~/.emacs.d/snippets/aozora-yasnippets")
;;    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/aozora-yasnippets")
;;    (defun ruby-clean-up ()
;;      (when (looking-back "\\\cC\\(.+\\)《.+\\(\\1\\)》")
;;        (insert (match-string 1))
;;        (delete-region (match-beginning 2) (match-end 2))
;;        (delete-region (match-beginning 1) (match-end 1))))
;;    (add-hook 'yas-after-exit-snippet-hook 'ruby-clean-up)))

;;;; asn1-mode
(use-package asn1-mode :no-require t
  :mode (("\\.mo$" . asn1-mode)
         ("\\.asn1$" . asn1-mode)
         ("\\.gdmo$" . asn1-mode))
  :config
  (add-hook 'asn1-mode-hook
            (lambda ()
              (set-variable 'tab-width 4)
              (set-variable 'indent-tabs-mode t))))

;;;; bbdb-export
;; bbdb-export is a versatille BBDB export support.
;; ファイルの輸出先：
;;   https://www.icloud.com/#contacts
(use-package bbdb-export :no-require t
  :bind ("C-c V" . bbdb-export-vcard-v3)
  ;;:init
  ;;(add-hook 'bbdb-after-save-hook 'bbdb-export-vcard-v3)
  :config
  (when (file-directory-p "~/Desktop")
    (set-variable 'bbdb-export-file "~/Desktop/bbdb.vcf")))

;;;; bib-cinii
;; BibTeX Cinii検索
(use-package bib-cinii :no-require t
  :commands (bib-cinii-bib-buffer))

;;;; bib-ndl
;; BibTeX 国会図書館検索
(use-package bib-ndl :no-require t
  :commands (bib-ndl-bib-buffer))

;;;; display-theme
(use-package display-theme :no-require t :defer t
  :if window-system
  :config
  (display-theme-mode))

;;;; helm-chrome
;; - autoload :: helm-chrome-bookmarks
(use-package helm-chrome :no-require t :defer t)

;;;; hz2py
(use-package hz2py :no-require t
  :commands (hz2py-region))

;;;; ids-edit
(declare-function global-ids-edit-mode "ids-edit")
(use-package ids-edit :no-require t
  :bind (("M-U" . ids-edit))
  :config
  (global-ids-edit-mode))

;;;; init-checker
(use-package init-checker :no-require t
  :commands (init-checker init-checker-generate-cask)
  :config
  (set-variable 'init-checker-files
                (list user-init-file
                      (expand-file-name "gnus.el" user-emacs-directory))))

;;;; ivariants
(use-package ivariants :no-require t
  :init
  (with-eval-after-load 'ivariants
    (require 'ivariants-browse))
  :bind (("M-I" . ivariants-insert)
         ("C-c i" . ivariants-browse)))

;;;; ivs-edit
(use-package ivs-edit :no-require t
  :bind ("M-J" . ivs-edit))

;;;; japanese-holidays
;; cf. [[info:emacs#Holiday Customizing]]
;; calendar-holidays のデータは、 displayed-year, displayed-month の
;; 変数を設定されて、 calendar-holiday-list 関数によってevalされて
;; チェックされる。
(use-package japanese-holidays :no-require t :defer t
  :defines (japanese-holidays)
  :init
  (with-eval-after-load 'holidays
    (require 'japanese-holidays))
  :config
  (set-variable 'calendar-holidays
                (append
                 japanese-holidays
                 ;; holiday-general-holidays   ; 米国休日
                 holiday-local-holidays
                 holiday-other-holidays
                 ;; holiday-christian-holidays
                 ;; holiday-hebrew-holidays
                 ;; holiday-islamic-holidays
                 ;; holiday-bahai-holidays
                 ;; holiday-oriental-holidays  ; 中国記念日
                 ;; holiday-solar-holidays
                 ))
    ;; 日曜日に色を付ける。
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))

;;;; math-symbols
(use-package math-symbols :no-require t :defer t
  :init
  (with-eval-after-load 'helm-config
    (bind-key "M" 'math-symbols-helm helm-command-map)))

;;;; next-bus
(use-package next-bus :no-require t
  :commands (next-bus-mode))

;;;; ox-pandoc
(use-package ox-pandoc :no-require t :defer t
  :init
  (with-eval-after-load 'ox
    (require 'ox-pandoc))
  :config
  (set-variable 'org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
  (set-variable 'org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))
  (pushnew '("p" "#+PANDOC_OPTIONS:" "") org-structure-template-alist)
  (defun tkw-markdown_github-after-processing-hook ()
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward " commonlisp" nil t) (replace-match " common-lisp" nil t)))
  (add-hook 'org-pandoc-after-processing-markdown_github-hook
            'tkw-markdown_github-after-processing-hook)
  (add-hook 'org-pandoc-before-processing-latex-hook
            (lambda ()
              (goto-char (point-min))
              (while (re-search-forward "#\\+LABEL: \\(.+\\)" nil t) (replace-match "\\\\label{\\1}" nil))
            ))
  )

(defun tkw-org-pandoc-reference-quote-buffer ()
  (interactive)
  (goto-char (point-min))
  ;;(or (search-forward "参考文献" nil t)
  ;;    (search-forward "References" nil t))
  (while (re-search-forward "\\(\\[[0-9]+\\]\\)" nil t)
         ;;(re-search-forward "\n\n\\(\\[[0-9a-zA-Z]+?\\]\\)" nil t)
    (replace-match "=\\1= ")))

;;;; rotate-fonts
(use-package rotate-fonts :no-require t
  :commands (rotate-fonts)
  :config
  (customize-set-variable
   'rotate-fonts-specs
   '((?l ("Inconsolata"
          "Source Code Pro"           ; Adobe
          "Monaco"
          "Monofur"
          "Droid Sans Mono"
          "DejaVu Sans Mono"
          "Anonymous Pro"
          "Consolas"                  ; Windows
          "Menlo"                     ; Macintosh
          "IPAMonaPGothic")
         (latin))
     (?k #1=(
             "Hiragino Kaku Gothic ProN" ;"ヒラギノ角ゴ Pro"
             "Hiragino Maru Gothic ProN" ;"ヒラギノ丸ゴ Pro"
             "Hiragino Mincho ProN"      ;"ヒラギノ明朝 Pro"
             "IPAMonaPGothic"            ;
             ;; "PMinIWA-HW-Md"             ;"IWAp明-中Plus"
             ;; "Iwata SeichouF Pro"        ;"I-OTF弘道軒清朝復刻版Pro B"
             ;; "g_comickoin\-Light"
             "Hanazono Mincho"
             "Hanazono Mincho A"
             "Hanazono Mincho B"
             "Hanazono Mincho A Regular"
             "Hanazono Mincho B Regular"
             "SimSun"
             ;; Windows
             "ＭＳ 明朝"
             "ＭＳ ゴシック"
             "小塚明朝 Pr6N"
             "花園明朝 A Regular"
             "花園明朝 B Regular"
             )
         (kana symbol))
     (?p ("EUDC2"
          "Hanazono Mincho A")
         ((#xf137 . #xf69d)))
     (?h #1#
         ((#x2e80 . #x2fff) (#x3400 . #x9fff)
          (#xf900 . #xfaff) (#x20000 . #x2fffd))))))

;;;; sync-env
(use-package sync-env :no-require t
  :commands (sync-env)
  :init
  (unless (executable-find "gpg")
    (setenv "PATH" nil)
    (sync-env))
  )

;;;; tkw-birthdays
;; tkw-friend-birthdays
;; bbdb-anniv.el に移行。
;;(with-eval-after-load 'calendar (load "my-birthdays" nil t))

;;;; view-dict-pdf
;; PDF辞書検索
(use-package view-dict-pdf :no-require t
  :bind ("M-S-d" . view-dict-at-point))

;;;; view-pdf
;; `browse-url-browser-function' 変数を拡張して、ローカルファイル上の
;; PDFを見れるようにする。lookup等で有用。
(use-package view-pdf :no-require t :defer t
  :init
  (autoload 'view-pdf "view-pdf"))

;;;; wolfram-mode
;; Wolfram Language
;; - 文法 ::
;;  http://reference.wolfram.com/mathematica/tutorial/OperatorInputForms.html
;; - コマンドラインオプション ::
;; http://reference.wolfram.com/mathematica/tutorial/MathematicaSessions.html
;; パッケージ自動読み込み
;; - (format "Block[{Short=Identity},Get[\"%s\"]]; SetOptions[$Output, PageWidth-> %d];" (emathica-comint-quote-filename file) (- (window-width) 1))
(defvar wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
(use-package wolfram-mode :no-require t
  :if (executable-find wolfram-program)
  :commands run-wolfram
  :mode (("\\.m$" . wolfram-mode)
         ("\\.nb$" . wolfram-mode)
         ("\\.cdf$" . wolfram-mode))
  :config
  ;;(remove-from-auto-mode-alist '("\\.m$" . wolfram-mode)) ;; Objective-C を扱う場合
  (set-variable 'wolfram-mode-program-arguments
        '("-run"
          "showit := Module[{}, Export[\"/tmp/math.jpg\",%, ImageSize->{800,600}]; Run[\"open /tmp/math.jpg&\"]]")))

;; Mathematicaプログラミングメモ
;; http://reference.wolfram.com/mathematica/tutorial/UsingATextBasedInterface.html
;; http://www.watson.org/~mccann/mathematica.el
;; Mathematica ノートブックは 標準UIを使うべきだが、
;; パッケージを書く場合は Emacs の方が便利。
;; http://stackoverflow.com/questions/6574710/integrating-notebooks-to-mathematicas-documentation-center
;; http://mathematica.stackexchange.com/questions/29324/creating-mathematica-packages
;; 「ライセンスが切れた」と表示される場合は、他プロセスを動かしていないか確認する。

;;;; zsh-history
(use-package zsh-history :no-require t :defer t
  :config
  (set-variable 'zsh-hist-merge-target-1 "~/.zsh_history")
  (set-variable 'zsh-hist-merge-target-2 "~/share/dotfiles/.zsh_history")
  (set-variable 'zsh-hist-merge-dest "~/.zsh_history_new"))

;;; Emacs 起動時に開くファイル
;; ファイルローカル変数値を優先するため、Custom 変数値の設定
;; (safe-local-variables) 後にファイルローカル変数値を設定する。そのた
;; め、ファイルを開く前に custom-file を読み込む。
;;;; 起動時に開くファイル
;; よく使うファイルはショートカットで開くようにするので、この箇所はコメントアウト。
;;(let
;;    ((files
;;      `(
;;        ;; シェルヒストリを開いておくと、作業中に履歴を簡単に検索でき
;;        ;; るので都合良い。
;;        "~/.zsh_history"
;;        ,(expand-file-name "init.el" user-emacs-directory)
;;        )))
;;  (dolist (file files)
;;    (if (file-exists-p file)
;;        (find-file file)
;;      (message "File `%s' does not exist!" file ))))

;; (provide 'init) ; make sure it only load once.

;; 幾つかのモードは、巨大ファイルでは動作が重くなるのでオフにする。

;; Local Variables:
;; coding: utf-8-unix
;; outline-minor-mode: t
;; auto-compile-mode: nil
;; auto-complete-mode: nil
;; flycheck-mode: nil
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; eval: (hide-sublevels 5)
;; End:

;;; init.el ends here
