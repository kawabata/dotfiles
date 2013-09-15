;;; .emacs/init.el  -*- coding: utf-8-unix; lexical-binding: t; -*-
;; For Emacs 24.3 / 24.3.50 (Windows, MacOSX, Linux)
;;
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Modified: 2013-09-15
;; URL: https://github.com/kawabata/dotfiles/
;;
;;; 技術メモ
;;;; 最新版Emacsおよび関連ソフトのインストール方法
;;
;; - Ubuntu 12.04 (LTS)
;;    % sudo add-apt-repository ppa:cassou/emacs
;;    % sudo apt-get update
;;    % sudo apt-get install emacs-snapshot
;;    % emacs-snapshot
;; - Macintosh
;;   Yamamoto Patch (comp.emacs.gnus)
;;   (git://github.com/railwaycat/emacs-mac-port)
;; - Windows
;;   GnuPack
;; - 個別パッケージインストール
;;   + TeX (MacTeX, W32TeX, TexLive)
;;   + Pandoc
;;   + GnuPG
;;   + Haskell
;;
;;;;; GnuPack のCyginw利用設定
;;   * config.ini の設定を以下のように変更する。
;;   [SetEnv]
;;   CYGWIN_DIR     = C:\cygwin
;;   (HOMEはコメントアウト)
;;
;;;; 現在のIssues
;; 必要に応じて、開発者に報告する。
;; - inf-ruby - autoload does load itself.
;; - powerline - autoload does load itself.
;; - erc-hl-nicks - autoload does load itself.
;; - auc-tex - autoload does load itself.
;; - ensime -- should not include auto-complete, that may shadow original auto-complete.
;; - pretty-mode.el -- should not include (require'cl)
;; - orglue - requires org-mac-link, which does not exists as independet package.
;; - alpha - 勝手に global-set-key を設定する。
;; - php-eldoc - cl package included.
;; - bibtex.el ... { .. ( ... ] ... } がエラーになる。
;; - clipboard-coding-system を utf-8-hfs にしても、Macでコピペしたら分解されたままになる。

;;;; init.el の書き方ルール
;;
;; (1) 標準添付ライブラリ
;;     (eval-and-compile  (require 'XXX)) ;; すぐに必要な場合
;;     (eval-when-compile (require 'XXX)) ;; 後で必要な場合
;;     (setq XXX-value YY)
;;     (global-set-key (kbd "XX") 'XXX-command)
;; (2) 外部ライブラリ （事前に package-initializeを実行）
;;     (eval-and-compile (require 'XXX nil t)) ; すぐに必要な場合
;;     (eval-when-compile (require 'XXX nil t)) ; 後で必要な場合
;; (3) 設定ファイルを外部に置く場合 (setup-XXX)
;;     (eval-after-load 'XXX (require 'setup-XXX nil t))...
;;
;;;; Emacs スクリプトの書き方
;;
;; - ライブラリを使用するスクリプトの場合：
;;   % emacs --eval "(package-initialize)" --batch -l package-build.el --eval "(package-build-archive 'fuel)"
;;   % emacs --script XXXX-build.el
;;
;; - サンプルプログラム
;;
;;   ;;; XXX-build.el
;;   ;; パッケージを利用する場合
;;   (package-initialize)
;;   (require 'package-build)
;;   (package-build-archive 'XXX)
;;   ;; コマンドラインの処理
;;   (require 'commander)
;;
;;;; Emacs パッケージ の書き方
;;
;; - 原則として、 auto-insert-alist の書き方に倣う
;; - lisp-mnt.el のコメントも確認すること。
;; - サンプル例
;;
;;   ;;; XXXX.el --- <<Short Description>>
;;   ;;
;;   ;; Filename: XXXX.el
;;   ;; Description: <<Description>>
;;   ;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;;   ;; Created: 2013-03-25
;;   ;; Version: 2.130914
;;   ;; Package-Requires: ((helm "1.0"))
;;   ;; Keywords: 《finder-known-keywordsのいずれか》
;;   ;; URL: https://github.com/kawabata/XXXX
;;   ;;
;;   ;;; Commentary:
;;   ;;
;;   ;; -*- mode:org -*-
;;   ;;
;;   ;; 《Documentation》
;;   ;;
;;
;;   ;;; Code:
;;
;;   《 Code 》
;;
;;   (provide 'XXXX)
;;   ;;; XXXX.el ends here
;;   ;; Local Variables:
;;   ;; lexical-binding: t
;;   ;; coding: utf-8
;;   ;; time-stamp-pattern: "10/Version:\\\\?[ \t]+2.%02y%02m%02d\\\\?\n"
;;   ;; End:
;;
;; - その他のルール
;;   (http://marmalade-repo.org/doc-files/package.5.html)
;;   1. コンパイル時は一切、警告が出ないようにする。
;;   2. マニュアルは Commentary に書けば、MELPAが Long Description にしてくれる。
;;      同じ内容を README に書けば、marmalade などが Long Description にしてくれる。
;;   3. global キーバインドを定義しない。
;;   4. autoload は非標準変数を操作したりmessageを表示したりしない。
;;
;;;; メジャーモード情報
;;;;; 設定機能
;;
;; | メジャーモード機能      | 変数例                                    |
;; |-------------------------+-------------------------------------------|
;; | syntax.c                | find-word-boundary-function-table, etc    |
;; |-------------------------+-------------------------------------------|
;; | font-lock.el            | font-lock-keywords, etc.                  |
;; | gud.el                  | gud-find-expr-function                    |
;; | imenu.el                | imenu-generic-expression, etc.            |
;; | indent.el               | indent-region-function, etc.              |
;; | newcomment.el           | comment-start, comment-end, etc.          |
;; | outline.el              | outline-regexp, outline-level             |
;; | textmodes/fill.el       | fill-paragraph-function, etc.             |
;; | textmodes/paragraphs.el | paragraph-start, paragraph-separate, etc. |
;;
;;;;; プログラミングモード （親がprog-mode）
;; | mode              | 文法チェック       | 補完                 | ドキュメント        | スニペット          | タグ  | デバッグ・実行  | リファクタリング   | テスト            |
;; |-------------------+--------------------+----------------------+---------------------+---------------------+-------+-----------------+--------------------+-------------------|
;; | awk-mode          | semantic           | semantic             | info-look           |                     | ctags | awk-it          |                    |                   |
;; | (cc-mode)         |                    |                      | (gawk.info)         |                     |       |                 |                    |                   |
;; | bat-mode          | semantic           |                      |                     |                     | ctags |                 |                    |                   |
;; | clojure-mode      | kibit-mode         | ac-nrepl             | clojure-cheatsheet  | clojure-snippets    |       | nrepl           | clj-refactor       | clojure-test-mode |
;; |                   |                    |                      |                     | datomic-snippets    |       | nrepl-inspect   | align-cljlet       |                   |
;; | c-mode/c++mode    | flymake-clang-c++  | auto-complete-clang  | c-eldoc             | yasnippet           | gtags | gud (gdb)       |                    |                   |
;; | (cc-mode)         | semantic           | semantic             | google-cpp-reference|                     |       |                 |                    |                   |
;; |                   |                    |                      | info-look           |                     |       |                 |                    |                   |
;; |                   |                    |                      | (libc/termcap.info) |                     |       |                 |                    |                   |
;; |                   |                    |                      | ffap                |                     |       |                 |                    |                   |
;; | csharp-mode       |                    | omnisharp            |                     |                     |       |                 |                    |                   |
;; | d-mode            | flymake            |                      |                     |                     |       |                 |                    |                   |
;; |                   | (cf. emacswiki)    |                      |                     |                     |       |                 |                    |                   |
;; | emacs-lisp-mode   |                    | auto-complete-config | eldoc               | yasnippet           |       | edebug          |                    |                   |
;; | (lisp-mode)       |                    |                      | help-fns            |                     |       |                 | erefactor          |                   |
;; |                   |                    |                      | info-look           |                     |       |                 |                    |                   |
;; |                   |                    |                      | (elisp/emacs.info)  |                     |       |                 |                    |                   |
;; |                   |                    |                      | ffap                |                     |       |                 |                    |                   |
;; | go-mode           | flymake-go         | go-autocomplete      | go-eldoc            | go-snippets         |       | gud (gdb)       | helm-go-packages   |                   |
;; | graphviz-dot-mode | semantic           |                      |                     |                     |       |                 |                    |                   |
;; | haskell-mode      | scion              | scion                | haskell-doc         | haskell-yas         |       | inf-haskell     |                    |                   |
;; |                   | flymake-hlint      |                      |                     |                     |       | ghci-completion |                    |                   |
;; |                   | flymake-haskell    |                      |                     |                     |       |                 |                    |                   |
;; | java-mode         | eclim              | eclim                | javadoc-lookup      |                     | gtags | gud (jdb)       |                    |                   |
;; | (cc-mode)         | semantic           | semantic             |                     |                     | jtags |                 |                    |                   |
;; | js2-mode          | semantic           | ac-js2               |                     | yasnippet           |       | skewer-mode     | js2-refactor       |                   |
;; |                   | flymake-jshint     | tern-auto-complete   |                     | angular-snippets    |       | jss             |                    |                   |
;; |                   | flymake-jslint     |                      |                     | buster-snippets     |       |                 | buster-mode        |                   |
;; |                   | flymake-gjshint    |                      |                     |                     |       |                 |                    |                   |
;; |                   | closure-lint-mode  |                      |                     |                     |       |                 |                    |                   |
;; |                   | tern               |                      |                     |                     |       |                 |                    |                   |
;; | lisp-mode         |                    |                      |                     |                     |       | inf-lisp        |                    |                   |
;; |                   |                    |                      |                     |                     |       | slime           |                    |                   |
;; | lua-mode          | flymake-lua        |                      |                     |                     | ctags |                 |                    |                   |
;; | makefile-mode     | semantic           |                      | info-look           |                     | ctags |                 |                    |                   |
;; | (make-mode)       |                    |                      | (make.info)         |                     |       |                 |                    |                   |
;; | opascal-mode      |                    |                      |                     |                     |       |                 |                    |                   |
;; | (opascal)         |                    |                      |                     |                     |       |                 |                    |                   |
;; | perl-mode         | flymake-perlcritic | perl-completion      | info-look           | yasnippet           | ctags | gud (perldb)    | lang-refactor-perl |                   |
;; |                   |                    | plsense              |                     |                     |       | perl-debug      |                    |                   |
;; | php-mode          | flymake-php        |                      | php-eldoc           |                     | gtags | inf-php         |                    |                   |
;; | processing-mode   |                    |                      |                     | processing-snippets |       |                 |                    |                   |
;; | python-mode       | flymake-python     | auto-complete-config |                     |                     |       | gud (pdb)       |                    | nose              |
;; |                   | semantic           | jedi                 |                     |                     |       |                 |                    |                   |
;; | ess-mode          |                    | ess-comp             | ess-eldoc           |                     |       | ess-inf         |                    |                   |
;; |                   |                    |                      |                     |                     |       | ess-debug       |                    |                   |
;; | ruby-mode         | flymake-ruby       | auto-complete-config |                     |                     | ctags | inf-ruby        | ruby-refactor      |                   |
;; |                   | semantic           | rsense               | rsense              |                     |       | rubydb3x        |                    |                   |
;; |                   | robe               | robe                 |                     |                     |       | robe            |                    |                   |
;; |                   |                    |                      |                     |                     |       | ruby-debug      |                    |                   |
;; |                   |                    |                      |                     |                     |       | ruby-test-mode  |                    |                   |
;; | rust-mode         | flymake            |                      |                     |                     |       |                 |                    |                   |
;; | scala-mode        | ensime-semantic-hl | ensime-auto-complete | ensime-doc          | yasnippet           |       | ensime          |                    | ensime-test       |
;; | (scala-mode2)     |                    |                      |                     |                     |       |                 |                    |                   |
;; | scheme-mode       | semantic           |                      | info-look           |                     | ctags |                 |                    |                   |
;; | (scheme.el)       |                    |                      | (r5rs.info)         |                     |       |                 |                    |                   |
;; |                   |                    |                      | gauche-manual       |                     |       |                 |                    |                   |
;; | sh-script         | flymake-shell      |                      | info-look           |                     |       |                 |                    |                   |
;; |                   |                    |                      | (bash.info)         |                     |       |                 |                    |                   |
;; | sql-mode          |                    |                      |                     | yasnippet           | ctags |                 |                    |                   |
;; | (sql)             |                    |                      |                     |                     |       |                 |                    |                   |
;; | typescript-mode   | tss                | tss                  | tss                 |                     |       |                 |                    |                   |
;; | (typescript)      |                    |                      |                     |                     |       |                 |                    |                   |

;;;;; テキストモード （主に親がtext-mode/fundamental-mode）
;; | mode          | 文法チェック       | 補完                 | ドキュメント   | スニペット       | タグ  |
;; |---------------+--------------------+----------------------+----------------+------------------+-------|
;; | css-mode      | flymake-css        | auto-complete-config | css-eldoc      | yasnippet        |       |
;; |               | flymake-csslint    | emmet-mode           |                |                  |       |
;; | haml-mode     | flymake-haml       |                      |                |                  |       |
;; | latex-mode    |                    |                      | info-look      | yasnippet        | ctags |
;; | (tex-mode)    |                    |                      | (latex.info)   |                  |       |
;; |               |                    |                      | ffap           |                  |       |
;; | markdown-mode |                    |                      |                | yasnippet        |       |
;; | nxml-mode     | rng-nxml           | nxml-mode            |                | docbook-snippets |       |
;; | scss-mode     | scss-mode(flymake) |                      |                |                  |       |
;; |               | flymake-sass       |                      |                |                  |       |
;; | texinfo-mode  |                    |                      | info-look      |                  |       |
;; |               |                    |                      | (texinfo.info) |                  |       |
;; | web-mode      |                    | emmet-mode           |                | yasnippet        | ctags |

;; Info ファイルがある場合は、C-h S で参照可能。
;; Exuberant Ctags と GTagsは連動が可能。
;;;; キーバインド一覧
;; | pre | A | M | C | S |    |                          |
;; |-----+---+---+---+---+----+--------------------------|
;; |     |   |   |   | * | 予 | （self-insert-command）  |
;; |     |   |   | O |   | 予 |                          |
;; |     |   |   | O | O | 可 | ※主に右手用             |
;; |     |   | O |   |   | 予 |                          |
;; |     |   | O |   | O | 可 | （MacOSのキーでも利用）  |
;; |     |   | O | O |   | 予 |                          |
;; |     |   | O | O | O | 可 | ※主に右手用             |
;; |     | O | * | * | * | 可 | ※                       |
;; |-----+---+---+---+---+----+--------------------------|
;; | C-c |   |   |   | * | 可 |                          |
;; | C-c |   |   | O | * | 予 | （Major-Modeで使用）     |
;; | C-c |   | O |   |   | 予 | b/f/o/w をorg-modeが使用 |
;; | C-c |   | O | O | * | 可 |                          |
;; | C-c | O | * | * | * | 可 | ※                       |
;; |-----+---+---+---+---+----+--------------------------|
;; | C-x |   |   |   | * | 予 |                          |
;; | C-x |   |   | O | * | 予 | （主にMinor-Modeで使用） |
;; | C-x |   | O | * | * | 可 |                          |
;; | C-x | O | * | * | * | 可 | ※                       |
;; |-----+---+---+---+---+----+--------------------------|
;; ※ Control-Shiftは公式には未サポート（ターミナルで利用不可）
;; ※ Alt キーは VNC (Linux) では利用不可能。（C-x @ a/M-Aで代用）
;; ※ A-C-M-S の４キー同時押しはUSBキーボードでは不可能
;;
;; - 参考資料
;;
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/

;;;; global prefix 一覧
;; Prefixの詳細は、<prefix> <help> (F1キー) で確認可能。
;; | 非モード型elisp     | Default   | 変更  |
;; |---------------------+-----------+-------|
;; | help-mode           | C-h       | C-z   |
;; | kmacro              | C-x C-k   |       |
;; | international/mule  | C-x <RET> |       |
;; | Alternatively-Key   | C-x @ a   |       |
;; | Hyper-Key           | C-x @ h   |       |
;; | *-other-window      | C-x 4     |       |
;; | bkmp-*-other-window | C-x 4 j   |       |
;; | *-other-frame       | C-x 5     |       |
;; | 2C                  | C-x 6     |       |
;; | iso-transl          | C-x 8     |       |
;; | abbrev              | C-x a     |       |
;; | abbrev-inverse      | C-x a i   |       |
;; | helm                | C-x c     | (M-X) |
;; | bookmark+           | C-x j     |       |
;; | narrow              | C-x n     |       |
;; | bookmark+           | C-x p     |       |
;; |                     | C-x r     |       |
;; | rectangle           | C-x r     |       |
;; | register            | C-x r     |       |
;; | vc                  | C-x v     |       |
;; | goto-map            | M-g       |       |
;; | isearch/occur       | M-s       |       |
;; | hi-lock             | M-s h     |       |
;; |---------------------+-----------+-------|
;; | howm                | C-c ,     |       |
;; | outline             | C-c @     |       |
;;
;;;; ショートカット一覧
;; 詳細は [[info:elisp#Reading%20Input]]
;; (M-x parse-init-global-key-settings で生成）
;; |---------+----------------------------------|
;; | M-A     | Alt prefix（Windows/Linuxのみ）  |
;; |---------+----------------------------------|
;; | C-c -   | (insert "⿺")                    |
;; | C-c 0   | (insert "⿰")                    |
;; | C-c 9   | (insert "⿹")                    |
;; | C-c ;   | orgtbl-comment-mode              |
;; | C-c =   | (insert "⿻")                    |
;; | C-c A   | artist-mode                      |
;; | C-c C   | cancel-debug-on-entry            |
;; | C-c E   | erase-buffer                     |
;; | C-c F   | recentf-open-files               |
;; | C-c L   | org-insert-link-global           |
;; | C-c O   | org-open-at-point-global         |
;; | C-c P   | pages-directory                  |
;; | C-c U   | untrace-function                 |
;; | C-c a   | org-agenda                       |
;; | C-c b   | org-iswitchb                     |
;; | C-c c   | org-capture                      |
;; | C-c d   | debug-on-entry                   |
;; | C-c e   | toggle-debug-on-error            |
;; | C-c g   | gnus                             |
;; | C-c i   | ids-edit-mode                    |
;; | C-c k   | clean-dmoccur-buffers            |
;; | C-c l   | org-store-link                   |
;; | C-c m   | gnus-msg-mail                    |
;; | C-c p   | list-packages                    |
;; | C-c r   | trace-function                   |
;; | C-c s   | gnus-spotlight-search            |
;; | C-c t   | google-translate-at-point        |
;; | C-c T   | google-translate-query-translate |
;; | C-c u   | untrace-all                      |
;; | C-c v   | variants-tree                    |
;; | C-c x   | maximize-frame-vertically        |
;; | C-c M-i | ~/.emacs.d/init.el               |
;; | C-c M-l | ~/.emacs.d/lookup/init.el        |
;; | C-c M-z | ~/.zsh_history                   |
;; | C-c M-m | *Messages*                       |
;; | C-c M-s | *scratch*                        |
;; | C-c M-' | lookup-restart                   |
;; | C-c M-; | lookup-word                      |
;; | C-c M-/ | lookup-pattern                   |
;; | C-c M-8 | mc/mark-all-like-this            |
;; |---------+----------------------------------|
;; | C-x C-b | ibuffer                          |
;; | C-x C-v | revert-buffer                    |
;; | C-x M-f | find-file-in-dropbox             |
;; | C-x M-l | find-library                     |
;; | C-x k   | kill-this-buffer                 |
;; | C-x r K | kill-rectangle-save              |
;; | C-x r t | string-rectangle                 |
;; | C-z C-z | iconify-or-deiconify-frame       |
;; |---------+----------------------------------|
;; | M-"     | lookup-select-dictionaries       |
;; | M-#     | <MacOS> Screenshot (file)        |
;; | M-$     | <MacOS> Regionshot (file)        |
;; | M-&     | <MacOS> Complete Sha1            |
;; | M-'     | lookup-list-modules              |
;; | M-*     | <MacOS> get AppleScript result   |
;; | M-?     | <MacOS> Help                     |
;; | M-/     | hippie-expand                    |
;; | M-;     | eval-region                      |
;; | M-A     | <MacOS> Lookup Man page          |
;; | M-B (X) | <MacOS> Pass files to bluetooth  |
;; | M-D     | <MacOS> Lookup Dictionary        |
;; | M-E     | bookmark-edit-annotation         |
;; | M-F     | <MacOS> Spotlight Search (X)     |
;; | M-H     | my-howm-concatenate-all-isearch  |
;; | M-I     | variants-insert                  |
;; | M-J     | <MacOS> Complete Citation        |
;; | M-K     | <MacOS> Complete Cite Key        |
;; | M-L     | <MacOS> Google Search            |
;; | M-M     | <MacOS> Lookup Man page          |
;; | M-O     | (other-window -1)                |
;; | M-Q     | (fill-paragraph 1)               |
;; | M-R     | my-rotate-keymap                 |
;; | M-S-SPC | toggle-input-method              |
;; | M-X     | <helm-command-map>               |
;; | M-Y     | <MacOS> Paste Stickmemo          |
;; | M-^     | <MacOS> SuperCollider exec       |
;; | M-c     | shell-toggle-cd                  |
;; | M-g s   | magit-status                     |
;; | M-h     | <MacOS> Hide Window              |
;; | M-l     | bury-buffer                      |
;; | M-o     | my-other-window                  |
;; | M-r     | my-rotate-keymap                 |
;; | M-s g   | grep                             |
;; |---------+----------------------------------|
;; | M-C-#   | <MacOS> Screenshot (clipboard)   |
;; | M-C-$   | <MacOS> Regionshot (clipboard)   |
;; |---------+----------------------------------|
;; | C-;     | helm-for-files                   |
;; | C-M-;   | helm-recentf                     |
;; | C-o     | toggle-input-method              |
;; | C-:     | bbdb                             |
;; | C-<     | mc/mark-previous-like-this       |
;; | C->     | mc/mark-next-like-this           |
;; | C-?     | transparency-set-value           |
;; |         | (overridden by undo-tree-redo)   |
;; | C-S-p   | shrink-window                    |
;; | C-S-n   | enlarge-window                   |
;; | C-S-b   | shrink-window-horizontally       |
;; | C-S-f   | enlarge-window-horizontally      |
;; | C-S-y   | helm-show-kill-ring              |
;; |---------+----------------------------------|
;; | A-*     | ace-jump-word-mode               |
;; |---------+----------------------------------|
;;
;; コマンドが設定されているのに動かない場合は以下の変数を確認し、
;; 自分の実行させたい、または不本意に実行されるコマンドを検索する。
;; - overriding-terminal-local-map
;; - overriding-local-map
;; - (keymap char property at point)
;; - emulation-mode-map-alists
;; - minor-mode-overriding-map-alist
;; - minor-mode-map-alist
;; - (Keymap text property at point)
;; - (current local-map)
;; - current global-map
;;
;;; 初期設定
(require 'cl-lib)
(eval-when-compile (require 'cl))
(setq init-file-debug t)
(cd "~/") ; ホームディレクトリにcd

;;; マクロ
;; マクロは先頭で設定する
;;;; ライブラリ遅延読み込み
;; hinted by https://gist.github.com/fukamachi/304391
;; and http://lunaryorn.com/blog/2013/05/31_byte-compiling-eval-after-load.html
(defmacro lazyload (funcs lib &rest body)
  (declare (indent 2))
  `(when (locate-library ,lib)
     ,@(mapcar
        (lambda (func)
          (typecase func
            (symbol `(autoload ',func ,lib nil t))
            (otherwise func)))
        funcs)
   (if (featurep (intern ,lib)) (eval '(progn ,@body))
       (eval-after-load ,lib
         '(progn
            ,@body)) t)))

;(require 'bytecomp)
;(defmacro lazyload (funcs feature &rest forms)
;  "After FEATURE is loaded, evaluate FORMS.
;FORMS is byte compiled."
;  (declare (indent 2))
;  `(;; バイトコンパイル時に、FEATUREをrequireする。
;    ;; FEATUREがない場合は式全体を with-no-warnings で囲む。
;    ,(if (or (not byte-compile-current-file)
;             (require (intern feature) nil :no-error))
;         'progn
;       (message "lazyload: cannot find %s" feature)
;       'with-no-warnings)
;    ;; 実行時にFEATUREがある場合は、autoload等を実行する。
;    (when (locate-library ,feature)
;      ,@(mapcar
;         (lambda (func)
;           (typecase func
;             (symbol `(autoload ',func ,feature nil t))
;             (otherwise func)))
;         funcs)
;      ;; FORMSをfunctionで囲むことで、バイトコンパイルさせる。
;      ;; すでに FEATUREが読み込まれている場合は即座に実行する。
;      (let ((function (function (lambda () ,@forms))))
;        (if (featurep (intern ,feature)) (funcall function)
;          (eval-after-load ',feature
;            `(funcall ,function)))))))

;;;; 関数のコマンド化
(defmacro command (&rest body)
  `(lambda () (interactive) ,@body))

;;;; 回転系コマンドのショートカット
;; | key | function               |
;; |-----+------------------------|
;; | f   | next-multiframe-window |
;; | h   | rotate-han             |
;; | k   | rotate-kana            |
;; | l   | rotate-latin           |
;; | s   | rotate-font-size       |
;; | t   | rotate-theme           |
;; | w   | rotate-winhist         |
(defvar my-rotate-keymap (make-sparse-keymap))
(defmacro smartrep-define-rotate-key (keymap prefix function-1 function-2)
  (declare (indent 2))
  `(smartrep-define-key
       ,keymap ,prefix
     '(("n" . (lambda () (interactive) ,function-1))
       ("p" . (lambda () (interactive) ,function-2)))))

;;; 標準設定
;;;; alloc.c
;;(setq gc-cons-threshold 8000000)

;;;; buffer.c
(setq scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0)
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction (quote left-to-right))
(setq truncate-lines nil)
(setq-default fill-column 70) ; 標準は70だが、やや少なめが見やすい。
(setq-default tab-width 8)              ; set tab width
(setq-default indicate-empty-lines t)   ; 空行をフリンジに表示
(setq-default line-spacing 1)           ; 行間を2ピクセルあける
(setq-default bidi-display-reordering t)
;; 現在バッファをkillするのにいちいち確認しない。
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; *scratch*バッファは削除させない (mmemo-buffersより)
(defun my-make-scratch (&optional arg)
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
(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))
;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(add-hook 'after-save-hook
          (lambda ()
            (unless (member "*scratch*" (my-buffer-name-list))
              (my-make-scratch 1))))
(global-set-key (kbd "C-c E") 'erase-buffer)

;;;; callproc.c
;; PATHがない環境から起動された場合に備えて、exec-path に "gnupg" 等の
;; 必要なソフトがインストールされているパスを追加しておく。
(loop for directory in '(
                         "/usr/bin"
                         "/opt/local/bin"
                         "/usr/local/bin"
                         "/cygwin/bin"
                         ;;"c:/w32tex/texinst2010/bin"
                         ;;"c:/Program Files/Ruby-1.9.2/bin"
                         )
      if (file-directory-p directory)
      do (add-to-list 'exec-path directory))

;;;; coding.c
;; coding関係の設定の前にこれを行う。
;;(set-language-environment "Japanese")
;; coding関係の設定の前にこれを行う。
(prefer-coding-system
 (cond ((equal system-type 'windows-nt) 'utf-8-dos)
       (t 'utf-8)))
(setq default-process-coding-system
      (case system-type ('windows-nt '(cp932 . cp932))
                        ('darwin (require 'ucs-normalize)
                                 '(utf-8-hfs . utf-8))
                        (t '(undecided . utf-8))))
;; decode-translation-table の設定
(coding-system-put 'euc-jp :decode-translation-table ; debug
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'iso-2022-jp :decode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'utf-8 :decode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
;; encode-translation-table の設定
; 原因は不明だが、これを設定すると Mac でのみ、eblook が動かなくなる？
; （ただし、init.el 読み込み後に設定すると動作する。）
;(coding-system-put 'euc-jp :encode-translation-table ;
;           (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp :encode-translation-table
           (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'cp932 :encode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
(coding-system-put 'utf-8 :encode-translation-table
           (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
;; charset と coding-system の優先度設定
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
          'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)
;; PuTTY 用の terminal-coding-system の設定
(apply 'define-coding-system 'utf-8-for-putty
   "UTF-8 (translate jis to cp932)"
   :encode-translation-table
   (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
   (coding-system-plist 'utf-8))
(set-terminal-coding-system 'utf-8-for-putty)
;; East Asian Ambiguous
;; - Adobe-Japan1 Width Information
;;   Proportional 1-230, 9354-9737, 15449-15975, 20317-20426
;;   Half-width 231-632, 8718, 8719, 12063-12087
;;   Third-width 9758-9778
;;   Quarter-width 9738-9757
;;   Others :: Full-width
(defun set-east-asian-ambiguous-width (width)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist (range
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
;; cp932エンコード時の表示を「P」とする (from GnuPack)
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)
;; 全角チルダ/波ダッシュをWindowsスタイルにする
(let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
  (mapc
   (lambda (coding-system)
     (coding-system-put coding-system :decode-translation-table table)
     (coding-system-put coding-system :encode-translation-table table))
   '(utf-8 cp932 utf-16le)))

;;;; composite.c
;; Character Composition
;;
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
;;
;;       composition-get-gstring       font-shape-gstring
;; +------------+       +-----------------+       +-----------------+
;; |STRING      +------>|GSTRING (glyphs) +------>|GSTRING (shaped) |
;; +------------+       +-----------------+       +-----------------+
;;                              ^
;;                              |
;;                       +------+------+
;;                       | font-object |
;;                       +-------------+
;;
;; - Example
;;   (setq object (open-font (find-font (font-spec :family "Devanagari Sangam MN"))))
;;   (query-font object)
;;   (font-get-glyphs object 0 6 "हिन्दी")
;;   (font-shape-gstring (composition-get-gstring 0 6 object "हिन्दी"))
;;   (font-get object :otf)
;;
;; - manual composition
;;   (insert (compose-string "." 0 1 '(?+ (tc . bc) ?o)))
;;   (insert (compose-string "." 0 1 '(?+ (cc . cc) ?o)))


;;;; data.c
;;;;; symbol architecture
;; | symbol      | check   | set      | get          | remove       |
;; |-------------+---------+----------+--------------+--------------|
;; | name        |         | intern   | symbol-name  | unintern     |
;; | dynamic val | boundp  | set      | symbol-value | makunbound   |
;; | lexical val |         | setq     | eval         |              |
;; | function    | fboundp | fset     | funcall      | fmakeunbound |
;; | plist       |         | setplist | symbol-plist |              |
;; |             |         | put      | get          |              |

;;;;; * database architecture *
;;
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
;; | struct XXX  | YYY     | make-XXX          | XXX-YYY      | setf XXX-YYY         |
;; | class XXX   | YYY     | make-instance     | oref         | oset                 |
;; | heap        | integer | make-heap         | heap-add     | heap-modify          |
;; | trie        | string  | make-trie         | trie-lookup  | trie-insert          |
;; | dict-tree   | string  | dictree-create    | dictree-look | dictree-insert       |
;;
;;
;; | kind        | enumerate      | type-check    | member-check     | delete            |
;; |-------------+----------------+---------------+------------------+-------------------|
;; | cons        |                | consp         |                  |                   |
;; | list        | mapcar         | listp         | member           | remove/remq       |
;; | vector      | mapcar         | vectorp       |                  | X                 |
;; | string      | mapcar         | stringp       | string-match     | X                 |
;; | char-table  | map-char-table | char-table-p  |                  | X                 |
;; | bool-vector | mapcar         | bool-vector-p |                  | X                 |
;; | ring        | ring-elements  | ring-p        |                  | ring-remove       |
;; | keymap      | map-keymap     | keymapp       |                  |                   |
;; | fontset     | fontset-list   |               | query-fontset    |                   |
;; | buffer      | while search-* | bufferp       |                  |                   |
;; | plist       | while cddr     |               | plist-member     |                   |
;; |             | map-plist*     |               |                  |                   |
;; | alist       | dolist         |               | assoc            | assoc-delete-all* |
;; |             |                |               | assq             | assq-delete-all   |
;; |             |                |               |                  | rassq-delete-all  |
;; | obarry      | mapatoms       |               |                  |                   |
;; | hash        | maphash        | hash-table-p  |                  | remhash           |
;; | struct XXX  |                | XXXX-p        |                  |                   |
;; | class XXX   |                | object-p      |                  | ???               |
;; | heap        |                | heap-p        |                  |                   |
;; | trie        |                | trie-p        | trie-complete    |                   |
;; | dict-tree   | dictree-mapcar |               | dictree-complete |                   |
;;
;;
;; | kind        | size              | value | order    | setf |
;; |-------------+-------------------+-------+----------+------|
;; | cons        | 2                 | any   | o(1)     | O    |
;; | list        | length (variable) | any   | o(n)     | O    |
;; | vector      | length (fixed)    | any   | o(1)     | O    |
;; | string      | length (variable) | char  | o(n)     | O    |
;; | char-table  | length            | any   | o(log n) | O    |
;; | bool-vector | length            | t/nil | o(1)     | X    |
;; | ring        | ring-size         | any   | o(n)     | X    |
;; | keymap      |                   |       |          |      |
;; | fontset     |                   |       |          |      |
;; | buffer      | (point-max)       |       |          |      |
;; | plist       | (/ length 2)      | any   | o(n)     | O    |
;; | alist       | length            | any   | o(n)     | O    |
;; | obarry      |                   | any   | o(1)     |      |
;; | hash        | hash-table-size   | any   | o(1)     |      |
;; | struct XXX  | (fixed)           | any   |          |      |
;; | class XXX   | fixed             | any   |          |      |
;; | heap        | length (variable) | any   | o(log n) |      |
;; | trie        | length (variable) | any   | o(log n) |      |
;; | dict-tree   |                   |       |          |      |
;;
;; - `char-table' and `class' can have parent.
;; - `char-table' can be used with `get-char-code-property' function.
;; - `obarray' can be used by a symbol that is `intern'ed to specific obarray.
;; - `dict-tree' can output file with `dictree-save/write'.
;;
;; char-code-property は、まず unicode-property-table-internal を見た後で、
;; char-code-property-table の各文字のplist を確認する。

;;;; dispnew.c
(setq visible-bell t)

;;;; editfns.c
(setq user-full-name "川幡 太一")
(defvar user-latin-name "Taichi KAWABATA")
(global-set-key (kbd "C-c M-m") (command (switch-to-buffer "*Messages*")))
(global-set-key (kbd "C-c M-s") (command (switch-to-buffer "*scratch*")))

;;;; emacs.c
(setq system-time-locale "C")

;;;; eval.c
(setq debug-on-error t)
(setq max-lisp-eval-depth 40000) ;; 600
(setq max-specpdl-size 100000) ;; 1300
(global-set-key (kbd "C-c e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c q") 'toggle-debug-on-quit)

;;;; fileio.c
(setq delete-by-moving-to-trash t)
(setq default-file-name-coding-system
      (cond ((eq system-type 'darwin) 'utf-8-hfs)
            ((eq window-system 'w32)  'cp932)
            (t 'utf-8)))

;;;; font.c
;;
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
;;
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
;;
;; - Functions with font-spec/entity/object as arguments
;;
;;   fontp/font-info/font-get/font-face-attributes/font-put/font-xlfd-name
;;   font-match-p
;;
;; - For debugging
;;
;;   font-drive-otf/font-otf-alternates/draw-string
;;
;; - Font Shaping by font Backend
;;
;;   font-shape-gstring
;;
;; - Examples
;;
;;   (font-info (font-spec :family "Hiragino Kaku Gothic ProN"))
;;   (setq entity (find-font (font-spec :family "Hiragino Kaku Gothic ProN")))
;;   (setq object (open-font entity))
;;   (font-face-attributes object)
;;   (font-get-glyphs object 0 6 "漢字と日本語")
;;   (font-otf-alternates object ?漢 'aalt)
;;   (font-variation-glyphs object ?漢)
;;   (font-shape-gstring)
;;   (font-get-glyphs object 0 3 [?a ?漢 #x800])
;;
(defun my-font-object-coverage (font-object)
  "指定された font-object でカバーされる文字の一覧を取得する。SIPまでをチェック。"
  (unless (fontp font-object 'font-object)
    (error "not font object! %s" font-object))
  (let* ((char-list (make-vector 196608 nil))
         glyph-list coverage)
    (dotimes (i 196608) (aset char-list i i))
    (setq glyph-list (font-get-glyphs font-object 0 196608 char-list))
    (dotimes (i 196608)
      (when (aref glyph-list i)
        (setq coverage (cons i coverage))))
    (nreverse coverage)))
;; (setq coverage (my-font-object-coverage object))

;;
;;;; fontset.c
(when (equal system-type 'windows-nt)
  (setq vertical-centering-font-regexp ".*"))
;; フォントセットの設定
(defun update-fontset (fontset size specs)
  ;; フォントセット FONTSET を更新する。
  ;; SPECS = ((target . fonts) (target . fonts) ...)
  ;; TARGET = t ← default for all, nil ← all
  ;; 最初から最後に向かってつなげていく。
  ;; TARGETのスクリプト名は、international/characters.el を参照。
  ;; fonts の最初の要素のフォントを利用する。
  (dolist (spec specs)
    (let ((target (car spec))
          (font-with-size (font-spec :size size :family (cadr spec)))
          (font           (font-spec :family (cadr spec))))
      (if (equal target t)
          (set-fontset-font fontset '(#x0 . #x2ffff) font-with-size)
        (set-fontset-font fontset target font nil 'prepend)))))
(defun filter-fonts (original-font-specs)
  "FONT-SPECSから、システムにインストールされていないフォントを除去。"
  (cl-remove-if
   'null
   (mapcar (lambda (font-spec)
             (let ((target (car font-spec))
                   (fonts (cl-remove-if-not
                           (lambda (x) (find-font (font-spec :family x)))
                           (cdr font-spec))))
               (if (null fonts) nil
                 (cons target fonts))))
           original-font-specs)))
(defvar japanese-fonts
  '(;; Macintosh 用
    ;"ヒラギノ丸ゴ Pro" "ヒラギノ角ゴ Pro" "ヒラギノ明朝 Pro"
    "Hiragino Kaku Gothic ProN"
    "Hiragino Maru Gothic ProN"
    "Hiragino Mincho ProN"
    ;; Windows 用
    "メイリオ"
    ;; Adobe フォント
    "小塚明朝 Pr6N R" "小塚ゴシック Pr6N R"
    ;;"Kozuka Mincho Pr6N R"
    "Kozuka Mincho Pr6N"
    "Kazuraki SPN"
    "Mio W4"
    "Ryo Text PlusN"
    "Ryo Gothic PlusN"
    "Ryo Display PlusN"
    ;; 花園明朝
    "HanaMinA"
    "HanaMinB"
    ;; イワタフォント
    "PMinIWA-HW-Md" ;; "PMinIWA-Md" ← 非固定幅
    "Iwata SeichouF Pro"
    ;; IPA
    "IPAmjMincho"
    "IPAexMincho" "IPAexGothic"
    ;; Mona font for AA
    "IPAMonaPGothic"))

(defvar font-specs
  ;; ((TARGET FONTS) (TARGET FONTS) ...)
  ;; 本リストの後方にあるTARGETほど優先される。
  (filter-fonts
   `((t "Inconsolata"
        ;; Adobe
        "Source Code Pro"
        ;; Programmer's Top 10 Fonts.
        "Monaco"
        "Monofur"
        "Droid Sans Mono"
        "DejaVu Sans Mono"
        ;; Windows
        "Consolas"
        ;; Macintosh
        "Menlo"
        ;; Mona
        "IPAMonaPGothic"
        "花園明朝 A")
     (burmese "hogehoge") ;; dummy (for test)
     (arabic "Arabic Typesetting Sample")
     (tagalog "Tagalog Stylized")
     ((#x1720 .  #x1c4f) "Code2000")
     ((#x1720 .  #x1c4f) "MPH 2B Damase")
     (symbol ,@japanese-fonts)
     (symbol "STIX")
     (han ,@japanese-fonts)
     (kana ,@japanese-fonts)
     (( #xf100 .  #xf6ff) "EUDC2")
     (mathematical "STIX")
     ((#x1f600 . #x1f6ff) "EmojiSymbols")
     ((#x20000 . #x2a6ff) "SimSun-ExtB")
     ((#x20000 . #x2a6ff) "花園明朝 B"))))
(defvar font-sizes '(16 18 20 22 24 28 12 14))

(update-fontset (face-attribute 'default :fontset) (car font-sizes) font-specs)

(defun rotate-fontspecs (target &optional direction)
  "`font-specs' 中の指定されたTARGETのFONTリストを回転させる。
DIRECTIONがnilなら前方向、それ以外なら後方向に回転させる。
結果として選択されたフォントを返す。"
  (let* ((spec (assoc target font-specs))
         (fonts (cdr spec)))
    (when fonts
      (if direction
          (setq fonts (list-rotate-backward fonts))
        (setq fonts (list-rotate-forward fonts)))
      (setcdr spec fonts)
      (update-fontset
       (face-attribute 'default :fontset) (car font-sizes) font-specs)
      (message "fonts=%s" (car fonts)))))

(defun rotate-font-sizes (&optional direction)
  (setq font-sizes
        (if direction (list-rotate-backward font-sizes)
          (list-rotate-forward font-sizes)))
  (update-fontset (face-attribute 'default :fontset) (car font-sizes) font-specs)
  (message "font size=%s" (car font-sizes)))

;;;; frame.c
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
;; デフォルトフレーム設定
(setq default-frame-alist
      '((background-color . "#102020")
        (foreground-color . "Wheat")   ; "#d0e0f0"
        (cursor-color . "#e0e0e0")
        (mouse-color . "Orchid")
        (pointer-color . "Orchid")
        (background-mode . dark)
        (line-spacing . 0)
        (scroll-bar-width . 12)
        (vertical-scroll-bars . left)
        (internal-border-width . 0)))
;; イニシアルフレーム設定　（デフォルトフレームと異なる場合に設定）
;; (setq initial-frame-alist default-frame-alist)
(modify-frame-parameters nil default-frame-alist)

;;;; keyboard.c
;; Event
;; keyboard-translate-table (keyboard.c)
;;  ↓
;; (Input Method)
;;  ↓
;; translation-table-for-input (coding.c)
;;  ↓
;; Event
(setq auto-save-timeout 30
      auto-save-interval 500
      echo-keystrokes 0.01)
;; Altキーの代替として、M-A をprefixとして使用する。
(define-key function-key-map (kbd "M-A") 'event-apply-alt-modifier)

;;;; lread.c
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
(global-set-key (kbd "M-;") 'eval-region)
;; load-path の設定
(defun add-to-load-path (dir)
  "新しい DIR を load-path の先頭に追加する。
DIR/subdir.el がある場合は、それを実行し、DIR下のディレクトリを追加する。"
  ;; 一旦追加されたpathは、normal-top-level-add-subdirs-inode-list に記
  ;; 録され、重複追加されないように管理される。
  (let* (;; subdir.el 実行は default-directory が設定されていることが必要。
         (dir (file-truename dir))
         (default-directory dir)
         (subdir-el (expand-file-name "subdirs.el" dir)))
    (when (file-directory-p dir)
      (remove dir load-path)
      (add-to-list 'load-path (expand-file-name dir))
      (if (file-exists-p subdir-el)
        (load subdir-el t t t)))))

;;;; macterm.c
(defvar mac-option-modifier nil)
(setq mac-option-modifier 'alt)

;;;; minibuf.c
(setq completion-ignore-case t)
;; ミニバッファ内で再帰的編集を許す
(setq enable-recursive-minibuffers t)

;;;; print.c
(setq print-circle t)

;;;; window.c
(setq next-screen-context-lines 3
      scroll-preserve-screen-position t
      scroll-conservatively most-positive-fixnum ; 4
      scroll-margin 4
      scroll-step 1)

;;;; xdisp.c
(setq frame-title-format "%b")
(setq line-number-display-limit 10000000)
(setq message-log-max 8000)             ; default 50
;; ミニバッファの変化が激しいと思うときは、'grow-onlyに。
(setq resize-mini-windows 'grow-only)
(setq-default show-trailing-whitespace t) ; 行末の空白を表示
(setq truncate-partial-width-windows nil)


;;; 標準ライブラリ
;; 分類
;; - application (provides complete set of data)
;; - utility (just function provision)
;; - major-mode
;; - minor-mode (global-minor-mode)
;; - extension to XXXX.el
;; - editing
;;;; align.el
(eval-after-load 'align
  '(progn
     (setq align-c++-modes (cons 'jde-mode align-c++-modes))
     (setq align-indent-before-aligning t)))

;;;; ansi-color.el (extension to comint.el)
;; SGR エスケープシーケンスをEmacsのFaceに解釈する。
(ansi-color-for-comint-mode-on)

;;;; arc-mode.el (minor-mode)
;; 注意。archive-mode と、auto-coding-alist は、両方を設定しなければならない。
(dolist (x '("\\.kmz\\'" "\\.odp\\'" "\\.otp\\'"))
  (add-to-list 'auto-coding-alist (cons x 'no-conversion))
  (add-to-list 'auto-mode-alist (cons x 'archive-mode)))
(lazyload () "arc-mode"
  (defun set-archive-file-name-coding-system (cs)
    "日本語ファイルの時は適宜この関数を呼び出してください。"
    (interactive "zCoding system for archived file name: ")
    (check-coding-system cs)
    ;; プログラム中で、強制的に変更されてしまうので、
    ;; file-name-coding-system を変更するしかない。あとで気をつけること。
    (setq archive-member-coding-system cs)))

;;;; autoinsert.el (global-minor-mode)
(auto-insert-mode t)
(defvar auto-insert-alist)
(lazyload () "autoinsert"
  (setq auto-insert-directory "~/.emacs.d/insert/")
  (setq auto-insert-query nil)
  ;; すでに設定済のテンプレより望ましいものがあればそれに置き換える。
  (dolist (elem
           '(
             ;;("\\.html" . "template.html")))
             (latex-mode . "template.tex")))
    (push elem auto-insert-alist)))

;;;; autorevert.el (global-minor-mode)
;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;;;; avoid.el (minor-mode)
;(if (eq window-system 'x) (mouse-avoidance-mode 'banish))

;;;; calendar.el (application)
;(setq calendar-view-diary-initially-flag t)
;(setq calendar-mark-diary-entries-flag t)

;;;; cedet/cedet.el, semantic.el (minor-mode)
;; cf. https://earthserver.com/Setting_up_a_C%2B%2B11_development_environment_on_Linux_with_Clang_and_Emacs
(lazyload () "cc-mode"
  (require 'cedet) ;; cedet
  (require 'semantic/senator) ;; semantic
  (require 'semantic/sb) ;; semantic, format
  (require 'cedet-files)
  (require 'srecode) ;; eieio
  ;;(setq semantic-load-turn-useful-things-on t)
  (global-ede-mode 1)
  ;; Activate semantic
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-decoration-mode 1)
  (global-semantic-highlight-func-mode 1)
  (global-semantic-stickyfunc-mode -1)
  (global-semantic-mru-bookmark-mode 1)

  ;; Try to make completions when not typing
  '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
  '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  ;; disable global-semantic-idle-summary-mode because it can interfere with flymake error messages:
  ;;(global-semantic-idle-summary-mode 1)

  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (semantic-add-system-include "/usr/include/GL" 'c++-mode)
  (semantic-add-system-include "/opt/local/include" 'c++-mode)
  (semantic-add-system-include "/usr/local/include" 'c++-mode)
  (semantic-add-system-include "/usr/include" 'c++-mode)
  (semantic-add-system-include "/usr/include/google" 'c++-mode))

;; semantic では senator は必須だが require されていない場合があるため補完。
(lazyload () "semantic"
  (require 'semantic/senator))

;;;; cc-mode.el (major-mode)
(lazyload () "cc-mode"
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (outline-minor-mode 1)
     (setq indent-tabs-mode t)
     (setq tab-width 4)
     (setq c-default-style '((c . "gnu") (java . "java"))))))

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

;;;; comint.el (major-mode)
(lazyload () "comint"
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)
  ;; [WinNT] Ctrl-M を除去する。
  (when (equal system-type 'windows-nt)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t))
  (setq comint-scroll-show-maximum-output t
        comint-input-ignoredups t
        comint-completion-autolist t
        comint-completion-addsuffix t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        ;; ヒストリサイズは500の10倍にする。
        comint-input-ring-size 5000))

;;;; cmuscheme.el (major-mode)
;; C-x C-e → scheme-send-last-sexp

;;;; dabbrev.el
(lazyload () "dabbrev"
  (setq dabbrev-abbrev-char-regexp "\\w\\|\\s_")
  (setq dabbrev-case-replace nil))

;;;; delsel.el
;; リージョンを文字入力で同時に削除。
;; 事故でうっかり削除がたまにあるため、Offにする。
;;(delete-selection-mode 1)

;;;; descr-text.el
(lazyload () "descr-text"
  (setq describe-char-unidata-list
        '(name general-category canonical-combining-class
          bidi-class decomposition decimal-digit-value digit-value
          numeric-value mirrored old-name iso-10646-comment uppercase
          lowercase titlecase)))

;;;; desktop.el
;; 作業途中で一旦、Emacsを再起動する必要があるときは、M-x desktop-save
;; を実行すること。
;; inhibit-default-init 変数を t にしてしまうので使用禁止。
;;(desktop-load-default)
;;(desktop-read)
;;(desktop-save-mode 1)
;;(add-hook 'kill-emacs-hook
;;          (lambda ()
;;            (desktop-truncate search-ring 3)
;;            (desktop-truncate regexp-search-ring 3)))

;;;; dired.el
(lazyload () "dired"
  (add-hook 'dired-mode-hook (lambda () (setenv "LANG" "C")))
  ;; diredのサイズ表示に Kbyte, Mbyte 等の単位を使う。
  ;; -h :: Kbyte, Mbyte 単位の表示
  (setq dired-listing-switches "-alh")
  ;; diredバッファの自動更新
  (setq dired-auto-revert-buffer t)
  ;; Diredでのコピー先ディレクトリの自動推定
  (setq dired-dwim-target t)
  ;; 再帰的にコピー・削除
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
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
  (defun dired-rotate-sort ()
    "Rotate dired toggle sorting order by `dired-sort-order'"
    (interactive)
    (setq dired-sort-order-position
          (% (1+ dired-sort-order-position) (length dired-sort-order)))
    (setq dired-actual-switches
          (concat dired-listing-switches (elt dired-sort-order
                                            dired-sort-order-position)))
    (dired-sort-other dired-actual-switches))
  (define-key dired-mode-map "s" 'dired-rotate-sort))

;; dired のバッファが氾濫しないように，ディレクトリを移動するだけなら
;; バッファを作らないようにする．
;;(defvar my-dired-before-buffer nil)
;;(defadvice dired-advertised-find-file
;;  (before kill-dired-buffer activate)
;;  (setq my-dired-before-buffer (current-buffer)))
;;(defadvice dired-advertised-find-file
;;  (after kill-dired-buffer-after activate)
;;  (when
;;      (and
;;       (eq major-mode 'dired-mode)
;;       (not (string= (buffer-name (current-buffer))
;;                     (buffer-name my-dired-before-buffer))))
;;    (kill-buffer my-dired-before-buffer)))
;;(defadvice dired-up-directory
;;  (before kill-up-dired-buffer activate)
;;  (setq my-dired-before-buffer (current-buffer)))
;;(defadvice dired-up-directory
;;  (after kill-up-dired-buffer-after activate)
;;  (when
;;      (and
;;       (eq major-mode 'dired-mode)
;;       (not (string= (buffer-name (current-buffer))
;;                     (buffer-name my-dired-before-buffer))))
;;    ;;(not (string-match "^[a-z]+:[/]$" (buffer-name my-dired-before-buffer))))
;;    (kill-buffer my-dired-before-buffer)))

;; Cygwin 環境では、diredのファイル名はutf-8のため、fopenと整合しない。
;;(when (file-executable-p "c:/cygwin/bin/ls.exe")
;;  (setq ls-lisp-use-insert-directory-program t)
;;  (setq insert-directory-program "c:/cygwin/bin/ls.exe"))

;;;; dired-aux.el
(lazyload () "dired"
  (require 'dired-aux)
  ;; atool を使い、多数の圧縮ファイルを閲覧可能にする。
  (when (executable-find "aunpack")
    (let ((dired-additional-compression-suffixes
           '(".7z" ".Z" ".a" ".ace" ".alz" ".arc" ".arj" ".bz" ".bz2" ".cab"
             ".cpio" ".deb" ".gz" ".jar" ".lha" ".lrz" ".lz" ".lzh" ".lzma"
             ".lzo" ".rar" ".rpm" ".rz" ".t7z" ".tZ" ".tar" ".tbz" ".tbz2"
             ".tgz" ".tlz" ".txz" ".tzo" ".war" ".xz" ".zip" ".epub")))
      (loop for suffix in dired-additional-compression-suffixes
            do (add-to-list 'dired-compress-file-suffixes
                            `(,(concat "\\" suffix "\\'") "" "aunpack"))))))

;;;; dired-x.el
(lazyload () "dired"
  (require 'dired-x)
  ;; dired-omit-mode :: LaTeX等の作業ファイルを表示しない。
  ;; Office のワークドキュメント（~で始まる）を表示しない。
  (setq dired-omit-files ; dired-omit-mode で隠すファイル
        (concat "^~\\|^\\.?#\\|^\\.\\|^\\.\\.?$"
                "\\|\\.aux$\\|\\.log$")))

;; shell-command-guesssing … "!" を押した時のシェルコマンドを予想
;; M-x dired-mark-extension … 特定の拡張子をマーク

;;;; doc-view.el
;; doc-view を無効にする。
(let ((doc-view-mode (rassoc 'doc-view-mode auto-mode-alist)))
  (if doc-view-mode (delete doc-view-mode auto-mode-alist)))

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

;;;; emacs-lisp/debug.el
;; よく使うのでショートカットを定義する。
(global-set-key (kbd "C-c d") 'debug-on-entry)
(global-set-key (kbd "C-c C") 'cancel-debug-on-entry)
;; C-M-c は exit-recursive-edit

;;;; emacs-lisp/edebug.el
;; デバッグ時は以下をtにすると、C-M-x で評価した関数にデバッガが入る。
;; 個別にデバッグ設定するなら、C-u C-M-x でも良い。
(setq edebug-all-defs nil)

;;;; emacs-lisp/eldoc.el
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;; emacs-lisp/find-func.el
(global-set-key (kbd "C-x M-l") 'find-library)
(lazyload () "find-func"
  (setq find-function-regexp
        (concat
         "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
\[^cgv\W]\\w+\\*?\\)\\|define-minor-mode\
\\|easy-mmode-define-global-mode\\|luna-define-generic\\)"
         find-function-space-re
         "\\('\\|\(quote \\)?%s\\(\\s-\\|$\\|\(\\|\)\\)")))
(find-function-setup-keys) ; C-x (4/5/null) (F/K/V)

;;;; emacs-lisp/lisp.el
;; デフォルトファイルなので lazyload しない。
(setq emacs-lisp-docstring-fill-column 72) ; default = 65
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq lexical-binding t)
            (message "lexical-binding is set to `t'.")))

;;;; emacs-lisp/macroexp.el
;; macroexp-let*
;; macroexp-let2

;;;; emacs-lisp/package-x.el
;; package-upload-buffer は tarファイルでも指定可能。
;; 自作ツールはパッケージ化すると、autoloadなどが自動化される。
;;
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
(lazyload (package-upload-file package-upload-buffer) "package-x"
  (setq package-archive-upload-base "~/.emacs.d/local-packages"))

;;;; emacs-lisp/trace.el
(global-set-key (kbd "C-c r") 'trace-function)
(global-set-key (kbd "C-c U") 'untrace-function)
(global-set-key (kbd "C-c u") 'untrace-all)

;;;; env.el
;; シェル環境変数をEmacsの環境変数に反映させる。
;; Emacs配下で動作するプロセスに引き継ぐ環境変数で重要なもの：
;; PATH, JAVA_TOOL_OPTIONS, HTTP_PROXY, BIBINPUTS, etc.
(eval-and-compile (require 'info))
(defun parse-env (env)
  "'env'命令の実行結果ENVの内容をEmacsに反映させる。"
  (dolist (env (split-string env "\n"))
    (when (string-match "^\\([A-Z][_A-Za-z0-9]+\\)=\\(.+\\)" env)
      (let* ((env-name (match-string 1 env))
             (env-val (match-string 2 env))
             (old-val (getenv env-name)))
        (unless (or (equal env "SHLVL") ; 例外
                    (equal env "TERM")  ; 例外
                    (equal env-val old-val))
          (message "%s (%s) is updated to %s" env-name old-val env-val)
          (pcase env
            ("PATH" (dolist (path (reverse (split-string env-val ":")))
                      (add-to-list 'exec-path path)))
            ("INFOPATH" (dolist (path (reverse (split-string env-val ":")))
                          (add-to-list 'Info-directory-list path))))
          (setenv env-name env-val))))))
(defun sync-env ()
  "シェルの環境変数をEmacsの環境変数を同期させる。"
  (interactive)
  (let ((env (shell-command-to-string "env")))
    (parse-env env)))
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
;;   -*- epa-file-encrypt-to: nil -*-
;; M-x epa-list-keys
;; 同じファイルに対して、パスワードを何度も打ち込まないようにする。
;; （GnuPG2 を使う場合は GPG-AGENTを起動する必要がある。）
(lazyload () "epa-file"
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;;;; erc/erc.el
(lazyload 
  ((defun erc-w3 () (interactive)
     (erc :server "irc.w3.org" :port "6665" :nick "kawabata"))) "erc"
  (erc-autojoin-mode 1)
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#emacs" "#emacs-lisp-ja" "#emacs-ja")
          ("irc.w3.org" "#css"))
        ;; erc-server "irc.freenode.net"
        ;; erc-port 6667
        erc-email-userid "kawabata"
        erc-nick '("dotabata" "kawabata" "bata")
        erc-nicklist-use-icons nil
        erc-password nil
        erc-nickserv-passwords nil
        erc-anonymous-login t
        ;; erc-auto-query 'bury
        ;; erc-join-buffer 'bury
        erc-max-buffer-size 40000
        erc-prompt-for-password nil
        erc-command-indicator "CMD"
        erc-send-whitespace-lines nil
        ;; erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
        ;;                          "324" "329" "332" "333" "353" "477")
        )
  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))
  ;;(defvar erc-insert-post-hook)
  ;;(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  ;;(setq erc-truncate-buffer-on-save t)
  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various)
  (setq erc-networks-alist
        '((freenode "freenode.net")
          (w3c "w3.org"))))

;;;; eshell/esh-opt.el
;; cf. [[info:eshell]]
;; http://www.masteringemacs.org/articles/2010/11/01/running-shells-in-emacs-overview/
;;
(lazyload () "esh-opt"
  (setenv "PAGER" "cat"))

;;;; ffap.el
;; iciclesと相性が悪そうなので、一時iciclesの方をオフにする。
(eval-and-compile (require 'ffap))
(ffap-bindings)
(global-set-key (kbd "C-x C-f") 'find-file-at-point)
(global-set-key (kbd "C-x 4 f") 'ffap-other-window)
(global-set-key (kbd "C-x d") 'dired-at-point)
(setq ffap-newfile-prompt t)
(setq ffap-rfc-path "http://www.ietf.org/rfc/rfc%s.txt")
(setq ffap-dired-wildcards "*")
(global-set-key (kbd "C-x C-v") 'revert-buffer)
;; バックスラッシュ記述のファイルサーバを自動補完
;; | テキスト文                                | 参照先               |
;; |-------------------------------------------+----------------------|
;; | \share\...                                | /Volume/share/...    |
;; | \\catseye\shared\...                      | /Volume/shared/...   |
;; | \\129.60.126.33\share\..                  | /Volume/share/...    |
;; | \\mirai-file.onlab.ntt.co.jp\mi-share\... | /Volume/mi-share/... |
;; samples
;; - \\mirai-file.onlab.ntt.co.jp\mi-share\01-管理簿\書庫\書庫整理2013.07.08.xls
;; - \\129.60.126.33\share\マネジメント中心_SG\マネジメントエンジン\2013年度\01.議論_AT\20130618_会議\AT資料
(add-to-list 'ffap-alist
             '("\\`\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))
;(add-to-list 'ffap-alist
;             '("\\`\\\\\\\\CATSEYE.*?\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))
(add-to-list 'ffap-alist
             '("\\`\\\\\\\\mirai-file.*?\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))
(add-to-list 'ffap-alist
             '("\\`\\\\\\\\msho-file.*?\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))
(add-to-list 'ffap-alist
             '("\\`\\\\\\\\129\\.60\\.126\\.33\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))
(add-to-list 'ffap-alist
             '("\\`\\\\\\\\129\\.60\\.121\\.48\\\\[-a-z]+\\\\.+" . my-ffap-server-complete))

(defun my-ffap-server-complete (name)
  (let ((name
         ;; 3. 先頭に "/Volumes" を加える。
         (concat "/Volumes"
                 ;; 2. バックスラッシュはスラッシュに変換
                 (replace-regexp-in-string
                  "\\\\" "/"
                  ;; 1. 先頭のサーバ名は除去。
                  (replace-regexp-in-string "^\\\\\\\\.+?\\\\" "/" name)))))
    (message "name=%s" name)
    (if (file-exists-p name) name
      (progn
        (setq name
              (car (file-expand-wildcards (concat name "*"))))
        (if (file-exists-p name) name
          (replace-regexp-in-string "[^/]+$" "" name))))))

;; ftp 時に ping をしないで，いきなり ange-ftp で開く
;(setq ffap-machine-p-known 'accept)
;; ffap-kpathsea-expand-path で展開するパスの深さ
;(setq ffap-kpathsea-depth 5)
;; ttp のように不完全な URL を修正する
;(defadvice ffap-url-at-point (after support-omitted-h activate)
;  (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
;    (setq ad-return-value (concat "h" ad-return-value))))
;; ffapを有効にする

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
(setq kept-old-versions 2)
(setq kept-new-versions 2)
(setq delete-old-versions t)
;; zip-mode などで、本来のキーを食うことがあるので、offにする。
(setq view-read-only nil)
;;(setq make-backup-files nil)
;;(setq version-control t)
;;(setq require-final-newline t)
(setq auto-mode-case-fold t) ;; auto-mode-alist を大文字小文字を区別して探して、その後に無視して探す。
(defun find-file-with-application (file)
  "ファイルを付随するアプリケーションで開く。"
  (let ((full-name (expand-file-name file)))
    (case system-type
      ('darwin      (shell-command (concat "open \"" full-name "\" &")))
      ('gnu/linux   (shell-command (concat "xdg-open \"" full-name "\"")))
      ('windows-nt  (shell-command (concat "cygstart " full-name " &"))))))
;; find-fileで Office ファイルを開いたとき、自動的にデフォルトアプリケー
;; ションに渡す。
(defadvice find-file (around find-file-with-application (file &optional wild))
  (cond
   ;; 条件つきでシステムでオープン
   ((and
     (or (string-match "\\.[sx]?html?$" file)
         (string-match "\\.epub$" file)
         (string-match "\\.asta$" file)
         (string-match "\\.jude$" file)
         (string-match "\\.mm$" file)
         (string-match "\\.zip$" file)
         (string-match "^.+\\.nb$" file)
         ;;拡張子 TS は、動画ファイルと TypeScriptファイルが衝突する。
         (string-match "^.+\\.ts$" file)
         (string-match "/$" file))
     (y-or-n-p "アプリケーションで開きますか??"))
    (find-file-with-application file))
   ;; 無条件でシステムでオープン
   ((or
        ;;(string-match "^.+\\.mp4$" file)
        (string-match "^.+\\.pdf$" file)
        (string-match "^.+\\.doc$" file)
        (string-match "^.+\\.xls$" file)
        (string-match "^.+\\.potx$" file)
        (string-match "^.+\\.pptx$" file)
        (string-match "^.+\\.ppt$" file)
        (string-match "^.+\\.docx$" file)
        (string-match "^.+\\.xlsx$" file)
        (string-match "^.+\\.odt$" file)
        (string-match "^.+\\.enc$" file)
        (string-match "^.+\\.wav$" file)
        (string-match "^.+\\.sxi$" file)
        (string-match "^.+\\.rtf$" file)
        (string-match "^.+\\.tif$" file)
        (string-match "^.+\\.cap$" file))
    (find-file-with-application file))
   (t ad-do-it)))

(ad-activate 'find-file)

;; 頻繁に開くファイル（org-captureで代用）
(global-set-key (kbd "C-c M-e")
  (command (find-file-other-window "~/.environ")))
(global-set-key (kbd "C-c M-g")
  (command (find-file-other-window "~/.emacs.d/.gnus.el")))
(global-set-key (kbd "C-c M-i")
  (command (find-file-other-window "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c M-l")
  (command (find-file-other-window "~/.emacs.d/lookup/init.el")))
(global-set-key (kbd "C-c M-w")
  (command (find-file-other-window "~/org/work/work.org.txt")))
(global-set-key (kbd "C-c M-j")
  (command (find-file-other-window "~/org/work/jsc2.org.txt")))
(global-set-key (kbd "C-c M-3")
  (command (find-file-other-window "~/org/work/w3c.org.txt")))
(global-set-key (kbd "C-c M-H") ; M-h はMacOSの予約キーなので避ける。
  (command (find-file-other-window "~/org/home/home.org.txt")))
(global-set-key (kbd "C-c M-p") ;
  (command (find-file-other-window "~/.passwd.gpg")))
(global-set-key (kbd "C-c M-z")
  (command (find-file-other-window "~/.zsh_history")))

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

;;;; font-core.el
(global-font-lock-mode t)

;;;; frame.el
(blink-cursor-mode 0)

;;;; gnus/gnus.el
;; 高機能ニュース・メールリーダ
(lazyload ((global-set-key (kbd "C-c g") 'gnus)
           (global-set-key (kbd "C-c m") 'gnus-msg-mail))
    "gnus-start"
  (setq gnus-init-file (expand-file-name ".gnus.el" user-emacs-directory)))

;;;; gnus/gnus-dired.el
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;;; gnus/html2text.el
;; iso-2022-jpをうっかりhtml化した時の復元に便利な設定
(lazyload () "html2text"
  (setq html2text-replace-list
        '(("&amp;" . "&") ("&nbsp;" . " ") ("&gt;" . ">") ("&lt;" . "<")
          ("&quot;" . "\"")))
  (setq html2text-remove-tag-list
        '("html" "body" "p" "img" "dir" "head" "div" "br" "font" "title"
          "meta" "tr" "td" "table" "span" "div"))
  (setq html2text-remove-tag-list2  '("li" "dt" "dd" "meta")))

;;;; gnus/mm-util.el
(eval-after-load "mm-util"
  '(when (coding-system-p 'cp50220)
     (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

;;;; gnus/score.el
(add-to-list 'auto-mode-alist '("\\.SCORE\\'" . gnus-score-mode))

;;;; gnus/sieve.el
;; RFC3028 Email Filtering Language Sieve.

;;;; gv.el
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
  (cdr (assoc key alist)))

;;;; help.el
;; helpキーを C-h から C-zに割り当て直す。
(setq help-char 26)
(global-set-key (kbd "C-z") help-map)
(if (eq window-system 'x)
    (global-set-key (kbd "C-z C-z") 'iconify-or-deiconify-frame))
(temp-buffer-resize-mode 1)

;;;; help-at-pt
(lazyload nil "help-at-pt"
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1))

;;;; hippie-exp.el
(lazyload ((global-set-key (kbd "M-/") 'hippie-expand)) "hippie-exp"
  (setq hippie-expand-dabbrev-as-symbol t))
;;;; hl-line.el
;; 現在の行をハイライトする
(defface hlline-face ;; デフォルトはhl-line
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line."
  :group 'basic-faces)
(eval-and-compile (require 'hl-line))
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;;;; htmlfontify.el
;; coral が入手できないので、hfyview.el を使ってブラウザに表示させて
;; それを印刷する。
;(defun print-buffer-html ()
;  "印刷する。"
;  (interactive)
;  (let ((file (make-temp-file "print-buffer-" nil ".html")))
;    (htmlfontify-buffer nil file)
;    (write-region (point-min) (point-max) file)
;    (message "printing... %s " file)
;    (cond ((eq system-type 'darwin)
;           (shell-command (concat "coral -d " file)))
;          ((eq system-type 'windows-nt)
;           (w32-shell-execute "print " file))
;          (t (shell-command (concat "open " file))))
;    (message "printing... done")
;    (delete-file file)))

;;;; indent.el
(setq indent-line-function 'indent-relative-maybe)
(setq-default indent-tabs-mode nil) ; no tabs indentation.

;;;; isearch.el
(setq lazy-highlight-initial-delay 0) ; isearch のハイライト反応を良くする。
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;;;; ibuffer.el
;; list-buffersの高機能版。色が付いて、様々なパラメータ表示が可能。隠れ
;; バッファを表示したい場合は、一時的にibuffer-maybe-show-predicatesの
;; 値をnilにする。
(lazyload ((global-set-key (kbd "C-x C-b") 'ibuffer)) "ibuffer"
  ;; 本当は設定しないことになっているが、これで大丈夫そう。
  (setq ibuffer-auto-mode t)
  (define-key ibuffer-mode-map "\M-o" nil)
  (define-key ibuffer-mode-map "\M-O" 'ibuffer-visit-buffer-1-window)
  (setq ibuffer-default-sorting-mode 'alphabetic) ; default is 'recency
  ;; ibufferのオリジナルカラムの設定
  (define-ibuffer-column
   ;; ibuffer-formats に追加した文字
   coding
   ;; 一行目の文字
   (:name " coding ")
   ;; 以下に文字コードを返す関数を書く
   (condition-case nil
       (if (coding-system-get buffer-file-coding-system 'mime-charset)
           (format " %s"
                   (coding-system-get buffer-file-coding-system 'mime-charset))
         " undefined")
     (error " undefined")))

  ;; 新しいibufferフォーマットの設定
  ;; オリジナルは、以下の通り。（ibuffer-switch-format で切り替え）
  ;; ((mark modified read-only " "  (name 16 -1) " "
  ;;        (size 6 -1 :right) " "
  ;;        (mode 16 16 :right) " " filename)
  ;;  (mark " " (name 16 -1) " " filename))
  (setq ibuffer-formats
        '((mark modified read-only (coding 15 15) " " (name 30 30)
                " " (size 6 -1) " " (mode 16 16) " " filename)
          (mark (coding 15 15) " " (name 30 -1) " " filename)))
  (setq ibuffer-fontification-alist
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
;; 保存時にファイル名の自動補完を行う。
;; C-f :: デフォルトの動作に戻る。
;; C-j :: 入力テキストをそのまま使う。
;; ファイル保存時に勝手に既存ファイル名を補完するのが不便なので使用中止。
;;(when (require 'ido nil t)
;;  (ido-mode t)
;;  (ido-everywhere 1)
;;  (define-key ido-file-dir-completion-map (kbd "SPC") 'ido-exit-minibuffer)
;;  (define-key ido-file-dir-completion-map (kbd "C-h") 'ido-delete-backward-updir)
;;  (setq ido-enable-prefix nil
;;        ido-enable-flex-matching t
;;        ido-create-new-buffer 'always
;;        ido-use-filename-at-point 'guess
;;        ido-use-virtual-buffers t ; [C-x b] (or M-x ido-switch-buffer)
;;        ido-max-prospects 10))

;;;; image-file.el
;; 画像ファイルは自動的にimage-file-modeで開く。
(auto-image-file-mode t)

;;;; image.el
;;(setq imagemagick-render-type 1)

;;;; imenu.el
;; Emacs 24.3 only.
;; load-theme を実行すると、eval → edebug-read → edebug-read-sexp
;; → which-function → imenu--make-index-alist →
;; →imenu-default-create-index-function
;; でエラーが起きるのを抑止する。
;; 対応モード : C, C++, Java, Fortran, etc.
(setq-default which-function-imenu-failed t)

;;;; info.el
(eval-after-load "info"
  '(progn
     ;; /usr/share/info を取り除く（/usr/local/share/info を優先する）
     (setq Info-directory-list (delete "/usr/share/info" Info-directory-list))
     ;; M-n を無効にする
     (define-key Info-mode-map "\M-n" nil)))

;;;; international/mule.el
(case system-type
  ('darwin
   (modify-coding-system-alist 'process "zsh" '(utf-8-hfs . utf-8))
   (modify-coding-system-alist 'process "git" '(utf-8-hfs . utf-8)))
  ('windows-nt
   ;;(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8-dos)
   ;;(modify-coding-system-alist 'process ".*sh" 'utf-8-dos)
   (set-keyboard-coding-system 'cp932)))
;; w32select.c では、'utf-16le-dos で CF_UNICODEを利用する。
(set-selection-coding-system
 (case system-type
   ('darwin 'utf-8-hfs)
   ('windows-nt 'utf-16le-dos)
   (t 'utf-8)))

;; mule-version を日本語に直す。
;; かつて mm-version.el としても実装されていた。
;; 本家Emacsのタイムラインは http://www.jwz.org/doc/emacs-timeline.html 参照。
(let*
    ;;; Emacs for Japanese
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

;; Adobe-Japan1 の文字集合を define-charset で定義する。
(let ((cid2code "~/.emacs.d/cmap/aj16/cid2code.txt")
      (aj16map  "~/.emacs.d/cmap/aj16/aj16.map")
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
(global-set-key (kbd "C-o") 'toggle-input-method) ;; non-Mac用
(global-set-key (kbd "M-S-SPC") 'toggle-input-method) ;; Mac用

;;;; iswitchb.el
;; TODO
;; iswitchb と、switch-to-buffer の両方を必要に応じて切り分けたいが、
;; iswtichb は、switch-to-buffer のキーバイドをsubstitute-..で、全て横
;; 取りする。これを抑制する方法。
;; TODO
;; iswitchb で、" " で始まるバッファを検索する方法
;; MEMO
;; - C-s/C-r で候補一覧をローテートできる。
(iswitchb-mode 1)
(lazyload () "iswitchb"
  (setq iswitchb-use-virtual-buffers t)
  (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
  (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)
  (define-key minibuffer-local-completion-map
    "\C-c\C-i" 'file-cache-minibuffer-complete)
  (setq iswitchb-method 'samewindow))
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
;;(global-linum-mode t)
;;(set-face-attribute 'linum nil :height 0.8)
;;(setq linum-format "%4d")

;;;; locate.el
;; Macintosh で locate を使う場合
;; % sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
;; Macintosh で外部ファイルサーバをインデックス化する場合
;; % mdutil -i on /Volumes/mountname
(lazyload () "locate"
  (when (eq system-type 'darwin)
    (setq locate-command "mdfind")))

;;;; ls-lisp.el
(eval-and-compile (require 'ls-lisp))
(setq ls-lisp-ignore-case t)
(setq ls-lisp-dirs-first nil)

;;;; man.el
(eval-after-load 'man
  '(progn
     (define-key Man-mode-map "\en" nil)
     (define-key Man-mode-map "\ep" nil)))

;;;; mb-depth.el
(minibuffer-depth-indicate-mode 1)

;;;; menu-bar.el
(menu-bar-mode nil)

;;;; minibuf-eldef.el
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
(defadvice browse-url (before support-omitted-h (url &rest args) activate)
  (when (and url (string-match "\\`ttps?://" url))
    (setq url (concat "h" url))))
(setq browse-url-browser-function
      '(("." . browse-url-default-browser)))


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
;;
;; 注意：auth-source.el を使えば、ssh利用時のパスワードの入力は不要。
;;
;; 注意：相手側hostでは、zshのプロンプトに色を付けたり、ls のエイリアス
;;       で --colorオプションを付けたりしないこと。freezeする場合は、そ
;;       こでBacktraceを出して、*tramp*バッファのプロセス状態を確認する。
;;
;;       sudo を使う場合は、zshで"Igonore insecure directories?" プロン
;;       プトが出ないよう、sudo -s 実行時で確認しておく。出る場合は、
;;       SHELL環境変数を無効にする(sudo の-iオプション）か、fpath から
;;       自分ディレクトリを外す。
;;
;;       なお、smbclientを使ったWindows FSへのアクセスは面倒なことがなくて便利。
;;       これに統一してしまうのもいいかも。;->  使い方：/smb:server_name:/path
(lazyload () "tramp"
  ;; root@localhostへはsudoメソッドを使う。
  (add-to-list
   'tramp-default-method-alist
   '("\\`localhost\\'" "\\`root\\'" "sudo")))

;; ファイルがrootの場合、自動的にsudoで開き直す。
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

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
(cl-delete-if (lambda (x) (equal (cdr x) 'xml-mode)) auto-mode-alist)
(cl-delete-if (lambda (x) (equal (cdr x) 'xml-mode)) magic-mode-alist)
;; その他のXML関連のモードのうち、自分で最初から編集するものを追加していく。
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))
(add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))

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
;;
;; XSDは、trangで rnc へ変換する。
;; 便利な RelaxNG Schema集
;; https://github.com/skybert/my-little-friends/tree/master/emacs/.emacs.d/xml
;; kml における section/name 設定例：
;; (setq nxml-section-element-name-regexp "Folder\\|Placemark\\|GroundOverlay")
;; (setq nxml-heading-element-name-regexp "name")
;; 手動でschemaを設定するには、C-c C-s C-f を使用する。
(lazyload () "nxml-mode"
  (add-to-list 'rng-schema-locating-files
               (expand-file-name "~/.emacs.d/schema/schemas.xml"))
  (add-hook 'nxml-mode-hook
            (lambda ()
              (define-key nxml-mode-map "\C-c/" 'rng-complete)
              ;;(define-key nxml-mode-map "\M-q" 'my-xml-pretty-print-buffer )
              ))
  (setq nxml-slash-auto-complete-flag t)
  ;; 自分で編集するXMLにおけるセクションの設定
  (setq nxml-section-element-name-regexp
        (eval-and-compile
        (regexp-opt
         '(;; html5
           "head" "body" "blockquote" "details" "fieldset" "figure" "td"
           "section" "article" "nav" "aside"
           ;; atom
           "entry"))))
  ;; 自分で編集するXMLにおけるヘッダの設定
  (setq nxml-heading-element-name-regexp
        (eval-and-compile
          (regexp-opt
           '(;; html5
             "h1" "h2" "h3" "h4" "h5" "h6"
             ;; atom
             "title"))))
  (setq nxml-slash-auto-complete-flag t))

;; saxon
;; Ubuntu 11 : /usr/share/java/saxon-6.5.5.jar
;; MacOS X   : /opt/local/share/java/saxon-9.1he.jar

;;;; org/org.el (major-mode)
;; org-mode は、git版を優先するため、load-pathの設定が終わった後で設定
;; を行う。後方を確認。

;;;; outline.el (minor-mode)
;; org-mode のFAQにある方法
;; http://orgmode.org/worg/org-faq.html#use-visibility-cycling-in-outline-mode
(lazyload () "outline"
  (define-key outline-minor-mode-map [(tab)] 'org-cycle)
  (define-key outline-minor-mode-map [(control tab)] 'org-global-cycle)
  (define-key outline-minor-mode-map "\C-c\C-f" 'outline-forward-same-level)
  (define-key outline-minor-mode-map "\C-c\C-b" 'outline-backward-same-level)
  (define-key outline-minor-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-minor-mode-map "\C-c\C-p"
    'outline-previous-visible-heading)
  (define-key outline-mode-map [(tab)] 'org-cycle)
  (define-key outline-mode-map [(shift tab)] 'org-global-cycle))

;;;; paren.el
(eval-and-compile (show-paren-mode t))
(setq show-paren-style 'parenthesis)

;;;; progmodes/compile.el
(define-key ctl-x-map "'"  'compile)

;;;; progmodes/flymake.el
;;(lazyload () "flymake"
;;  (set-face-background 'flymake-errline "red4")
;;  (set-face-background 'flymake-warnline "dark slate blue"))

;;;; progmodes/grep.el
(if (executable-find "xzgrep")
    (setq grep-program "xzgrep")
  (if (executable-find "bzgrep")
      (setq grep-program "bzgrep")))
(global-set-key (kbd "M-s g") 'grep)

;;;; progmodes/gdb-mi
(lazyload () "gdb-mi"
  (setq gdb-many-windows t
        gdb-show-main t))

;;;; progmodes/gud.el
(lazyload () "gud"
  ;;http://www.youtube.com/watch?v=p7XdkrlFXnU
  ;; https://gist.github.com/chokkan/5693497
  (add-hook
   'gdb-mode-hook
   '(lambda ()
      (gud-tooltip-mode t)
      (gud-def gud-break-main "break main" nil "Set breakpoint at main.")))
  (define-key gud-minor-mode-map (kbd "<f1>") 'gud-print)
  (define-key gud-minor-mode-map (kbd "<S-f1>") 'gud-watch)
  (define-key gud-minor-mode-map (kbd "<f2>") 'gud-refresh)
  (define-key gud-minor-mode-map (kbd "<f5>") 'gud-cont)
  (define-key gud-minor-mode-map (kbd "<S-f5>") 'gud-kill)
  (define-key gud-minor-mode-map (kbd "<f6>") 'gud-until)
  (define-key gud-minor-mode-map (kbd "<f9>") 'gdb-set-clear-breakpoint)
  (define-key gud-minor-mode-map (kbd "<S-f9>") 'gud-break-main)
  (define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)
  (define-key gud-minor-mode-map (kbd "<f11>") 'gud-step)
  (define-key gud-minor-mode-map (kbd "<C-f10>") 'gud-until)
  (define-key gud-minor-mode-map (kbd "<C-f11>") 'gud-finish)
  (define-key gud-minor-mode-map (kbd "<S-f11>") 'gud-finish)
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

  (defun gud-kill ()
    "Kill gdb process."
    (interactive)
    (with-current-buffer gud-comint-buffer (comint-skip-input))
    (kill-process (get-buffer-process gud-comint-buffer))))


;;;; progmodes/hideshow.el
(eval-after-load 'hideshow
  '(progn
     (setq hs-hide-comments-when-hiding-all nil)
     (add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))))

;;;; progmodes/inf-lisp.el
(lazyload () "inf-lisp"
  (setq inferior-lisp-program (or (executable-find "ccl")
                                  (executable-find "sbcl"))))

;;;; progmodes/make-mode.el
;(add-hook 'makefile-mode-hook
;          (function (lambda ()
;                      (fset 'makefile-warn-suspicious-lines 'ignore))))

;;;; progmodes/opascal.el
(autoload 'opascal-mode "opascal")
(add-to-list 'auto-mode-alist
             '("\\.\\(pas\\|dpr\\|dpk\\)\\'" . opascal-mode))

;;;; progmodes/scheme.el
(lazyload () "scheme"
  (setq scheme-program-name (executable-find "gosh")))

;;;; progmodes/sh-script.el
(lazyload () "sh-script"
  (add-hook 'sh-mode-hook 'outline-minor-mode))

;;;; progmodes/sql.el
;; 主にsqliteと組み合わせて使うことを想定する。
;; http://www.emacswiki.org/cgi-bin/wiki.pl?SqlMode
;; M-x sql-help, M-x sql-sqlite
(autoload 'master-mode "master" "Master mode minor mode." t)

;; SQL mode に入った時点で sql-indent / sql-complete を読み込む
(lazyload (sql-mode sql-oracle sql-ms) "sql"
  ;; デフォルトのデータベースの設定
  (setq sql-user nil)
  (setq sql-sqlite-program "sqlite3")
  (setq sql-database "sqlite")
  ;; SQLi の自動ポップアップ
  (setq sql-pop-to-buffer-after-send-region t)
  ;; 「;」をタイプしたら SQL 文を実行
  (setq sql-electric-stuff 'semicolon)
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
  ;;            (setq comint-input-autoexpand t)
  ;;            (setq comint-output-filter-functions
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
  ;;      (setq comint-save-input-ring-index comint-input-ring-index)
  ;;      (setq comint-input-ring-index nil))))
  ;;(ad-activate 'sql-send-region)
  )

;;;; recentf.el
;; 最近開いたファイルの一覧を表示。helm.el と組み合わせる。
(eval-and-compile (recentf-mode 1))
(global-set-key (kbd "C-c F") 'recentf-open-files)
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)

;;;; rect.el
(eval-and-compile (require 'rect))
(if (functionp 'string-rectangle)
    (global-set-key (kbd "C-x r t") 'string-rectangle))
(defun kill-rectangle-save (start end)
  (interactive "*r\nP")
  (setq killed-rectangle (extract-rectangle start end)))
(global-set-key (kbd "C-x r K") 'kill-rectangle-save)

;;;; saveplace.el
;; ファイルでのカーソルの位置を保存しておく
(eval-and-compile (require 'saveplace))
(when (file-writable-p "~/.emacs.d/.emacs-places")
  (setq save-place-file (convert-standard-filename "~/.emacs.d/.emacs-places"))
  (setq-default save-place t))

;;;; server.el
(when (not (equal system-type 'windows-nt))
  (server-start))

;;;; shell.el
;; ファイル名に使われる文字
(eval-and-compile (require 'shell))
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
;; zsh のヒストリファイル
;; - 0x80-0x9d,a0 :: 0x83 (r0 + 0x20)
(define-ccl-program zsh-history-decoder
  '(1 ((loop
        (read-if (r0 == #x83)
                 ((read r0) (r0 ^= #x20)))
        ;; バイナリ出力を前提（別途UTF-8エンコード要）
        (write r0)
        (repeat))))
  "decode .zsh_history file.")

(define-ccl-program zsh-history-encoder
  '(2 ((loop
        ;; バイナリ入力をを前提
        (read r0)
        (r1 = (r0 < #x9e))
        (r2 = (r0 == #xa0))
        (if (((r0 > #x82) & r1) | r2)
            ((write #x83) (write (r0 ^ #x20)))
          (write r0))
        (repeat))))
  "encode .zsh_history file.")

;; CCLのwriteのUCS符号がEmacsに入る。
;; これをLatin-1としてdecodeしてUTF-8でencode。
(defun post-read-decode-utf8 (len)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (encode-coding-region (point-min) (point-max) 'latin-1)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (- (point-max) (point-min)))))

(defun pre-write-encode-utf8 (ignored ignored2)
  (identity ignored2)
  (encode-coding-region (point-min) (point-max) 'utf-8)
  (decode-coding-region (point-min) (point-max) 'latin-1))

;; CCLの注意点
;; - バイナリを扱う際は、事前に バッファのバイナリをlatin-1 にエンコードする。
(define-coding-system 'zsh-history "ZSH history"
  :coding-type 'ccl
  :charset-list '(unicode)
  :mnemonic ?Z :ascii-compatible-p t
  :eol-type 'unix
  :ccl-decoder 'zsh-history-decoder
  :post-read-conversion 'post-read-decode-utf8
  :ccl-encoder 'zsh-history-encoder
  :pre-write-conversion 'pre-write-encode-utf8)

(modify-coding-system-alist 'file "zsh_history" 'zsh-history)

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

(defun zsh-arrange-history (hist)
  "zshのヒストリデータのタイムスタンプとメタキャラを除去する。"
  (when hist
    (replace-regexp-in-string "^.+?;" "" hist)))

;(setq shell-mode-hook
;      ;; ヒストリファイルのタイムスタンプとメタキャラ除去
;      (lambda ()
;        (when (and (stringp comint-input-ring-file-name)
;                   (string-match "zsh_history" comint-input-ring-file-name))
;          (let ((array (cddr comint-input-ring)))
;            (do ((i 0 (+ i 1))) ((> i (1- (length array))))
;              (aset array i (zsh-arrange-history (aref array i))))))))

(defun new-shell ()
  (interactive)
  (let ((shell (get-buffer "*shell*")))
    (if (null shell) (shell)
      (set-buffer shell)
      (rename-uniquely)
      (shell))))


;;;; simple.el
(setq eval-expression-print-length nil) ; default 12.
(setq eval-expression-print-level nil)  ; default 4
(setq mail-user-agent 'gnus-user-agent) ; Gnusをメールに使用する。
(setq kill-whole-line t)
(line-number-mode t)
(column-number-mode t)
(transient-mark-mode t)
(global-set-key (kbd "C-M-h") 'backward-kill-word) ; 単語をまとめてBS。
(global-set-key (kbd "C-x :") 'goto-line) ; 古い慣習。
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(normal-erase-is-backspace-mode 1)
;; (setq interprogram-cut-function x-select-text)
;; overwriteは危険なので警告を出す。
(put 'overwrite-mode 'disabled t)

;;;; speedbar.el
;; 不明なファイルは非表示。
(lazyload () "speedbar"
  (setq speedbar-show-unknown-files nil))

;;;; startup.el
;; interactive-mode 時の、 巨大テキストを貼り付けた時の処理の遅さを回避する。
(setq initial-major-mode 'text-mode)
(setq mail-host-address (replace-regexp-in-string "^.+@" "" user-mail-address))
;; `default.el' があっても読み込まない。（site-start.elは読み込む。）
;;(setq inhibit-default-init t)
(setq inhibit-startup-message t)
;;(setq inhibit-startup-echo-area-message -1)

;;;; subr.el
(fset 'yes-or-no-p 'y-or-n-p)
;; C-h / DEL exchange.
(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.
(setq read-quoted-char-radix 16)

;;;; term.el
;; エスケープ・シーケンスは、/etc/e/eterm-color.ti に従う。
(when (executable-find "zsh")
  (setq explicit-shell-file-name "zsh"))

;;;; term/common-win.el
;; [Emacs 24.3] x-select-enable-clipboardはデフォルトでtになった。
;;(when (eq window-system 'x)
;;  (setq x-select-enable-clipboard t))

;;;; textmodes/artist.el
;; | key       | Picture   |
;; |-----------+-----------|
;; | C-c C-a P | polyline  |
;; | C-c C-a r | rectangle |
;; | C-c C-a V | vaporize  |
;; | C-c C-k   | kill-rect |
;; | C-c C-y   | yank-rect |
(global-set-key (kbd "C-c A") 'artist-mode)

;;;; textmodes/bibtex.el
;;
;; BibTeXファイルの基本構成
;;
;; | 文献フォルダ                | BibTeXフォルダ                   |
;; |-----------------------------+----------------------------------|
;; | ~/Documents/Books/          | ~/Bibliography/Documents/        |
;; | ~/Resources/Books/          | ~/Bibliography/Resources/        |
;; | ~/Dropbox/Books_and_Papers/ | ~/Bibliography/Books_and_Papers/ |
;; | 非デジタル文献              | ~/Bibliography/Books/            |
;; |-----------------------------+----------------------------------|
;; | 全ファイルの結合            | ~/Bibliography/all.bib           |
;;
;; - BibTeXディレクトリ場所は BIBINPUTS 環境変数で管理。
;; - BibTeXディレクトリの"~/Bibliography/" を除いた場所が実際の文献の場所。
;;   （ただしDropBoxのフォルダの場合はそれを除く。）
;; - 文献ディレクトリのサブディレクトリ名とBibTeXファイル名は一致。
;; - 全てのbibファイルは、"~/Bibliography/all.bib" に集める。
;;
;; - 文献のフォルダ内の移動に伴い、BibTeXファイルも自動更新する。
;;   (M-x my-update-bibtex-files)
;;
(defun my-collect-bibtex-files ()
  (shell-command
   (concat
    "cat ~/Dropbox/Bibliography/**/*.bib~all.bib"
    "  > ~/Dropbox/Bibliography/all.bib ")))

;; - BibTeX 基本要素について
;; |        | file-path                    | files                       |
;; |--------+------------------------------+-----------------------------|
;; | bibtex | bibtex-file-path             | bibtex-files                |
;; |        | BIBINPUTS 環境変数           |                             |
;; |        | bibtex-string-file-path      |                             |
;; |        | BIBINPUTS 環境変数           |                             |
;; | reftex | BIBINPUTS 環境変数           | reftex-default-bibliography |
;; |        |                              | → my-bibtex-files          |
;; | refer  | refer-bib-directory          | refer-bib-files             |
;; |        | →'bibinputs                 | → my-bibtex-files          |
;; | ebib   | ebib-preload-bib-search-dirs | ebib-preload-bib-files      |
;; |        | → my-bibtex-directories     | → my-bibtex-files          |

;; 一旦、全てのBibTeXファイルは、all.bib につなげる。
;; % cat *.bib~all.bib > all.bib

(defvar my-bibtex-file-path nil)
(defvar my-bibtex-directories nil)
(defvar my-bibtex-files nil)

(defun my-bibtex-setup ()
  "BibTeXファイルのセットアップ"
  (interactive)
  (setq my-bibtex-file-path (getenv "BIBINPUTS"))
  (setq my-bibtex-directories
        (and my-bibtex-file-path (split-string my-bibtex-file-path ":")))
  (setq my-bibtex-files
        (loop for dir in my-bibtex-directories
              nconc
              (let ((default-directory dir)) (file-expand-wildcards "*.bib"))))
  (setq my-bibtex-files (remove "all.bib" my-bibtex-files)))

(my-bibtex-setup)

;; - biblatex
;;
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
;;
;; BibLaTeXを使う。
(add-hook 'bibtex-mode-hook
          (lambda () (setq outline-regexp "[ \t]*\\(@\\|title\\)"
                           ;; fill はさせない。
                           fill-column 1000)))
(lazyload () "bibtex"
  ;; BibLaTeX の拡張
  (let ((misc (assoc-default "Misc" bibtex-biblatex-entry-alist))

        (types '("artwork" "audio" "bibnote" "commentary" "image" "jurisdiction"
                 "legislation" "legal" "letter" "movie" "music" "performance"
                 "review" "softare" "standard" "video" "map")))
    (dolist (type types)
      (add-to-list 'bibtex-biblatex-entry-alist
                   (cons type misc))))
  (bibtex-set-dialect 'biblatex)
  ;; BibLaTeX ファイルは、環境変数BIBINPUTSから取得。
  (setq bibtex-files 'bibtex-file-path
        ;; BibTeXの整形
        bibtex-entry-format
        '(opt-or-alts numerical-fields whitespace inherit-booktitle
          last-comma delimiters unify-case sort-fields)
        ;; `=' で揃えるか、値で揃えるか。
        bibtex-align-at-equal-sign nil
        bibtex-autofill-types nil
        bibtex-entry-alist bibtex-biblatex-entry-alist
        bibtex-field-indentation 2
        bibtex-include-OPTkey nil
        bibtex-search-entry-globally t
        bibtex-text-indentation 14)

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
;;
;; 厄介な問題：

;; bibtex-field-delimiters が、braces だと、abstractの中でカッコの対応
;; がとれていない場合（たとえば (...》など）エラーになる。稀に機械的に
;; 取得したデータにAbstractにそういうものが紛れ込むそのため、
;; double-quotes にする。
  (setq bibtex-field-delimiters 'double-quotes)
  (setq bibtex-user-optional-fields
        '(("file"   "PDF/DjVu File location (ignored)")
          ("access" "文献を入手・借入した日 (ignored)")
          ("library" "文献のある図書館・または書庫での位置 (ignored)")
          ("start" "文献を読み始めた日 (ignored)")
          ("finish" "文献を読み終えた日 (ignored)")
          ("review" "文献に対する批評・ノート (ignored)")
          ("ranking" "文献に対する評価（５段階） (ignored)")
          ("attribute" "文献の属性、フリーキーワード (ignored)")))) ;; end bibtex

;;;; textmodes/css-mode.el
(lazyload () "css-mode"
  (setq css-indent-offset 2)) ;; TAB値を8にしておく。

;;;; textmodes/flyspell.el
;; M-x flyspell-mode
;; C-, 移動 C-. 修正

;;;; textmodes/ispell.el
;; spell check は aspell で行う。
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]")))

;;;; textmodes/page-ext.el
;; ^L で区切られた領域をnarrowingして一覧表示・ジャンプ。
(autoload 'pages-directory "page-ext" "pages" t)
(global-set-key (kbd "C-c P") 'pages-directory)

;;;; textmodes/paragraphs.el
;; fill の際に文章の終わりはピリオド＋１スペースに認識させる。
(setq sentence-end-double-space nil)

;;;; textmodes/picture.el
;; picture の日本語に関する課題
;; - 日本語文字の挿入・削除の際に、カーソル後方の位置がずれる。(TODO)
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
(lazyload () "refer"
  (setq refer-bib-directory 'bibinputs ; BIBINPUTS環境変数ディレクトリ
        refer-bib-files 'dir))          ; ディレクトリ内の全 *.bib ファイル

;;;; textmodes/reftex.el
;; 色々と引用文献を楽に入力するようにする。
;; reftex-browse （スペースで該当Bibファイルにジャンプ）
(lazyload (reftex-browse my-reftex-wikipedia-reference) "reftex"
  (setq reftex-cite-format
        '((?b . "[[bib:%l][%l-bib]]")
          (?n . "[[note:%l][%l-note]]")
          (?t . "%t")
          (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[bib:%l][%l-bib]]")))
  (setq reftex-default-bibliography my-bibtex-files)
  (setq reftex-cite-format-builtin '((default "Default macro %t \\cite{%l}"
                                       "%t \\cite[]{%l}")))
  (setq reftex-cite-format 'default)
  (defun reftex-browse ()
    (interactive) (reftex-citation t))
  ;; Wikipedia 執筆用のreftex引用を用意する。
  (defun my-reftex-wikipedia-reference ()
    (interactive)
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
      (reftex-citation))))

;;;; textmodes/sgml-mode.el
(lazyload () "sgml-mode"
  (when (functionp 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)))

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
(eval-after-load "thingatpt"
  '(setq thing-at-point-url-regexp
         (mapcar (lambda (x) (push x thing-at-point-uri-schemes))
                 '("javascript:" "ttp:"))
         thing-at-point-url-regexp
         (concat "\\<\\("
                 (mapconcat 'identity thing-at-point-uri-schemes "\\|") "\\)"
          thing-at-point-url-path-regexp)))
(defadvice thing-at-point-url-at-point (after support-omitted-h activate)
  (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
    (setq ad-return-value (concat "h" ad-return-value))))

;;;; time.el
(eval-and-compile (display-time))
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))

;;;; time-stamp.el
;; バージョン管理に使用。
(add-hook 'before-save-hook 'time-stamp)

;;;; uniquify.el
;; バッファ名にディレクトリ名を追加する。
(eval-and-compile (require 'uniquify))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
(setq uniquify-min-dir-content 1)

;;;; vc/ediff.el
(lazyload () "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;;; vc/vc.el
(delete 'Bzr vc-handled-backends)
(delete 'Git vc-handled-backends)

;;;; view.el
;; 一週間以上古いファイルは、自動的にread-onlyにする。
(defun read-only-if-old-file ()
  (let ((modification-time
         (elt (file-attributes (buffer-file-name)) 5)))
    (when (and modification-time
               (< 6 (time-to-day-in-year
                     (time-subtract (current-time)
                                    modification-time)))
               ;; (org-mode) "_archive" で終わる名前は対象外
               (not (string-match "_archive$" (buffer-file-name)))
               ;; (org-feed) "feeds.org" を含む名前は対象外
               (not (string-match "feeds\\.org$" (buffer-file-name)))
               ;; (package) "-autoloads" を含む名前は対象外
               (not (string-match "-autoloads" (buffer-file-name)))
               ;; (bookmark+)
               (not (string-match "bookmarks" (buffer-file-name)))
               (not (string-match "emacs-bmk-bmenu" (buffer-file-name))))
      (read-only-mode))))
(add-hook 'find-file-hook 'read-only-if-old-file)
;; これがまずい場合は、以下の命令でキャンセルする。
;; (remove-hook 'find-file-hook 'read-only-if-old-file)

;;;; wdired.el
(autoload 'wdired-change-to-wdired-mode "wdired" nil t)
(lazyload () "dired"
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;;; windmove.el
;; Shift + ↑←↓→ で、移動。
(eval-and-compile (windmove-default-keybindings))
(setq windmove-wrap-around t)

;;;; window.el
(global-set-key (kbd "M-l") 'bury-buffer)
(defun my-other-window ()
  "ウィンドウが1つしかない場合は、過去のウィンドウ配置に戻るか、左
右・上下のいずれかに分割する。"
  (interactive)
  (when (one-window-p)
    (if (functionp 'winhist-backward) (call-interactively 'winhist-backward)
      (if (< (window-width) 140)
          (split-window-vertically)
        (split-window-horizontally))))
  (other-window 1))
(global-set-key (kbd "M-o") 'my-other-window)
(global-set-key (kbd "M-O") (command (other-window -1)))
(global-set-key (kbd "M-Q") (command (fill-paragraph 1)))
(setq split-window-preferred-function 'split-window-sensibly)
(setq split-height-threshold 80)
(setq split-width-threshold 160)
;(global-set-key "\M-y"
;  (lambda (arg) (interactive "p*")
;    (if (not (eq last-command 'yank))
;        (insert (x-get-cut-buffer 0))
;      (yank-pop arg))))
;; window操作
(global-set-key (kbd "C-S-n") 'enlarge-window)
(global-set-key (kbd "C-S-p") 'shrink-window)
(global-set-key (kbd "C-S-f") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-b") 'shrink-window-horizontally)
;;(global-set-key [C-right] (command (scroll-left 8)))
;;(global-set-key [C-left] (command (scroll-right 8)))
(global-set-key [M-up] (command (scroll-up 1)))
(global-set-key [M-down] (command (scroll-down 1)))
;(global-set-key [C-right] (command (scroll-left 1)))
;(global-set-key [C-left] (command (scroll-right 1)))
;; 現在のウィンドウを垂直方向に伸ばす。まず、下にウィンドウがあれば、
;; それを消して、無ければ、上を消して、上もなければ、
;; delete-other-windowsする。
(global-set-key (kbd "C-x 9")
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
;; 上下スクロールする際に、カーソルが追随するかどうかを切替える。
(defvar scroll-with-cursor nil)
(defun toggle-scroll-with-cursor ()
  (interactive)
  (if scroll-with-cursor
      (progn
        (setq scroll-with-cursor nil)
        (global-set-key (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
        (global-set-key (kbd "M-p") (lambda() (interactive) (scroll-down 1))))
    (global-set-key (kbd "M-n")
                    (lambda() (interactive) (scroll-up 1) (forward-line 1)))
    (global-set-key (kbd "M-p")
                    (lambda() (interactive) (scroll-down 1) (forward-line -1)))
    (setq scroll-with-cursor t)))
(toggle-scroll-with-cursor)

;;;; w32-ime.el
(declare-function w32-ime-initialize "w32-ime")
(declare-function ime-get-mode "w32-ime")
(declare-function ime-force-off "w32-ime")
(declare-function wrap-function-to-control-ime "w32-ime")
(defvar w32-ime-mode-line-state-indicator-list)
(defvar w32-ime-buffer-switch-pa)
(when (functionp 'w32-ime-initialize)
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  ;;(setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  ;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  ;; バッファ切り替え時にIME状態を引き継ぐ
  (setq w32-ime-buffer-switch-pa nil)
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
  ;; Todo:暗い背景と明るい背景で色を変えること
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
(lazyload () "woman"
  (setq woman-cache-filename "~/.wmncach.el"))
;;(defalias 'man 'woman)


;;; パッケージ管理システム
;; - 詳細は [[info:elisp#Packaging]] 参照。
;;;; 初期化
;;
;;   Emacsは init.el 読み込み後に各パッケージへのload-path設定を行い
;;   XXX-autoloads.el を読み込む。このままでは init の段階では
;;   require/locate-library ができないため、(package-initialize) を事前
;;   に実行する。
;;
(package-initialize)
(setq package-enable-at-startup nil) ;; 初期化済みなので自動初期化は停止。

;;   パッケージの情報は、~/.emacs.d/elpa/archives/ に格納される。自分
;;   でパッケージを作る場合は、 package-x.el の、
;;   `package-upload-{buffer,file}' を利用する。archive-contents ファ
;;   イルが自動生成製される。

;;;; Package Archives
;;
;; | archives   | MELPA                   | Marmalade            |
;; |------------+-------------------------+----------------------|
;; | source     | Public Repository       | Upload manually      |
;; | update     | Automatic               | Manual               |
;; | XXX-pkg.el | automatically generated | prepare by oneself   |
;; | version    | year-date-revision      | prepaed by oneself   |
;; | curaton    | relatively safe         | random fork possible |

;;;; Other Package Archives
;; SC ... Sunrise Commander
;; org ... org-mode
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun init-package-archives ()
  "Initialize package archives"
  (interactive)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")
          ;; sunrise-commander
          ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
          ;; org-mode
          ("org"   . "http://orgmode.org/elpa/")))
  ;; ローカルレポジトリを追加
  (when (file-exists-p "~/.emacs.d/local-packages/archive-contents")
    (add-to-list 'package-archives
                 '("local" . "~/.emacs.d/local-packages/") t)))
(init-package-archives)
(global-set-key (kbd "C-c p") 'list-packages)

;; Marmalade
;; marmalade は危険なファイルが入る可能性があるので、専用の関数で処理する。
;; 利用後は M-x init-package-archives して、もとに戻す。
(defun list-packages-marmalade ()
  (interactive)
  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
  (list-packages))

;; ノート :: 様々なパッケージ管理システム
;; TODO： これらの統合管理システムが欲しいかも。
;; | system      | packager      |
;; |-------------+---------------|
;; | Clojure     | Leiningen     |
;; | Common Lisp | QuickLisp     |
;; | Emacs       | Cask/ELPA     |
;; | Haskell     | Hackage       |
;; | Java        | Maven         |
;; | JavaScript  | npm/JSAN      |
;; | Lua         | LuaRocks      |
;; | Mac/BSD     | Ports         |
;; | OCaml       | GODI          |
;; | PHP         | Pecl/Pear     |
;; | Perl        | CPAN          |
;; | Python      | PyPi,PIP      |
;; | R           | CRAN          |
;; | RedHat      | yum           |
;; | Ruby        | RubyGems      |
;; | Scheme      | Eggs (chiken) |
;; | TeX         | CTAN          |
;; | Ubuntu      | apt-get       |

;; パッケージの load-pathの前に 個人のsite-lisp のpathを設定する。
(add-to-load-path "~/.emacs.d/site-lisp/")
;; 本来なら上記で、load-pathの先頭に site-lisp 以下のディレクトリが入る
;; はずだが、なぜか入らないので以下のように無理やり順序を入れ替える。
(let (site-lisp non-site-lisp)
  (dolist (path load-path)
    (if (string-match "/.emacs.d/site-lisp/" path) (push path site-lisp)
      (push path non-site-lisp)))
  (setq load-path (nconc (nreverse site-lisp) (nreverse non-site-lisp))))
;; load-path の理想的な順番
;; ( <site-lisp 以下> <elpa 関係> <標準elisp> )...

;; パッケージ読み込み後は、読み込みライブラリを表示する。
;; （繁雑な XXXX-autoload は表示させない。）
(setq force-load-messages t)

;;; テーマ
;; パッケージ読み込み後にテーマの確認
(defvar available-themes
  (cons nil (cl-set-difference
             (sort (custom-available-themes)
                   (lambda (x y) (string< (symbol-name x) (symbol-name y) )))
             ;; 不要なテーマ一覧
             '())))

(defun reset-theme ()
  (interactive)
  (mapc (lambda (th)
          (disable-theme th)) custom-enabled-themes))

(defun rotate-theme (&optional direction)
  (setq available-themes
        (if direction (list-rotate-backward available-themes)
          (list-rotate-forward available-themes)))
  (reset-theme)
  (if (null (car available-themes))
      (modify-frame-parameters nil default-frame-alist)
    (load-theme (car available-themes)))
  (update-fontset (face-attribute 'default :fontset)
                  (car font-sizes) font-specs)
  (message "theme=%s" (car available-themes)))

(defun rotate-theme-to (target)
  (when (member target available-themes)
    (setq available-themes (list-rotate-to available-themes target))
    (reset-theme)
    (load-theme (car available-themes))))

;; My favorite themes
(global-set-key (kbd "C-x M-b") (command (rotate-theme-to 'light-blue)))
(global-set-key (kbd "C-x M-m") (command (rotate-theme-to 'moe-light)))
(global-set-key (kbd "C-x M-d") (command (rotate-theme-to 'moe-dark)))



;;; 非標準ライブラリ設定
;;;; abc-mode (elpa)
;; auto-insert-mode 変数を autoload にしてエラーを引き起こすため
;; 使用中止（2013/07/08）

;;;; ac-ja (elpa)
;; 日本語自動補完
;; ホームディレクトリに SKK-JISYO.L のリンクを入れておく。

;;;; ac-js2 (elpa)
;; js2-mode に自動的にフックされる。

;;;; ac-math (elpa)
;; (add-to-list 'ac-modes 'latex-mode)

;;;; ace-jump-mode (elpa)
;; 画面中で高速に指定した場所に移動する。
(eval-and-compile
  (defun add-keys-to-ace-jump-mode (prefix c &optional mode)
    (global-set-key
     (kbd (concat prefix (string c)))
     `(lambda () (interactive)
        (funcall ',(if (eq mode 'word) 'ace-jump-word-mode 'ace-jump-char-mode)
                 ,c)))))

(lazyload
    ((loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "A-" c 'word))
     (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "A-" c 'word))
     ;; Meta+Alt+文字で、その文字に移動。
     ;;(loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "A-M-" c 'word))
     ;;(loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "A-M-" c 'word)))
     )
    "ace-jump-mode")

;;;; ack-and-a-half (elpa)
;; ag/helm-ag へ移行。

;;;; ag (elpa)
;; % sudo port install the_silver_searcher

;;;; agda2-mode
;; Mac では古い agda-mode が残っているので使わないように注意する。
;; % export PATH=$HOME/Library/Haskell/bin:$PATH
;; % cabal update
;; % cabal install cabal-install
;; % cabal install agda
;; トラブルシューティング
;; % ghc-pkg check
;; % sudo ghc-pkg recache
;; % cabal install --reinstall --force-reinstall <pkg-name>
(lazyload (agda2-mode) "agda2-mode"
  (add-to-list 'auto-mode-alist '("\\.l?agda\\'" . agda2-mode)))

;;;; all-ext (elpa)
(lazyload () "all"
  (require 'all-ext nil t))

;;;; alpha (elpa)
;; 透明度は90%に設定しておく。
;; (global-set-key (kbd "C-?") 'transparency-set-value)
;; the two below let for smooth transparency control
(lazyload
    (transparency-increase
     transparency-decrease
     (global-set-key (kbd "M-C->") 'transparency-increase)
     (global-set-key (kbd "M-C-<") 'transparency-decrease))
    "alpha"
  (transparency-set-value 90))

;;;; anaphora (elpa)
;; anaphoric macro は便利だが、これでelispコードを書くと、anaphoraがな
;; い環境で動作しなくなる。
; (require 'anaphora nil t)

;;;; ansi (elpa)

;;;; anthy
(when (and (or (eq window-system 'x) (null window-system))
           (locate-library "anthy")
           (executable-find "anthy-agent")
           (null (equal default-input-method 'japanese-mozc))
           (null (require 'uim-leim nil t)))
  (register-input-method "japanese-anthy" "Japanese"
                         'anthy-leim-activate "[anthy]"
                         "Anthy Kana Kanji conversion system")
  (lazyload () "anthy"
    (setq anthy-accept-timeout 1) ;; Emacs 22のみの設定を23でも行う。
    ;; leim-list.el 相当の動作を行う。
    (setq default-input-method 'japanese-anthy)))
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

;;;; asn1-mode
;(autoload 'asn1-mode "asn1" "Major mode to edit ASN.1/GDMO files." t )
;(add-to-list 'auto-mode-alist '("\\.mo$" . asn1-mode))

;;;; AUCTeX (elpa)
;; http://oku.edu.mie-u.ac.jp/~okumura/texfaq/auctex.html

;;;; auto-compile (elpa)
;; elisp を保存する際に自動的にコンパイルする。
(and (functionp 'auto-compile-mode)
     (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

;;;; auto-complete (elpa)
;; http://cx4a.org/software/auto-complete/manual.ja.html
(lazyload () "auto-complete-config"
  ;; (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
  ;; CSS3 properties の追加
  (mapc
    (lambda (entry) (add-to-list 'ac-css-property-alist entry))
    '(;; http://dev.w3.org/csswg/css-writing-modes/
      ("direction" "ltr" "rtl")
      ("text-combine-horizontal" "none" "all" integer)
      ("text-orientation" "mixed" "upright" "sideways-right" "sideways-left" "sideways"
       "use-glyph-orientation mixed")
      ("unicode-bidi" "normal" "embed" "isolate" "bidi-override" "isolate-override"
       "plaintext")
      ("writing-mode" "horizontal-tb" "vertical-rl" "vertical-lr")
      ;;http://dev.w3.org/csswg/css-text-decor-3
      ("text-decoration-line" "none" "underline" "overline" "line-through" "blink")
      ("text-decoration-color" color)))
  (ac-config-default))
(add-hook 'prog-mode-hook
          (lambda () (require 'auto-complete-config nil t)))

;;;; auto-save-buffers-enhanced (elpa)
;(when (functionp 'auto-save-buffers-enhanced)
;  (auto-save-buffers-enhanced))
;(defun auto-save-file-name-p (filename)
;  (or (string-match "^#.*#$" filename)
;      (string-match "\\.passwd$" filename)))

;;;; bbdb (elpa)
;; 人名・住所管理システム。
;;
;;;;; Recordの構造
;;
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
;;
;; xfield の例
;; | フィールド  |                         |
;; |-------------+-------------------------|
;; | www         | Web Home Page           |
;; | name-format | first-last / last-first |
;; | name-face   |                         |
;; | twitter     | ツイッター              |
;; | skype       |                         |
;;
;;;;; 名前のソート
;; BBDB での名前のソーティングを日本語ベースにする（kakasiを使用）
;; 全ての日本語（半角カナ含む）を「ひらがな」に直す関数
(defconst japanese-to-kana-buffer "*jp2kana*")
(defvar japanese-to-kana-process nil)
(defvar japanese-to-kana-hash (make-hash-table :test 'equal))
;; 以下の関数は青空文庫用のyasnippetでも使用する。
(defun japanese-to-kana-string (str)
  (if (null (executable-find "kakasi")) str)
  (when (null japanese-to-kana-process)
    (setq japanese-to-kana-process
          (start-process
           "kakasi" japanese-to-kana-buffer
           ;; kakasi に必ず "-u" (fflush) を入れておかないと、バッファリングして
           ;; 答えが返ってこなくなるので注意する。
           "kakasi" "-u" "-ieuc" "-oeuc" "-KH" "-JH" "-EH" "-kH")))
  (or (gethash str japanese-to-kana-hash)
      (let ((old-buffer (current-buffer)))
        (unwind-protect
            (progn
              (set-buffer japanese-to-kana-buffer)
              (set-buffer-process-coding-system 'euc-jp-unix 'euc-jp-unix)
              (erase-buffer)
              (process-send-string japanese-to-kana-process (concat str "\n"))
              (while (= (buffer-size) 0)
                (accept-process-output nil 0 50))
              (puthash str (substring (buffer-string) 0 -1)
                       japanese-to-kana-hash))
          (set-buffer old-buffer)))))

;;;;; カスタマイズ部分
(lazyload ((global-set-key (kbd "C-:") 'bbdb)
           bbdb-create) "bbdb-com"
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
  (setq bbdb-separator-alist
        (cons '(name-last-first "[ ,;]" " ")
              (assq-delete-all 'name-last-first bbdb-separator-alist)))
  (setq bbdb-file "~/.emacs.d/.bbdb.gpg")
  ;; 日本人主体なので、Last+Firstの順番で表示する。
  (setq bbdb-name-format 'last-first)
  (setq bbdb-mail-name-format 'last-first)
  ;; Gnus連携時、サマリバッファの横にBBDB情報を表示する。
  (setq bbdb-mua-pop-up 'horiz)
  ;; BBDBのポップアップ時のウィンドウサイズ。
  ;; 注：MUA使用時のサイズは別途、 `bbdb-mua-pop-up-window-size' で設定するので注意。
  (setq bbdb-pop-up-window-size 0.3) ;; 0.5 / 4
  ;; 写真画像は、パスに "Last, First.jpg" 名で入っている。
  (setq bbdb-image 'lf-name
        bbdb-image-path "~/.emacs.d/bbdb-images/")
  (setq bbdb-message-mail-as-name nil)
  ;; 全てのフィールドを表示する。
  (setq bbdb-layout 'full-multi-line)
  ;; NANP (north american numbering plan) は使用しない。
  (setq bbdb-phone-style nil)
  ;; 日本の住所フォーマット
  (setq bbdb-address-format-list
        '((("USA") "scSpC" "@%s\n@%c@, %S@ %p@\n%C@" "@%c@")
          (t bbdb-edit-address-japan bbdb-format-address-japan "@%c@")))
  (setq bbdb-default-country "Japan") ;; Japan, Emacs, etc
  ;; 日本の〒番号フォーマットを追加
  (add-to-list 'bbdb-legal-postcodes "^〒?[0-9]\\{3\\}-[0-9]\\{4\\}$")
  (bbdb-initialize 'gnus)
  (add-to-list 'auto-mode-alist '("\\.bbdb" . emacs-lisp-mode))
  (define-key bbdb-mode-map "\M-w" 'kill-ring-save)
  ;; (define-key bbdb-mode-map "O" 'bbdb-insert-new-field) ;; bbdb2
  (define-key bbdb-mode-map "O" 'bbdb-insert-field) ;; bbdb3
  (setq bbdb-complete-mail-allow-cycling t)
  ;; -------
  (defun my-bbdb-name-add-title (name-addr)
    (save-match-data
      (let* ((ml-title (string-match " ML\" <" name-addr)))
        ;; BBDB の名前の最後が " ML " で終わるならば、様は付けない。
        (if (and (null ml-title)
                 (string-match "\\(\".+?\\)\\(\" <.+\\)" name-addr))
            (concat (match-string 1 name-addr) " 様" (match-string 2 name-addr))
          name-addr))))
  ;; BBDBから名前に変換するとき、敬称を付加する。
  (defadvice bbdb-dwim-mail (after bbdb-dwim-add-title nil activate)
    (setq ad-return-value (my-bbdb-name-add-title ad-return-value)))
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
  (when (executable-find "kakasi")
    (defun bbdb-record-sortkey (record)
      (or (bbdb-cache-sortkey (bbdb-record-cache record))
          (bbdb-cache-set-sortkey
           (bbdb-record-cache record)
           (bbdb-japanese-sortkey record)))))

  (defadvice bbdb-rfc822-addresses
    (after remove-honorable-title last (&optional arg) activate)
    "This advice removes honorable titles from the result."
    (dolist (elem ad-return-value)
      (let ((name (car elem)))
        (if (and (stringp name) (string-match " ?様$" name))
            (setcar elem (substring name 0 (match-beginning 0))))))))

  ;; 実際にソートする場合は（一回で十分）、emacsを再起動（キャッシュを消
  ;; 去）して、.bbdbのバッファが無いのを確認した上で、
  ;; (bbdb-resort-database)を実行する。

;;;; bibeltex
;; ox-org を使い、
;; #+BIBLIOGRAPHY: example plain
;; を展開する。変換は、C-c C-e O O で確認可能。
;; * トラブルシューティング
;;   ox-org したときに、listp で array が帰るエラーが起きる場合は、
;;   ox.el のソースを読み込み直してみる。
(lazyload () "ox-org"
  (require 'bibeltex nil t))

;;;; bookmark+ (elpa)
;; |---------------+---------------------------------|
;; | C-x p m (r m) | bookmark-set                    |
;; | C-x p g (r b) | bookmark-jump                   |
;; | C-x p t + a   | bookmark-bmenu-list             |
;; | C-x p x       | bmkp-set-bookmark-file-bookmark |
;; | C-x p L       | switch to bookmark file         |
;; | C-x p e (r l) | bookmark-bmenu-list             |
;; |---------------+---------------------------------|
;; | T +           | bmkp-add-tags                   |
;; | M-d >         | bmkp-bmenu-dired-marked         |
;; |---------------+---------------------------------|
;; | C-x p H       | bmkp-light-bookmarks            |
;; |---------------+---------------------------------|
(lazyload ((add-hook 'find-file-hook 'bmkp-light-bookmarks)
           (global-set-key (kbd "M-E") 'bookmark-edit-annotation)) "bookmark+"
  (define-key bookmark-bmenu-mode-map "\M-o" nil)
  (setq bmkp-auto-light-when-set 'any-bookmark)
  (setq bmkp-auto-light-when-jump 'any-bookmark)
  (setq bookmark-use-annotations t)
  (setq bookmark-automatically-show-annotations t)
  (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
  (setq bmkp-bmenu-state-file "~/.emacs.d/.emacs-bmk-bmenu-state.el"))

;;;; boxquote (elpa)
;; 使用中止。→ rebox2 を利用

;;;; browse-kill-ring (elpa)
;; helm-kill-rings を使うので不要。

;;;; browse-url-dwim (elpa)
;; Context-sensitive external browse URL

;;;; c-eldoc (elpa)
(when (functionp 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;;;; caml (elpa)

;;;; calfw (elpa)

;;;; calfw-gcal (elpa)

;;;; cdlatex
(when (functionp 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'latex-mode-hook 'turn-on-cdlatex))
;; org-mode でも、M-x org-cdlatex-mode で利用可能。

;;;; clojure-mode (elpa)
;; http://github.com/jochu/clojure-mode

;;;; cmake-mode
;; http://www.cmake.org/CMakeDocs/cmake-mode.el
(lazyload (cmake-mode
           (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
           (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))) "cmake-mode")

;;;; cobol-mode
(lazyload (cobol-mode
           (add-to-list 'auto-mode-alist '("\\.cob" . cobol-mode)))
    "cobol-mode")

;;;; color-moccur (elpa)
;; http://www.bookshelf.jp/soft/meadow_49.html#SEC669
;; replace.el list-matching-lines → occur
;;            → color-moccur
(when nil ;;(locate-library "color-moccur")
  ;; occur を moccur 版に置き換える。
  (autoload 'occur-by-moccur "color-moccur" nil t)
  (autoload 'isearch-moccur "color-moccur" nil t)
  (autoload 'list-matching-lines "color-moccur" nil t)
  (defalias 'occur 'occur-by-moccur)
  ;; color-moccurのisearchのキーをキャンセル・再設定する。
  (define-key isearch-mode-map "\M-o" nil)
  (define-key isearch-mode-map "\M-O" 'isearch-moccur)
  ;(setq moccur-use-migemo t)
  ;(defun occur-outline ()
  ;  (interactive)
  ;  (cond ((eq major-mode 'emacs-lisp-mode) (occur "^;;;+ " nil))
  ;        ((eq major-mode 'sh-mode) (occur "^###+ " nil))
  ;        ((eq major-mode 'rd-mode) (occur "^=+ " nil))))
  ;; moccur-edit を使用する。（検索結果全てで編集が可能になる。）
  (when (locate-library "moccur-edit")
    (setq moccur-use-ee nil) ;; ee-autloadsは使用中止。
    (setq moccur-split-word t)
    (setq *moccur-buffer-name-exclusion-list*
          '(".+TAGS.+" "*Completions*" "*Messages*")))
  ;; dmoccur (directory moccur)
  (setq dmoccur-recursive-search t) ; ディレクトリを再帰的に検索
  ;; 開いた大量のバッファを片付ける。
  (global-set-key (kbd "C-c k") 'clean-dmoccur-buffers)
  ;;(setq dmoccur-use-list t)
  ;;(setq dmoccur-list
  ;;      '(
  ;;        ("dir" default-directory ("\\.el$") dir)
  ;;        ))
  ;;(define-key dired-mode-map "O" 'dired-do-moccur)
  ;;(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur))
  )

;;;; codepage 51932 設定
(require 'cp5022x nil t)
(when (coding-system-p 'cp51932)
  (define-coding-system-alias 'euc-jp 'cp51932))

;;;; ctable (elpa)
;; Table component for Emacs Lisp

;;;; cygwin-mount (elpa)
;; cygwin風のファイル名をWindows風に加えて使えるようにする。
(when (equal system-type 'windows-nt)
  (lazyload () "cygwin-mount"
    (setq cygwin-mount-cygwin-bin-directory
          (concat (getenv "CYGWIN_DIR") "\\bin"))
    (cygwin-mount-activate)))

;;;; d-mode (elpa)
;; http://prowiki.org/wiki4d/wiki.cgi?EditorSupport/EmacsEditor
;; http://www.emacswiki.org/emacs/FlyMakeD
;; auto-mode-alist などは autoloadで設定済み
(lazyload () "d-mode"
  ;; flymake
  (require 'flymake)
  (defun flymake-D-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "dmd" (list "-c" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.d$" flymake-D-init
                 flymake-simple-cleanup flymake-get-real-file-name))
  (add-to-list 'flymake-err-line-patterns
               '("^\\([^ :]+\\)(\\([0-9]+\\)): \\(.*\\)$" 1 2 nil 3)))

;;;; dabbrev-ja
;; 使用中止

;;;; dash (elpa)
;; !cons, !cdr, -each, -each-while, -dotimes, -map, etc.

;;;; deferred (elpa)

;;;; dict-tree (elpa)

;;;; dired+ (elpa)
(lazyload () "dired"
  (when (require 'dired+ nil t)
    ;; dired+が頻用キーを奪うのを無効化。
    (define-key dired-mode-map "\M-c" nil)
    (define-key dired-mode-map "\M-b" nil)
    (define-key dired-mode-map "\M-p" nil))
  ;; dired+は dired-mode-map に様々なキーをバインドする。
  ;; dired-mode-map の変更は dired+ の読み込み後に行う。
  (define-key dired-mode-map "\M-o" nil)
  (define-key dired-mode-map "\C-\M-o" 'dired-omit-mode))

;;;; doc-mode (elpa)

;;;; doxymacs
;; doxygen mode for emacs.
;;(if (locate-library "doxymacs")
;;    (require 'doxymacs))

;;;; durendal (elpa)
;; slime-clj が必要。
;; A bucket of tricks for Clojure and Slime.
;(when (require 'durendal nil t)
;  (durendal-enable))

;;;; e2wm (elpa)
;; simple window manager for emacs
(lazyload (e2wm:start-management
           (global-set-key (kbd "M-+") 'e2wm:start-management))
    "e2wm")

;;;; ebib (elpa)
;; browse-reftex で十分なので利用中止
;; [[info:ebib]]
;;(lazyload ((ebib "C-x M-b")) "ebib"
;;  (setq ebib-rc-file "~/.emacs.d/ebibrc.el"
;;        ebib-preload-bib-search-dirs my-bibtex-directories
;;        ebib-preload-bib-files       my-bibtex-files
;;        ebib-default-type 'book
;;        ebib-use-timestamp t
;;        ebib-layout 'custom
;;        ebib-index-display-fields '(title)
;;        ebib-biblatex-inheritance t
;;        ebib-sort-order '(sortname)
;;        ebib-additional-fields '(doi isbn jpno issn isrn ismn pbno
;;                                     plno usmarc ukmarc brno sici ndlcn))
;;  (ebib-key index "n" ebib-next-entry)
;;  (ebib-key index "p" ebib-prev-entry)
;;  (ebib-key index "N" ebib-search-next))

;;;; eclim (elpa)
;; https://github.com/senny/emacs-eclim
;; Installation : http://eclim.org/install.html
(lazyload (global-eclim-mode) "eclim"
  (help-at-pt-set-timer)
  (setq eclim-auto-save nil)
  (when (require 'ac-emacs-eclim-source nil t)
    (ac-emacs-eclim-config))
  ;;(when (and (require 'company nil t)
  ;;           (require 'company-emacs-eclim nil t))
  ;;  (company-emacs-eclim-setup)
  (require 'eclimd))

(lazyload nil "eclimd"
  (let ((workspace "~/Documents/workspace"))
    (when (file-directory-p workspace)
      (setq eclimd-default-workspace workspace))))

;;;; edbi (elpa)
;; Emacs Database Interface

;;;; ejacs
(lazyload (js-console) "js-console"
  ;; compatibility with old emacs (warning will appear)
  (with-no-warnings
    (defvar e float-e "The value of e (2.7182818...).")))
;;;; elscreen
;;(setq elscreen-prefix-key "\C-c\C-c") ; Old copy-to-register
;;(require 'elscreen nil t)

;;;; emmet-mode (elpa)
;; https://github.com/smihica/emmet-mode/blob/master/README.md
;; C-j :: emmet-expand-line
(lazyload () "emmet-mode"
  (setq emmet-indentation 2)) ; indent はスペース2個
(lazyload () "css-mode"
  (when (functionp 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))
(lazyload () "sgml-mode"
  (when (functionp 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)))
(lazyload () "web-mode"
  (when (functionp 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)))

;;;; enclose-mode (elpa)
;; 自動閉じ括弧挿入
;; しっくりと馴染まないので利用中止。
;; (enclose-global-mode 1)

;;;; ensime
;; * 注意
;;   2013.7 現在、きちんと動作しない。swankプロトコル不整合、というかバージョン非互換問題。
;;   パッケージ版もあるが、まだ色々と準備不足
;; * マニュアル
;;   http://aemoncannon.github.io/ensime/index.html
;; * ダウンロードサイト
;;   - https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases
;;     Scala-2.10 && 0.9.8.9
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
;;   - ~/.sbt/plugins/plugins.sbt に、'addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.1")' を記述。
;;     （ここの 0.1.1 の数字は https://github.com/aemoncannon/ensime-sbt-cmd を参照して適宜変更）
;;      2013.7 -- 0.1.2 にしても動作しないので注意
;; * 利用手順
;;   SBTプロジェクトの流儀に従いソースコードを配置し、フォルダ内で、
;;   % sbt "ensime generate"
;;   を実行し、その後、M-x ensime する。
;;
(lazyload () "scala-mode2"
  (when (require 'ensime nil t)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;;;; erefactor (elpa)
;; autoload で、フック・erefactor-map は設定されている。
(lazyload () "lisp-mode"
  (when (boundp 'erefactor-map)
    (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))

;; 
;;(add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
;;(add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)


;;;; ess (elpa)
;; Emacsの強力なRフロントエンド
;; org-babel の ob-R で使用。

;;;; esxml (elpa)
;; XML/XHTML の動的生成

;;;; evernote-mode
;; evernote-mode は、autoload時 に ruby へのアクセスをして（お行儀が悪
;; い）、rubyがインストールされていない環境では回避不可なエラーを出すの
;; で当面は利用中止。
;; http://emacs-evernote-mode.googlecode.com/svn/branches/0_21/doc/readme_ja.html
;; /bin の enclient.rb は、PATHのどこかに入れること。
;; EN_PROXY環境変数を設定しておくこと。
;;(when (locate-library "evernote-mode")
;;  (setq evernote-enml-formatter-command
;;        '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
;;  (require 'evernote-mode)
;;  (global-set-key "\C-cec" 'evernote-create-note)
;;  (global-set-key "\C-ceo" 'evernote-open-note)
;;  (global-set-key "\C-ces" 'evernote-search-notes)
;;  (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;  (global-set-key "\C-cew" 'evernote-write-note)
;;  (global-set-key "\C-cep" 'evernote-post-region)
;;  (global-set-key "\C-ceb" 'evernote-browser))

;;;; flymake-go (elpa)
;; flymake-0.4.13 を要求する（現在は0.3）ので中止

;;;; flymake-ruby (elpa)
;; M-x flymake-ruby-load
(lazyload () "ruby-mode"
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

;;;; flymake-shell (elpa)
(lazyload () "sh-script"
  (add-hook 'sh-set-shell-hook 'flymake-shell-load))

;;;; frame-cmds (elpa)
(lazyload (frame-to-right
           (global-set-key (kbd "C-c x") 'maximize-frame-vertically))
    "frame-cmds"
  (defun frame-to-right ()
    "現在のフレームの大きさを全画面の半分にして右に配置する。"
    (interactive)
    (maximize-frame)
    (set-frame-parameter nil 'width
                         (/ (frame-parameter nil 'width) 2))
    (call-interactively
     'move-frame-to-screen-right)))

;;;; frame-fns (elpa)
;; (required by frame-cmds)

;;;; gauche-manual
;; https://code.google.com/p/gauche-manual-el/
(lazyload (gauche-manual) "gauche-manual"
  (add-hook 'scheme-mode-hook
            (lambda ()
              (define-key scheme-mode-map "\C-c\C-f" 'gauche-manual))))

;;;; git-commit-mode (elpa)
;; elpa では、 magit 利用時に自動的に読み込まれる。

;;;; go-eldoc (elpa)
(lazyload () "go-mode"
  (when (functionp 'go-eldoc-setup)
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;;;; go-mode (elpa)
;; autoload で auto-mode-alist は自動設定
;; M-x helm-go-packages
(lazyload () "go-mode"
  (when (functionp 'helm-go-package)
    (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

;;;; google-this (elpa)
;;
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
(when (require 'google-this nil t)
  (google-this-mode 1)) ; マイナーモードの起動

;;;; google-translate (elpa)
(lazyload (google-translate-at-point google-translate-query-translate
           (global-set-key (kbd "C-c t") 'google-translate-at-point)
           (global-set-key (kbd "C-c T") 'google-translate-query-translate))
    "google-translate"
  (setq google-translate-default-target-language "French"
        google-translate-default-source-language "English"))

;;;; graphviz-dot-mode (elpa)
;; 'auto-mode-alist への追加は autoload 済

;;;; gtags (GNU Global) (elpa)
(lazyload () "gtags"
  (setq gtags-suggested-key-mapping t)
  (when (functionp 'gtags-mode)
    (add-hook 'c-mode-hook
              (command (gtags-mode t)))))

;;;; haml-mode (elpa)
;; HTMLやそのテンプレートをより見やすい構文で扱う。
;; RoRやmerbで扱える。

;;;; haskell-mode (elpa)
;; Haskell の関数名を、実際の記号に置換するfont-lock
;; * MacOS
;;   uninstall: /Library/Haskell/bin/uninstall-hs
(lazyload () "haskell-font-lock"
  (setq haskell-font-lock-symbols 'unicode))

;;;; helm (elpa)
;; * 解説等
;;   - https://github.com/emacs-helm/helm/wiki
;;   - http://d.hatena.ne.jp/syohex/20121207/1354885367
;;   - http://qiita.com/items/d9e686d2f2a092321e34
;; * helm のコマンド一覧
;; | M-X       |                                 | target                            | action |
;; |-----------+---------------------------------+-----------------------------------+--------|
;; | C-x C-f   | helm-find-files                 | file                              |        |
;; | f         | helm-for-files                  | file                              |        |
;; | C-c f     | helm-recentf                    | file                              |        |
;; | l         | helm-locate                     | file                              |        |
;; | /         | helm-find                       | file                              |        |
;; | C-x r b   | helm-bookmarks                  | file                              |        |
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
;; | ----      | helm-descbinds                  |                                   |        |
;; |           | helm-orgcard                    |                                   |        |
;; |           | helm-rails                      |                                   |        |
;; |           | helm-sheet                      |                                   |        |
;; |           | helm-spaces                     |                                   |        |
;; | T         | helm-themes                     |                                   |        |

(defvar helm-command-map) ;; compile-error 避け
(lazyload
    (
     (global-set-key (kbd "C-M-;") 'helm-recentf)
     (global-set-key (kbd "C-;") 'helm-for-files) ; <hcm> f
     (global-set-key (kbd "C-M-y") 'helm-show-kill-ring) ; <hcm> M-y
     ) "helm-config"
  ;; Helmキーマップの再割り当て
  (global-set-key (kbd "M-X") helm-command-map) ; <hcm>=helm-command-map
  ;; Infoファイルへジャンプ
  (define-key helm-command-map (kbd "h e") 'helm-info-elisp)
  (define-key helm-command-map (kbd "h c") 'helm-info-cl)
  (define-key helm-command-map (kbd "h o") 'helm-info-org))

(lazyload () "helm-net"
  (setq helm-google-suggest-use-curl-p (executable-find "curl"))
  (setq helm-google-suggest-search-url
        "http://www.google.co.jp/search?hl=ja&num=100&as_qdr=y5&lr=lang_ja&ie=utf-8&oe=utf-8&q=")
  (setq helm-google-suggest-url
        "http://google.co.jp/complete/search?ie=utf-8&oe=utf-8&hl=ja&output=toolbar&q="))

;; * helm のkey binnding の問題
;;   helm は、 (where-is-internal 'describe-mode global-map) で、
;;   describe-mode の全キーバインディングを取って、helm-key にマップす
;;   る。しかし、global-map で定義された prefix-key 付きのキーとhelmで
;;   定義済のキーが衝突すると、エラーになる。回避策として、global-map
;;   から一時的にhelp-map を除去し、読み込み後復活する。
(global-set-key (kbd "C-z") nil) ; 退避
(require 'helm nil t)
(require 'helm-config nil t)
(global-set-key (kbd "C-z") help-map) ; 復旧
;; * helm で quail を使うには
;;   helm は、override-keymaps を設定するがこれが設定されていると
;;   quail はキーイベントを素通ししてしまうので使えない。以下で修正。
;;
;; === modified file 'lisp/international/quail.el'
;; --- lisp/international/quail.el 2012-08-15 16:29:11 +0000
;; +++ lisp/international/quail.el 2013-01-21 13:17:01 +0000
;; @@ -1330,8 +1330,10 @@
;;
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

;;;; helm-ag (elpa)
(lazyload () "helm-config"
  (when (functionp 'helm-ag)
    (define-key helm-command-map "G" 'helm-ag)))

;;;; helm-c-moccur (elpa)
;; (lazyload (helm-c-moccur-occur-by-moccur) "helm-c-moccur"))
;;
;; (lazyload () "helm-config"
;;   (when (functionp 'helm-c-moccur-occur-by-moccur)
;;     (define-key helm-command-map "o" 'helm-c-moccur-occur-by-moccur)))

;;;; helm-c-yasnippet (elpa)
(lazyload (helm-c-yas-complete) "helm-c-yasnippet")
(lazyload () "helm-config"
  (when (functionp 'helm-c-yas-complete)
    (define-key helm-command-map "y" 'helm-c-yas-complete)))

;;;; helm-sys
(lazyload () "helm-sys"
  (when (eq system-type 'darwin)
    (setq helm-top-command "env COLUMNS=%s top -l 1")))

;;;; helm-descbinds (elpa)
;; describe-bindings をhelmで行なう。
(lazyload (describe-bindings) "helm-descbinds"
  (helm-descbinds-mode))
(lazyload () "helm-config"
  (when (functionp 'helm-descbinds-mode)
    (define-key helm-command-map "d" 'helm-descbinds)))

;;;; helm-git (elpa)
(lazyload () "helm-config"
  (when (functionp 'helm-git-find-files)
    (define-key helm-command-map "g" 'helm-git-find-files)))

;;;; helm-gtags (elpa)
(lazyload () "helm-config"
  (when (functionp 'helm-gtags-select)
    (define-key helm-command-map "\M-t" 'helm-gtags-select)))

;;;; helm-themes (elpa)
(lazyload () "helm-config"
  (when (functionp 'helm-themes)
    (define-key helm-command-map "T" 'helm-themes)))

;;;; hiwin
;; http://d.hatena.ne.jp/ksugita0510/20111223/p1
;; 現在のウィンドウをハイライトする。
;; 行をハイライトすれば、それでアクティブな画面が分かりやすいのでこれは不要か。
;;(require 'hiwin nil t)
;;(hiwin-activate) ;; (hiwin-deactivate)

;;;; howm
;; haskell-mode との衝突に注意。haskell-mode のあとで、howmを読み込むこと。
(lazyload (howm-list-all my-howm-concatenate-all-isearch
           (global-set-key (kbd "M-H") 'my-howm-concatenate-all-isearch)
           (add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))) "howm"
  ;; TITLEの変更（org-modeにも対応）
  (setq howm-directory "~/Dropbox/howm/"
        howm-view-title-header "#+TITLE:" ;; ← howm のロードより前に書くこと
        howm-view-title-regexp "^\\(\\(#\\+TITLE:\\)\\|=\\)\\( +\\(.*\\)\\|\\)$"
        howm-view-title-regexp-pos 4
        howm-view-title-regexp-grep "^\\(\\(#\\+TITLE:\\)\\|=\\) "
        howm-list-title t
        ;; 時間のかかるhowmのscanningは行わない。
        howm-menu-allow nil
        howm-menu-lang 'ja
        howm-list-all-title t
        howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
  ;; 一気に全てを繋げての表示を行う。
  (defun my-howm-concatenate-all-isearch ()
    (interactive)
    (howm-list-all)
    (howm-view-summary-to-contents)
    (isearch-forward))
  ;; org 式のメモをhowm で取る。
  (eval-after-load "org-capture"
  '(add-to-list
   'org-capture-templates
   '("h" "Howm式メモ" entry
     (function
      (lambda ()
        (let ((file
               (expand-file-name
                (format-time-string
                 "%Y/%m/%Y-%m-%d-%H%M%S.howm"
                 (current-time)) howm-directory)))
          (make-directory (file-name-directory file) t)
          (set-buffer (org-capture-target-buffer file)))))
     "* %t\n**%?"))))

;;;; html-script-src
;; jquery などのライブラリの最新版を素のHTMLソースに挿入する。
;;;; htmlize
;; htmlfontify があるがこちらが便利。
;;(setq htmlize-output-type 'font)
;;(setq htmlize-convert-nonascii-to-entities nil)
;;(setq htmlize-html-charset 'utf-8)

;;;; hfyview
;; htmlfontify を使って、バッファをブラウザで開く。（印刷向け）
;; commands `hfyview-buffer', `hfyview-region', `hfyview-frame'
(lazyload () "hfyview"
  (setq hfyview-quick-print-in-files-menu t))

;;;; IIMECF
;;(setq iiimcf-server-control-hostlist
;;      (list (concat "/tmp/.iiim-" (user-login-name) "/:1.0")))
;;(when nil ;(and (= 0 (shell-command
;;          ;          (concat
;;          ;            "netstat --unix -l | grep " (car iiimcf-server-control-hostlist))))
;;          ;   (require 'iiimcf-sc nil t))
;;  ;;(setq iiimcf-server-control-hostlist '("unix/:9010"))
;;  ;; IIIM server のデバッグには、rc.d/iiimd に、-d オプションと、>/tmp/iiim.debugへの出力を付加してrestart。
;;  ;; IIIM server に送出するusernameは通常は不要。
;;  ;;(setq iiimcf-server-control-username "kawabata")
;;  (setq iiimcf-server-control-default-language "ja")
;;  (setq iiimcf-server-control-default-input-method "atokx3") ;; atokx2 / wnn8(not work)
;;  (setq default-input-method 'iiim-server-control)
;;
;;  ;;(setq iiimcf-UI-lookup-choice-style 'buffer)
;;  ;; set kana keyboard
;;  ;;(setq iiimcf-current-keycode-spec-alist 'iiimcf-kana-keycode-spec-alist)
;;  (defun my-add-kana-map (key val)
;;    (setq iiimcf-kana-keycode-spec-alist
;;          (cons `(,key 0 ,val)
;;                (assq-delete-all key iiimcf-kana-keycode-spec-alist))))
;;  ;; かなの位置を、ASCII+AppleKeyboard風に。
;;  ;; １段
;;  (my-add-kana-map ?^ #xff6b) ; 「ぉ」を"^"に (US key)
;;  (my-add-kana-map ?& #xff6c) ; 「ゃ」を"&"に (US key)
;;  (my-add-kana-map ?* #xff6d) ; 「ゅ」を"*"に (US key)
;;  (my-add-kana-map ?\( #xff6e) ; 「ょ」を"("に (US key)
;;  (my-add-kana-map ?\) #xff66) ; 「を」を")"に (US key)
;;  (my-add-kana-map ?= #xff8d) ; 「へ」を"="に (US key)
;;  (my-add-kana-map ?\\ #xff70) ; 「ー」を"\"に (US key)
;;  (my-add-kana-map ?_ #xff70) ; 「ー」を"_"にも (original)
;;  ;; ２段
;;  (my-add-kana-map ?\[ #xff9e) ; 「濁音」を"["に (US key)
;;  (my-add-kana-map ?\] #xff9f) ; 「半濁音」を"]"に (US key)
;;  ;; ３段
;;  (my-add-kana-map ?: #xff91) ; 「む」を":"に (original)
;;  (my-add-kana-map ?' #xff79) ; 「け」を"'"に (US key)
;;  (my-add-kana-map ?\" #xff9b) ; 「ろ」を`"'に (apple style)
;;
;;  ;; 上記変更のalternative
;;  (setcdr (assoc 15 iiimcf-keycode-spec-alist) '(39)) ; C-o を 左矢印に
;;  (setcdr (assoc 9 iiimcf-keycode-spec-alist)  '(37)) ; C-i を 右矢印に。
;;  (setcdr (assoc 6 iiimcf-keycode-spec-alist)  '(39 nil 1)) ; C-f → S-right
;;  (setcdr (assoc 2 iiimcf-keycode-spec-alist)  '(37 nil 1)) ; C-b → S-left
;;
;;  ;;(iiimp-debug)
;;  )

;;;; imaxima
;; breqn.sty が必要なので注意すること。
(when (and (locate-library "imath")
           (executable-find "maxima")
           (executable-find "dvips"))
  (autoload 'imaxima "imaxima" "Image support for Maxima." t)
  (autoload 'imath-mode "imath" "Interactive Math minor mode." t))

;;;; image+ (elpa)
;; 画像の拡大・縮小（要ImageMagick）

;;;; idle-highlight-mode (elpa)
;; アイドル中に同じ部分文字列をハイライトする。
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;;;; inf-ruby (elpa)
;; M-x run-ruby

;;;; japanese-holidays
(lazyload () "calendar"
  (require 'japanese-holidays nil t))

;;;; jd-el
;; http://julien.danjou.info/google-maps-el.html
;;(when (locate-library "google-maps")
;;  (autoload 'google-maps "google-maps" "" t))

;;;; js2-mode (elpa)
(lazyload ((add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
    "js2-mode"
  ;(setq js2-consistent-level-indent-inner-bracket-p 1)
  ;(add-hook 'js2-mode-hook
  ;          '(lambda ()
  ;             (js2-indent-line js2-basic-offset) ))
  ;(add-hook 'js2-mode-hook
  ;          '(lambda ()
  ;             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
  ;; 後で確認
  ;(when (locate-library "smart-tabs-mode")
  ;  (add-hook js2-mode-hook 'smart-tabs-advice)
  )

;;;; js-comint (elpa)
;;(setq inferior-js-program-command
;;      (or (executable-find "js")
;;          (executable-find "rhino")))

;;;; js-doc
;; Document :: http://d.hatena.ne.jp/mooz/20090820/p1
(lazyload (js-doc-insert-function-doc
           js-doc-insert-tag)
    "js-doc"
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "KAWABATA Taichi <%s>" js-doc-mail-address)
        js-doc-url "http://github.com/kawabata/"
        js-doc-license "The MIT License"))

(lazyload () "js2-mode"
  (when (functionp 'js-doc-insert-function-doc)
    (add-hook 'js2-mode-hook
              '(lambda ()
                 (local-set-key "\C-ci" 'js-doc-insert-function-doc)
                 (local-set-key "@" 'js-doc-insert-tag)
                 ))))

;;;; keisen-mule
;; 罫線描画モード。Emacs 22以降は利用不可能。
;;(when nil ; (locate-library "keisen-mule")
;;  (if window-system
;;      (autoload 'keisen-mode "keisen-mouse" "MULE 版罫線モード + マウス" t)
;;    (autoload 'keisen-mode "keisen-mule" "MULE 版罫線モード" t)))

;;;; kill-summary
;; Emacs 22 からは、truncate-string を、truncate-string-to-width に変更。
;;(when (locate-library "kill-summary")
;;  (if (functionp 'truncate-string-to-width)
;;      (defalias 'truncate-string 'truncate-string-to-width))
;;  (autoload 'kill-summary "kill-summary" nil t))

;;;; less-css-mode (elpa)

;;;; lilypond-mode
(when (locate-library "lilypond-init" t)
  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
  (add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock))))

;;;; list-register
;; レジスタを見やすく一覧表示。
;;(when (locate-library "list-register")
;;  (autoload 'my-jump-to-register "list-register" "list-register." t)
;;  (autoload 'list-register "list-register" "list-register." t)
;;  (global-set-key "\C-c\C-r" 'data-to-resgister)
;;  (global-set-key "\C-xrj" 'my-jump-to-register)
;;  (global-set-key "\C-ci" 'list-register)
;;  )

;;;; lookup
;; Emacs終了時にLookupでエラーが出る場合は、
;;   (remove-hook 'kill-emacs-hook 'lookup-exit)
;; を実行する。
(lazyload (lookup-pattern lookup-word lookup-select-dictionaries
           lookup-list-modules lookup-restart
           (global-set-key (kbd "C-c M-/") 'lookup-pattern)
           (global-set-key (kbd "A-?") 'lookup-pattern)
           (global-set-key (kbd "C-c M-;") 'lookup-word)
           (global-set-key (kbd "M-\"") 'lookup-select-dictionaries)
           (global-set-key (kbd "M-'") 'lookup-list-modules )
           (global-set-key (kbd "C-c M-\"") 'lookup-restart)) "lookup"
  ;; emacsclient org-protocol:/lookup:/testimony
  ;; javascript:location.href='org-protocol://lookup://'+encodeURIComponent(window.getSelection())
  (eval-after-load "org-protocol"
    '(add-to-list 'org-protocol-protocol-alist
                  '("Lookup"
                    :protocol "lookup"
                    :function lookup-word))))

;;;; lua-mode (elpa)

;;;; mac-print-mode.el

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
    (add-to-list 'load-path uim-el)
    (add-to-list 'exec-path uim-bin)
    (add-to-list 'exec-path uim-libexec)
    (when (and (executable-find "uim-el-agent")
               (require 'uim-leim nil t)
               ;; uim-leim → uim
               ;; uim-init → uim-im-init で、uim-im-alist 更新。
               (assoc uim-im input-method-alist))
      (defvar uim-candidate-display-inline)
      (setq uim-candidate-display-inline t)
      (setq default-input-method uim-im))))
;; 注意。稀に、"uim-el-agent"があるにも関わらず、japanese-mozc-uim が
;; 動かない場合がある。(uim-im-alistが空。)
;; その場合は、MacUIMを再度インストールすること。

;;;; magic-buffer
(defun magic-buffer ()
  (interactive)
  (let ((try-downloading
         (lambda ()
           (let ((lexical-binding t))
             (with-current-buffer
                 (url-retrieve-synchronously
                  "https://raw.github.com/sabof/magic-buffer/master/magic-buffer.el")
               (goto-char (point-min))
               (search-forward "\n\n")
               (delete-region (point-min) (point))
               (setq lexical-binding t)
               (eval-buffer))))))
    (condition-case nil
        (funcall try-downloading)
      (error (funcall try-downloading))))
  (magic-buffer))

;;;; magit (elpa)
;; [[info:magit#Top]]
;; git のマージの方法
;; git remote add -f 2SC1815J git://github.com/2SC1815J/aozora-fix.git
;; git checkout -b 2SC1815J/fix
;; git pull 2SC1815J fix
;; git checkout fix
;; git merge 2SC2815J/fix
;; git push origin fix
(lazyload ((global-set-key (kbd "M-g s") 'magit-status)) "magit")

;;;; magithub

;;;; Malabar
;; （$ git clone https://github.com/espenhw/malabar-mode.git）
;; 上記は2012年現在、メンテナンスされていない。
;; $ git clone https://github.com/buzztaiki/malabar-mode
;; $ git pull
;; $ mvn package → 完成したファイルをsite-lisp へインストール
;;
;; * mvn は、version 3.04以降 を使うこと。
;; * emacs は、23.2 以降を使うこと。
;;(defvar malabar-dir
;;  (expand-file-name "~/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT"))
;;(when (file-directory-p malabar-dir)
;;  (add-to-list 'load-path (concat malabar-dir "/lisp")))
;;(when (locate-library "malabar-mode")
;;  (autoload 'malabar-mode "malabar-mode" nil t)
;;  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;;  (eval-after-load 'malabar-mode
;;    '(progn
;;       (require 'cedet)
;;       (setq malabar-groovy-lib-dir (concat malabar-dir "/lib"))
;;       ;; 普段使わないパッケージを import 候補から除外
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^java\\.awt\\..*$")
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^com\\.sun\\..*$")
;;       (add-to-list 'malabar-import-excluded-classes-regexp-list
;;                    "^org\\.omg\\..*$")
;;       ;; **** Malabar Groovy : Customization of malabar-mode's inferior Groovy.
;;       ;; 日本語だとコンパイルエラーメッセージが化ける
;;       (setq malabar-groovy-java-options '("-Duser.language=en"))
;;       (add-hook 'malabar-mode-hook
;;                 (lambda ()
;;                   (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                             nil t))))))

;;;; markchars (elpa)

;;;; markdown-mode (elpa)
;; 詳細は http://jblevins.org/projects/markdown-mode/
;; GitHub Flavored Markdown Mode (gfm-mode) にする。
(lazyload ((add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))
    "markdown-mode"
  (setq markdown-coding-system 'utf-8)
  (setq markdown-content-type " ")
  (setq markdown-command
        (or (executable-find "multimarkdown")
            (executable-find "markdown"))))

;;;; mediawiki (elpa)
;; リンクや文字修飾などのMediaWikiを編集するための便利機能が多数。
;; M-x mediawiki-draft
;; M-x mediawiki-draft-page
;; M-x mediawiki-draft-buffer
;;
;; M-x mediawiki-mode
;; M-x mediawiki-open


;;;; melpa (elpa)
;; パッケージ管理システム。ブラックリストパッケージの管理等。
;;(lazyload () "melpa"
;;  (add-to-list 'package-archive-exclude-alist '(("melpa" bbdb-vcard))))
;;(require 'melpa nil t)


;;;; mercurial
;; /opt/local/share/mercurial/contrib/mercurial.el
;; not well implemented.  We will use `monkey'.
;;(lazyload (hg-help-overview hg-log-repo hg-diff-repo) "mercurial")

;;;; mic-paren (elpa)
;; show-paren の拡張
;; エラーが頻発するので使用中止。
;;(when (functionp 'paren-activate)
;;  (paren-activate))

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
               '("/usr/share/cmigemo/utf-8"      ;; fedora
                 "/usr/local/share/migemo/utf-8" ;; cvs
                 "/usr/share/migemo")))          ;; ubuntu
(lazyload () "migemo"
  ;; 保存パターンファイルを設定すると、Emacs終了時に極端に重くなる。
  (setq migemo-pattern-alist-file nil)
  ;; 
  (setq migemo-directory (file-name-directory migemo-dictionary))
  (setq migemo-options '("-q" "--emacs")); "-i" "\a"))
  (setq migemo-user-dictionary nil)   ; nil with C/Migemo
  (setq migemo-regex-dictionary nil)  ; nil with C/Migemo
  (setq migemo-coding-system 'utf-8-unix)
  ;; ("-s" "_your_dict_path") で、辞書を追加可能。
  ;; 高速化のためにキャッシュを使う
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  ;; キャッシュの長さ
  (setq migemo-pattern-alist-length 1024)
  ;(defadvice isearch-yank-string
  ;(before migemo-off activate)
  ;"文字をバッファからコピーするときにはmigemo をオフにする。"
  ;(setq migemo-isearch-enable-p nil))
  ;(defadvice isearch-mode
  ;  (before migemo-on activate)
  ;  "isearch で検索する時にはmigemo をオンにする。"
  ;  ;;(setq migemo-isearch-enable-p t)) ; ちょっとmigemoをoffにする。キー一発で切り替えたいので、方法を考える。
  ;  (setq migemo-isearch-enable-p nil))
  ; Meadowyでは、isearchに入る前にfepをオフにする。
  ;(when (eq window-system 'w32)
  ;  (defadvice isearch-mode
  ;    (before fep-off activate)
  ;    (fep-force-off))
  ;  (defadvice isearch-edit-string
  ;    (after fep-off activate)
  ;    (fep-force-off))
  ;  (setq w32-pipe-read-delay 10))     ; default 50
  ;;; kogiku
  ;; ファイルを開く際 にmigemo を利用して補完できる。(M-k で小菊on。)
  ;(when (and (locate-library "migemo") (locate-library "kogiku"))
  ;  (eval-after-load 'migemo
  ;    '(require 'kogiku)))
  (migemo-init)
  )
(and migemo-command
     migemo-dictionary
     (require 'migemo nil t))

;;;; mmm-mode (elpa)
;; 使用中止。web-mode へ。

;;;; mode-info
;; ruby reference manual などは別途取得する。
;; emacs 23 で、 make install-index すると、
;; /usr/local/share/emacs/23.0.60/etc/mode-info/ にインデックスファイルが入る。
;;(if (file-exists-p "/usr/local/share/emacs/23.0.90/etc/mode-info/")
;;    (setq mode-info-index-directory "/usr/local/share/emacs/23.0.90/etc/mode-info/")
;;  (setq mode-info-index-directory "/usr/local/share/emacs/site-lisp/mode-info/"))
;;(when nil ;;(locate-library "mode-info")
;;  (autoload 'mode-info-describe-function "mode-info" nil t)
;;  (autoload 'mode-info-describe-variable "mode-info" nil t)
;;  (autoload 'mode-info-find-tag "mode-info" nil t)
;;  (define-key global-map "\C-hf" 'mode-info-describe-function)
;;  (define-key global-map "\C-hv" 'mode-info-describe-variable)
;;  (define-key global-map "\M-." 'mode-info-find-tag)
;;  (defadvice help-for-help
;;    (before activate-mi activate)
;;    (when (locate-library "mi-config")
;;      (require 'mi-config)
;;      (require 'mi-fontify))))

;;;; monky (elpa)
;; mercurial (hg) management
;; manual: http://ananthakumaran.in/monky/index.html
;; M-x monky-status
;; 'l' for log view.

;;;; moz
;; MozRepl との連携
;; C-c C-s: open a MozRepl interaction buffer and switch to it
;; C-c C-l: save the current buffer and load it in MozRepl
;; C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;; C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;; C-c C-r: send the current region to MozRepl
(lazyload (moz-minor-mode) "moz"
  (add-hook 'javascript-mode-hook 'javascript-custom-setup)
  (defun javascript-custom-setup ()
    (moz-minor-mode 1)))

;;;; mozc
(when (and (not (equal window-system 'mac))
           (or (require 'mozc "/usr/share/emacs/site-lisp/emacs-mozc/mozc.elc" t)
               (require 'mozc "/usr/share/emacs/site-lisp/emacs-mozc/mozc.el" t)
               (require 'mozc "/usr/share/emacs/site-lisp/mozc/mozc.elc" t))
           (executable-find "mozc_emacs_helper"))
  (setq default-input-method 'japanese-mozc))

;;;; multi-term (elpa)
;; (1) たくさんのバッファを同時に開くことができる。
;; (2) zshの補完機能とshell の標準出力編集機能を同時に使うことができる。
;; ただし、独自の`eterm-color'と呼ばれるターミナルを使うので、以下のコ
;; マンドを事前に実行しておく。
;; % tic -o ~/.terminfo ~/cvs/emacs/etc/e/eterm-color.ti
;; 環境変数：TERM=eterm
(lazyload () "multi-term"
  (add-hook 'term-mode-hook
         (lambda ()
           ;; C-h を term 内文字削除にする
           (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
           ;; C-y を term 内ペーストにする
           (define-key term-raw-map (kbd "C-y") 'term-paste)
           ;; 幾つかのキーをバインドから外す。
           (add-to-list 'term-unbind-key-list "M-x")
           (add-to-list 'term-unbind-key-list "M-v")
           (add-to-list 'term-unbind-key-list "M-o")
           (add-to-list 'term-unbind-key-list "C-o")
           (add-to-list 'term-unbind-key-list "C-e")
           (add-to-list 'term-unbind-key-list "M-f"))))

;;;; multiple-cursors (elpa)
;; カーソルを分身させ同時に同じ環境で編集できる。
;; - Emacsでリファクタリングに超絶便利なmark-multiple
;;   http://d.hatena.ne.jp/tuto0621/20121205/1354672102
;;   http://emacsrocks.com/e13.html
(lazyload 
    ((global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (global-set-key (kbd "C-c M-8") 'mc/mark-all-like-this))
    "multiple-cursors")

;;;; mustache (elpa)
;; 現実問題として、テンプレート処理は elisp のbackquoteを使ったほうが早い。
;; my-org-publish-gepub-script を参照のこと
;; (cl-letf ((,@ ...)))

;;;; nav
;; http://d.hatena.ne.jp/wocota/20091001/1254411232
;; ディレクトリナビゲータ
(when (locate-library "nav")
  (autoload 'nav-toggle "nav" "nav" t nil))

;;;; navi
;; navi はバッファの概略を表示するパッケージ。
;; outline-mode で代替できるので不要。
;(when (locate-library "navi")
;  (autoload 'navi "navi" "navi." t nil)
;  (global-set-key [f11]  'call-navi)
;  (global-set-key "\C-x\C-l" 'call-navi)
;  (defun call-navi ()
;    (interactive)
;    (navi (buffer-name))))

;;;; navi2ch
;; * インストール
;;   % cp *.el *.elc ~/.emacs.d/site-lisp/navi2ch/
;;   % cp icons/* ~/.emacs.d/etc/navi2ch/icons
;; * 板の追加方法 :: ~/.emacs.d/navi2ch/etc.txt に以下を記入
;;     板の名前
;;     板の URL
;;     板の ID
;; * JBBS
;;   URLに jbbs.livedoor.jp が入っていれば、 navi2ch-jbbs-shitaraba.el で処理
(lazyload (navi2ch) "navi2ch"
  (setq navi2ch-directory (expand-file-name "~/.emacs.d/navi2ch"))
  (autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
  (setq navi2ch-icon-directory (expand-file-name "~/.emacs.d/etc/navi2ch/icons"))
  ;; 検索結果の最大取得数（デフォルトは30）
  (setq navi2ch-search-find-2ch-search-num 50)
  ;; モナーフォントを使用する。
  ;; navi2ch-mona-setup.el で、mac でも使えるように変更が必要。
  (setq navi2ch-mona-enable t
        navi2ch-mona-use-ipa-mona t)
  (when (find-font (font-spec :family "IPAMonaPGothic"))
    (setq navi2ch-mona-ipa-mona-font-family-name "IPAMonaPGothic"))
  ;; 既読スレはすべて表示
  (setq navi2ch-article-exist-message-range '(1 . 1000))
  ;; 未読スレもすべて表示
  (setq navi2ch-article-new-message-range '(1000 . 1))
  ;; oyster のパスワード等は別ファイルで暗号化しておく。
  (when (and (executable-find "gpg")
             (locate-library "navi2ch-oyster-setup.el.gpg"))
    (load-library "my-navi2ch-oyster-setup.el.gpg")))

;;;; nethack
;; "6x10" フォントを要求するので当面コメントアウトする。
;;(lazyload (nethack) "nethack"
;;  (when (executable-find "jnethack")
;;    (add-to-list 'process-coding-system-alist
;;                 '("nethack" euc-jp . euc-jp))
;;    (setq nethack-program "jnethack")))

;;;; nxhtml-mode
;; 本ディレクトリには".nosearch"があるので、本来は読み込めない。
;(when (locate-library "nxhtml/autostart")
;  (load-library "nxhtml/autostart"))

;;;; nyan-mode (elpa)

;;;; oneliner
; http://oneliner-elisp.sourceforge.net
;(require 'oneliner nil t)

;;;; org
;;;;; org/org
;;
;; * 基本記法
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
(setq load-path
      (cl-delete-if (lambda (x) (or (string-match "/elpa/org-20" x)
                                    (string-match "/lisp/org" x))) load-path))
;;
;; ".org.txt" も org-mode 管理にする。
(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))
;; 基本グローバル４＋２コマンド
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;; [[info:org#External links][org link]]
(global-set-key (kbd "C-c O") 'org-open-at-point-global) ;; follow link
(global-set-key (kbd "C-c L") 'org-insert-link-global)
;;
;; org-mode で使用する数学記号パッケージの一覧
;; 一覧は http://milde.users.sourceforge.net/LUCR/Math/unimathsymbols.pdf 参照
;;
(lazyload () "org"
  ;; orgモジュール （org-mhe org-rmail は不要なので削除）
  (setq org-modules '(org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc))
  ;; Shiftキーでまとめて選択しての処理を可能にする。
  (setq org-support-shift-select t)
  (setq org-ellipsis "↓")
  ;; org-caputure/org-mobile 等が使用するデフォルトディレクトリ
  (setq org-directory "~/Dropbox/org")
  ;; org-bullets
  (when (require 'org-bullets nil t)
    (add-hook 'org-mode-hook 'org-bullets-mode))
  ;; C-o 回避
  (define-key org-mode-map "\C-c\M-o" 'org-open-at-point)
  ;; #+STARTUP: indent 相当。自動的にインデントする。必須。
  (setq org-startup-indented t)
  (setq org-directory "~/org/")
  ;; テンプレートについては、すでに org-structure-template-alist で定義
  ;; されている。
  (setq org-hide-leading-stars t)
  (setq org-footnote-tag-for-non-org-mode-files "脚注:")
  (setq org-todo-keywords '((sequence "TODO" "URGENT" "WAIT" "DONE")))
  (setq org-format-latex-options
        '(:foreground default :background default :scale 1.5
          :html-foreground "Black" :html-background "Transparent"
          :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  ;; インライン画像の幅は、#ATTR画像で設定する。
  (setq org-image-actual-width nil)
  ;; * org-preview-latex-fragment (数式の画像化 C-c C-x C-l) について
  ;;   現在のorg-mode は、直接 latex 命令で数式を生成する
  ;;   "org-create-formula-image-with-dvipng" と、LaTeXからPDFを生成して
  ;;   そこからImageMagickで画像を生成する
  ;;   "org-create-formula-image-with-imagemagick" の２つの方法が用意さ
  ;;   れている。そのうち、..-with-dvipng は、直接 latex 命令を呼び出している。
  ;;   imagemagick は動作が遅い。
  (setq org-latex-create-formula-image-program 'dvipng) ; imagemagick
  ;; * HTML出力の際は 自動的に色付けをする。
  (setq org-src-fontify-natively t)
  ;; * 数式を生成する際に、 `latex' 命令で利用できるパッケージを仮設定す
  ;;   るアドバイス
  (defadvice org-create-formula-image-with-dvipng
    (around org-reset-default-packages activate)
    (let ((org-export-latex-default-packages-alist
           `(;;("" "fixltx2e" nil)
             ,@my-org-latex-math-symbols-packages-alist
             "\\tolerance=1000"))
          (org-export-latex-packages-alist nil))
      ad-do-it))
  (ad-activate 'org-create-formula-image-with-dvipng)
  ;; glyphwiki リンクの設定（ローカルにあればそれを参照してもよい。）
  ;;(add-to-list 'org-link-abbrev-alist
  ;;            ("glyphwiki" . "http://glyphwiki.org/wiki/%s"))
  ;; org-agenda
  (setq org-agenda-files
        (list (expand-file-name "work" org-directory)
              (expand-file-name "home" org-directory)))
  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\.txt$")
  ;; org-clock
  (org-clock-persistence-insinuate))

;;;;; org/org-agenda

;;;;; org/org-capture
(lazyload () "org-capture"
  ;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
  (setq org-capture-templates
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
           "\n- %?\n\n" :prepend t))))

;;;;; org/org-clock
;; C-c C-x C-i     (org-clock-in)
;; C-c C-x C-o     (org-clock-out) ;; C-o が衝突するためM-oに変更
;; C-c C-x C-x     (org-clock-in-last)
;; C-c C-x C-e     (org-clock-modify-effort-estimate)
(lazyload () "org-clock"
  (org-defkey org-mode-map (kbd "C-c C-x M-o") 'org-clock-out)
  (setq org-clock-persist 'history))

;; 先頭が "*" でなく、改行の前後が日本の文字の場合はその改行を除去する。
;; org-mode で利用する。
;; U+3000 以降の文字同士が改行で分割されていた場合は改行を削除する。
;; XeTeX/LuaTeX や HTML, DocBook 等、日本語の改行が空白扱いになる箇所で利用する。
(defun remove-newlines-at-cjk-text (&optional _mode)
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^*\n].+\\)\\(.\\)\n *\\(.\\)" nil t)
    (if (and (> (string-to-char (match-string 2)) #x2000)
               (> (string-to-char (match-string 3)) #x2000))
        (replace-match "\\1\\2\\3")
      (replace-match "\\1\\2 \\3"))
    (goto-char (point-at-bol))))

;; org-mode のテキストを PowerPoint などにコピーする際に
;; キルリングに入る日本語の改行・空白を削除する。
(defun remove-newlines-at-cjk-kill-ring-save (from to)
  (interactive "r")
  (let ((string (buffer-substring from to)))
    (with-temp-buffer
      (insert string)
      (remove-newlines-at-cjk-text)
      (copy-region-as-kill (point-min) (point-max))))
  (if (called-interactively-p 'interactive)
      (indicate-copied-region)))

(global-set-key (kbd "M-W") 'remove-newlines-at-cjk-kill-ring-save)

;;;;; org/org-feed
;; RSSリーダ
;; M-x org-feed-update-all
;; M-x org-feed-update
;; M-x org-feed-goto-inbox
(lazyload () "org-feed"
  (setq org-feed-retrieve-method 'wget) ; wget でフィードを取得
  ;; テンプレート
  (setq org-feed-default-template "\n* %h\n  - %U\n  - %a  - %description")
  ;; フィードのリストを設定する。
  (setq my-org-feeds-file (expand-file-name "feeds.org" org-directory))
  (setq org-feed-alist
        `(
          ("Slashdot"
           "http://rss.slashdot.org/Slashdot/slashdot"
           ,my-org-feeds-file
           "Slashdot" ))))

;;;;; org/org-mobile
(lazyload () "org-mobile"
  (setq org-mobile-directory "~/Dropbox/MobileOrg"))

;;;;; org/org-protocol
;; 設定方法：(http://orgmode.org/worg/org-contrib/org-protocol.html)
;;
;; - 仕組み
;;
;; (1) OS で、 "org-protocol:" というプロトコルを設定し、起動アプリケー
;;     ションを emacsclient にする。
;; (2) Emacs側で (require 'org-protocol) する。
;; (3) ブラウザ側で、org-protocol:/sub-protocol: でEmacs Client に接続する。
;;
;; - org-protocol:/org-protocol-store-link:  ...
;;     javascript:location.href='org-protocol://store-link://'+encodeURIComponent(location.href)
;; - org-protocol:/org-protocol-capture:     ...
;;     javascript:location.href='org-protocol://capture://'+
;;           encodeURIComponent(location.href)+'/'+
;;           encodeURIComponent(document.title)+'/'+
;;           encodeURIComponent(window.getSelection())
;; - org-protocol:/org-protocol-open-source: ...
;;
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
;;
;; - Windows … レジストリファイルを作成して起動。
;;
;; org-protocol-capture （選択テキストをorg-mode にキャプチャ）
;; org-protocol-store-link （選択リンクをブックマークとしてコピー）
(lazyload (org-protocol-create) "org-protocol")

;;;;; org/ob
;; ソースコードの編集は C-c '
;; C-c で評価し、結果は #+RESULTに置かれる。
(defvar org-ditaa-jar-path "~/.emacs.d/program/jditaa.jar")

(lazyload () "ob"
  (setq ;; org-babel-do-load-languages
   org-babel-load-languages
   `((C . t) (css . t) (emacs-lisp . t)
     ,@(and (file-exists-p org-ditaa-jar-path)
           '((ditaa . t)))
     ,@(and (executable-find "zsh")
            '((sh . t)))
     ,@(and (executable-find "clj")
            '((clojure . t)))
     ,@(and (executable-find "dot")
            '((dot . t)))
     ,@(and (executable-find "ghc")
            '((haskell . t)))
     ,@(and (executable-find "gnuplot")
            '((gnuplot . t)))
     ,@(and (executable-find "node")
            '((js . t)))
     ,@(and (executable-find "javac")
            '((java . t)))
     ,@(and (executable-find "latex")
            '((latex . t)))
     ,@(and (executable-find "lilypond")
            (locate-library "lilypond-init" t)
            (add-to-list 'org-src-lang-modes
                         '("lilypond" .LilyPond))
            '((lilypond . t)))
     ,@(and (executable-find "mscgen")
            '((mscgen . t)))
     ,@(and (executable-find "R")
            (require 'ess nil t)
            '((R . t)))
     ,@(and (executable-find "ruby")
            '((ruby . t)))
     ,@(and (executable-find "sqlite")
            '((sqlite . t))))))

;;;;; org/ox
;; html (deck/s5/md) → (pandoc)
;; odt (odf) → (pandoc)
;; latex (beamer) → (pandoc)
;; icalendar
;; taskjuggler
;; (pandoc) → docbook, latex, mediawiki, etc. etc...

(lazyload () "ox"
  (add-hook 'org-export-before-processing-hook 'remove-newlines-at-cjk-text))

;;;;; org/ox-freemind
(lazyload () "ox-freemind"
  (setq
   ;; org-export-headline-levels 6 ;; #+OPTIONS H:6
   org-freemind-styles
   '((default . "<node>\n</node>")
     (0 . "<node COLOR=\"#000000\">\n<font NAME=\"SansSerif\" SIZE=\"20\"/>\n</node>")
     (1 . "<node COLOR=\"#0033ff\" FOLDED=\"true\">\n<edge STYLE=\"sharp_bezier\" WIDTH=\"8\"/>\n<font NAME=\"SansSerif\" SIZE=\"18\"/>\n</node>")
     (2 . "<node COLOR=\"#00b439\" FOLDED=\"true\">\n<edge STYLE=\"bezier\" WIDTH=\"thin\"/>\n<font NAME=\"SansSerif\" SIZE=\"16\"/>\n</node>")
     (3 . "<node COLOR=\"#990000\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (4 . "<node COLOR=\"#0033ff\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (5 . "<node COLOR=\"#00b439\" FOLDED=\"true\">\n<font NAME=\"SansSerif\" SIZE=\"14\"/>\n</node>")
     (6 . "<node COLOR=\"#111111\">\n</node>"))
   org-freemind-section-format 'inline))

;;;;; org/ox-html
;; org-html-head
(lazyload () "ox-html"
  ;; (setq org-html-head
  ;; "<link rel='stylesheet' type='text/css' href='style.css' />")
  ;; org-html-head-extra
  ;; org-html-preamble-format
  ;; org-html-postamble-format
  (setq org-html-postamble t)
  (setq org-html-postamble-format
      '(("en" ""))))

;;;;; org/ox-ical
;; M-x org-icalendar-combine-agenda-files

;;;;; org/ox-latex
;;
;; TeXはMacPortsではなく、MacTeXからインストールする。
;;
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

(lazyload () "ox-latex"
  (defvar my-org-latex-math-symbols-packages-alist
    '(("" "amssymb"   t)
      ("" "amsmath"   t)
      ("" "amsxtra"   t) ; MathJax未対応
      ;;("" "bbold"     t)
      ("" "isomath"   t) ; MathJax未対応
      ("" "latexsym"  t) ; MathJax未対応
      ("" "marvosym"  t) ; Martin Vogel's Symbols Font
      ;;("" "mathdots"  t) ; MathJax未対応
      ("" "stmaryrd"  t) ; MathJax未対応
      ("" "textcomp"  t) ; 特殊記号
      ("" "wasysym"   t); Waldi symbol font.
      ))

  (defun my-org-export-latex-setup (latex)
    (interactive
     (list (intern (completing-read "engine for org-latex=? "
                                    '("luatex" "xetex" "euptex")))))
    ;; すべてのLaTeX出力に共通なパッケージ
    (setq
     org-latex-default-packages-alist
     `(
       ;; 各 TeX に応じた日本語パッケージ設定
       ,@(case latex
           ('luatex '(("" "luacode" t)
                      ("" "luatexja-otf" t)))
           ('xetex  '(;; noCJKchecksiingle で、\meaning の非BMPでの分割を抑止
                      ("AutoFallBack=true,noCJKchecksingle" "zxjatype" t)
                      ;;("macros" "zxotf" t)
                      ))
           ('euptex '(("uplatex,multi" "otf" t)
                      ("" "okumacro" t)))
           (t nil))
       ;; 互換性より利便性を重視したLaTeX2eのバグ修正
       ("" "fixltx2e" nil)
       ;; Verbatimで枠線を綺麗に出力
       ("" "fancyvrb" t)
       ;; ページをまたがるテーブルの作成
       ("" "longtable" nil)
       ("" "float" nil)
       ;; ("" "wrapfig" nil) ; figureをwrapする。
       ;; ("" "soul" t) ; ドイツ語用
       ;; LaTeX標準文字記号マクロ
       ,@my-org-latex-math-symbols-packages-alist
       ;;("" "tabulary" t)
       ("" "bigtabular" t)
       ("" "multicol" t)
       ;; その他のデフォルトで使用するLaTeX設定
       ,(concat
         "\\tolerance=1000\n"
         "\\providecommand{\\alert}[1]{\\textbf{#1}}\n"
         "\\fvset{xleftmargin=2em}\n")
       ;; XeTeXの場合、1.9999 以降は IVSが使えるので、これらに glue が
       ;; 入らないよう、キャラクタクラスを 256 にする。
       ,(when (equal latex 'xetex)
          (concat
           "\\setjamainfont{HanaMinA}\n"
           "\\setCJKfallbackfamilyfont{rm}{HanaMinB}\n"
       ;;    "\\XeTeXcharclass\"E0100=256\n"
       ;;    "\\XeTeXcharclass\"E0101=256\n"
       ;;    "\\XeTeXcharclass\"E0102=256\n"
       ;;    "\\XeTeXcharclass\"E0103=256\n"
       ;;    "\\XeTeXcharclass\"E0104=256\n"
       ;;    "\\XeTeXcharclass\"E0105=256\n"
       ;;    "\\XeTeXcharclass\"E0106=256\n"
       ;;    "\\XeTeXcharclass\"E0107=256\n"
       ;;    "\\XeTeXcharclass\"E0108=256\n"
       ;;    "\\XeTeXcharclass\"E0109=256\n"
           ))
       ))

    ;; 一部のLaTeX出力に特有のパッケージ（Beamerで使わないパッケージ）
    (setq
     org-latex-packages-alist
     `(
       ;; graphicx: jpeg や png を取り込む。
       ;;   ebb *.png 命令を実行しないと Bounding Boxが生成されない。
       ,(case latex
          ('xetex  '("" "graphicx"  t))
          ('euptex '("dvipdfm" "graphicx"  t))
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
       ;; で入れるのがいいかも。
       ;; ("backend=biber", "biblatex" t)
       ;; ↓これを入れると、includegraphics で png が入らないので注意。
       ("" "listings")
       ("" "color")))

    (setq
     org-latex-classes
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
        ,(concat "\\documentclass[compress,dvipdfm]{beamer}\n[NO-PACKAGES]\n"
                 "\\usepackage{graphicx}\n")
        org-beamer-sectioning)))

    (setq
     org-latex-pdf-process
     (case latex
       ('luatex '("latexmk -pdf"))
       ('xetex  '("latexmk -pdf"))
       ('euptex '("latexmk -pdfdvi"))
       (t '("latexmk -pdfdvi")))))
  ;; euptex, xetex, luatex
  (my-org-export-latex-setup 'xetex)

  ;; "verbatim" → "Verbatim" 置換
  (defun org-latex-filter-fancyvrb (text backend _info)
    "Convert begin/end{verbatim} to begin/end{Verbatim}.
Allows use of the fancyvrb latex package."
    (when (or (org-export-derived-backend-p backend 'beamer)
              (org-export-derived-backend-p backend 'latex))
      (replace-regexp-in-string
       "\\\\\\(begin\\|end\\){verbatim}"
       "\\\\\\1{Verbatim}" text)))
  (add-to-list 'org-export-filter-final-output-functions
               'org-latex-filter-fancyvrb)

  ;; "tabular" → "Tabular" 置換
  (defun org-latex-filter-bigtabular (text backend _info)
    "Convert begin/end{tabular} to begin/end{Tabular}."
    (when (or (org-export-derived-backend-p backend 'beamer)
              (org-export-derived-backend-p backend 'latex))
      (replace-regexp-in-string
       "\\\\\\(begin\\|end\\){tabular}"
       "\\\\\\1{Tabular}" text)))
  (add-to-list 'org-export-filter-final-output-functions
               'org-latex-filter-bigtabular))

;;;;; org/ox-odt.el
;; 出力されたodt を、MacのTextEditで開くとぐちゃぐちゃになる。
;; → Windows で開く。
;; → pandoc -o XXX.docx XXX.odt で変換する。
;; pandoc は、/usr/local/bin に入れる。（MacPortsは利用しない。）
;; % ports deps package-name
;; デフォルト出力ファイルのフォントの変更
;; →　MS Gothic SimSun → MS Mincho
;;(eval-after-load 'org-odt
;;  '(let ((style-dir "~/.emacs.d/etc/org/"))
;;     (if (file-directory-p style-dir)
;;         (setq org-odt-styles-dir style-dir))))

;;;;; org/ox-publish
(lazyload () "ox-publish"
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
  (setq org-publish-project-alist
        ;; Sample ePUB3 publication project
        '(("epub3-sample"
           :base-directory "~/Dropbox/lessons/org-mode/publish/"
           :base-extension "org"
           :publishing-function org-html-publish-to-html
           :publishing-directory "~/Dropbox/Public/org-publish/"
           :auto-sitemap t
           :sitemap-title "タイトルのテスト"
           :sitemap-subtitle "サブタイトルのテスト"
           :sitemap-filename "sitemap.org"
           :with-author "山田 太郎 "
           :with-author-reading "やまだ たろう"
           :section-numbers nil
           :with-toc nil
           ;;:preparation-function my-org-publish-prep-function
           :completion-function my-org-publish-epub3-function)))

  (defun my-org-publish-epub3-function ()
    (let* ((plist (symbol-value 'project-plist))
           (publishing-directory (plist-get plist :publishing-directory))
           (default-directory publishing-directory))
      (with-temp-file (expand-file-name
                       "generate.rb" publishing-directory)
        (insert (my-org-publish-gepub-script plist)))
      (message "now publishing epubs in %s..." default-directory)))

  (defun my-org-publish-gepub-script (plist)
    (cl-flet ((\, (key) (plist-get plist key)))
      (let* ((sitemap-html (concat (file-name-sans-extension ,:sitemap-filename) ".html"))
             (content-files
              (nreverse
              (with-temp-buffer
                (insert-file-contents sitemap-html)
                (loop while (re-search-forward "<li><a href=\"\\(.+\\)\">" nil t)
                      collect (match-string 1))))))
        (concat
         "require 'gepub'\n"
         "builder = GEPUB::Builder.new {\n"
         "  language 'ja'\n"
         "  unique_identifier 'http:/example.jp/bookid_in_url', 'BookID', 'URL'\n"
         "  title '" ,:sitemap-title "'\n"
         (when ,:sitemap-subtitle
           (concat "  subtitle '" ,:sitemap-subtitle "'\n"))
         "  creator '" ,:with-author "'\n"
         "  contributors ''\n"
         "  date '" (format-time-string "%Y-%m-%dT%T") "Z'\n"
         "  #cover_image 'img/cover.jpg'\n"
         "  resources(:workdir => '.') {\n"
         "    nav '" sitemap-html "'\n"
         "    ordered {\n"
         (mapconcat
          (lambda (file)
            (concat "      file '" file "'\n"))
          content-files "")
         "    }\n"
         "  }\n"
         "}\n"
         "epubname = File.join(File.dirname(__FILE__), 'sample.epub')\n"
         "builder.generate_epub(epubname)\n")))))

;;;;; org/ox-taskjuggler
(lazyload () "ox-taskjuggler"
  (setq org-taskjuggler-reports-directory
        (expand-file-name "~/org/tj-reports")))
;;;;; org/ox-texinfo
(lazyload (org-texinfo-export-to-texinfo) "ox-texinfo")

;;;;; org/contrib/org-bibtex-extras
(lazyload () "org-bibtex-extras"
  (setq obe-bibtex-file nil)
  (add-hook 'org-export-before-parsing-hook
            (lambda ()
              (when (equal org-export-current-backend 'html)
                (obe-html-export-citations)))))

;;;;; org/contrib/org-contacts
(lazyload () "org-contacts"
  (add-to-list
   'org-capture-templates
   `("c" "Contacts" entry (file "~/org/contacts.org")
     ,(concat "* %(org-contacts-template-name)\n"
              ":PROPERTIES:\n"
              ":EMAIL: %(org-contacts-template-email)\n"
              ":END:"))))
;;;;; org/contrib/ox-deck
;; file:/Users/kawabata/Dropbox/Public/
;; → https://dl.dropboxusercontent.com/u/463784/
(lazyload (org-deck-export-as-html org-deck-export-to-html) "ox-deck"
  (setq org-deck-directories (list (expand-file-name "~/Dropbox/Public/deck.js"))
         org-deck-base-url (concat "file:" (car org-deck-directories))))
        ;;org-deck-base-url "http://imakewebthings.com/deck.js"))

;;;; org-octopress (elpa)
;; org-mac-link の built-in-package への追加（暫定処置）
;; org-octopress -> orglue -> org-mac-link への対応 (org-mac-link-grabbar が消滅)
(when (locate-library "org-mac-link")
  (add-to-list 'package--builtins
               '(org-mac-link . [(1 2) nil "Grab links and url from various mac"])))
;; TODO 画像等の static file への対応
(lazyload (org-octopress) "org-octopress"
  (setq org-octopress-directory-top       "~/Dropbox/cvs/octopress/source"
        org-octopress-directory-posts     "~/Dropbox/cvs/octopress/source/_posts"
        org-octopress-directory-org-top   "~/org/octopress"
        org-octopress-directory-org-posts "~/org/octopress/blog"
        org-octopress-setup-file          "~/org/octopress.org"))

;;;; org-table-comment (elpa)
;; orgtbl-mode では対応できない 非ブロックコメント形式のプログラム言語
;; でのテーブル入力を実現。
;; コメントヘッダを入力後、M-x orgtbl-comment-mode
(global-set-key (kbd "C-c ;") 'orgtbl-comment-mode)

;;;; osx-osascript
(when (locate-library "osx-osascript")
  (autoload 'osascript-run-str "osx-osascript" "Run the osascript STR."))

;;;; osx-plist (elpa)
;; MacOS 10.6 から environment.plist が未サポートになり利用中止。

;;;; package-build (elpa)
;; 自動的にパッケージを構築してくれる。
;; http://qiita.com/ongaeshi/items/0502030e0875b6902fe1
;; package-build-archive
;; (package-build)
;;
;; XXX.el (Version: VERSION) --> XXX-pkg.el (define-pkg ... "VERSION")
;;                           --> XXX-VERSION.tar
;; パッケージの構築方法
;;

;;;; pandoc-mode (elpa)
;; M-x pandoc-mode
(lazyload () "pandoc-mode"
  (setq pandoc-binary (executable-find "pandoc")))

;;;; paredit-mode (elpa)
;; 非常に重くなるので必要な場面以外は使ってはいけない。

;;;; php-mode (elpa)

;;;; powerline (elpa)
;; http://hico-horiuchi.com/wiki/doku.php?id=emacs:powerline
;; 美麗なmodelineとの触れ込みだが、仮名漢字変換が表示できないので使用しない。

;;;; powershell (elpa)
;; http://blogs.msdn.com/b/powershell/archive/2008/04/15/powershell-running-inside-of-emacs.aspx
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)

;;;; popwin (elpa)
;; *Help* などのバッファのポップアップを便利にする。
(when (require 'popwin nil t)
  (popwin-mode 1)
  (setq popwin:popup-window-width 24
        popwin:popup-window-height 15
        popwin:popup-window-position 'bottom)
  (add-to-list 'popwin:special-display-config '("*anything*" :height 20))
  (add-to-list 'popwin:special-display-config '(dired-mode :position top))
  (add-to-list 'popwin:special-display-config '("*BBDB*" :height 10)))
;(eval-after-load "popwin"
;  (setq display-buffer-function 'popwin:display-buffer))

;;;; pretty-mode (elpa)
;; nil や lambda 等を λ や ∅ に置き換える。
(when (require 'pretty-mode nil t)
  (global-pretty-mode 1))

;;;; processing-mode (elpa)
;; auto-mode-alist への追加等は autoload で実行。

;;;; python-mode (elpa)

;;;; rect-mark (elpa)
;; 矩形選択を便利にする
(when (locate-library "rect-mark-autoloads")
  (define-key ctl-x-map "r\C-@" 'rm-set-mark)
  (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
  (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
  (define-key ctl-x-map "r\C-w" 'rm-kill-region)
  (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save))

;;;; regexp-lock
;; 正規表現の \\(....\\) に対応番号を付与する elisp
(lazyload () "lisp-mode"
  (when (require 'regexp-lock nil t)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-regexp-lock-mode)))

;;;; riece
;; ;; irc.w3.org 6665
;; チャンネルに入る ・C-c j
;; 次のチャンネルへ移動 ・C-c >
;; 前のチャンネルへ移動 ・C-c <
;;(lazyload (riece) "riece"
;;  (setq riece-server-alist
;;        '(;("freenode" :host "irc.freenode.net"
;;          ; :nickname "kawabata" :username "kawabata")
;;          ("w3c" :host "irc.w3.org" :port 6665
;;           :nickname "kawabata" :username "kawabata")
;;          ;("wide" : host "irc.tokyo.wide.ad.jp" :port 6668
;;          ; :nickname "kawabata" :username "kawabata")
;;          )))

;;;; rnc-mode (elpa)
;; http://www.pantor.com
(lazyload ((add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode)))
    "rnc-mode"
  (setq rnc-indent-level 2) ;; trangの出力にあわせる。
  (let ((jing (executable-find "jing.jar")))
    (when jing
      (setq rnc-enable-flymake t
            rnc-jing-jar-file jing))))

;;;; rainbow-delimiters (elpa)
;; ネストしたカッコを色違いで表示する。
(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode))


;;;; rainbow-mode (elpa)
;; CSSなどの色指定をその色で表示する。
(lazyload () "css-mode"
  (when (functionp 'rainbow-mode)
    (add-hook 'css-mode-hook
              (command (rainbow-mode +1)))))

;;;; rbenv (elpa)
;; 現在の rbenv の状態をモードラインに表示する。
;; M-x global-rbenv-mode/ rbenv-use-global/ rbenv-use

;;;; rcode-tools
;;(when (locate-library "rcodetools")
;;  (eval-after-load 'ruby-mode
;;    '(progn (require 'rcodetools)
;;            (require 'anything-rcodetools))))

;;;; rd-mode / rd-mode-plus
;;(if (locate-library "rd-mode-plus")
;;    (autoload 'rd-mode "rd-mode-plus")
;;  (if (locate-library "rd-mode")
;;      (autoload 'rd-mode "rd-mode")))
;;(when (or (locate-library "rd-mode") (locate-library "rd-mode-plus"))
;;  (add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode))
;;  ;; howm を RDで管理するのはやめる。
;;  ;;(add-to-list 'auto-mode-alist '("\\.howm$" . rd-mode))
;;  )
;;(eval-after-load 'rd-mode
;;  '(progn
;;     (defvar rd-mode-hook nil)          ; =endの“...”への置換防止
;;     (add-hook 'rd-mode-hook 'rd-show-other-block-all)))

;;;; rebox2 (elpa)
;; http://www.youtube.com/watch?v=53YeTdVtDkU
;; http://www.emacswiki.org/emacs/rebox2
(lazyload ((global-set-key (kbd "S-C-q") 'rebox-dwim)) "rebox2"
  (setq rebox-style-loop '(16 21 24 25 27)))

;;;; recursive-narrow (elpa)

;;;; rsense (elpa)
;; Rubyのための開発援助ツール.
;;(defvar rsense-home (expand-file-name "~/src/rsense-0.3"))
;;(when (file-directory-p rsense-home)
;;  (load-library "rsense")
;;  ;; 随時、http://www.ruby-lang.org/ja/man/archive/ からダウンロードして最新版をインストール。
;;  ;; e.g. ~/Resources/ruby/ruby-refm-1.9.2-dynamic-20100929/
;;  (setq rsense-rurema-home "~/Resources/ruby")
;;  (setq rsense-rurema-refe "refe-1_9_2") ;; 上記ディレクトリ下にあること！
;;  (add-hook 'ruby-mode-hook
;;            (lambda ()
;;              (local-set-key (kbd "C-c .") 'ac-complete-rsense))))

;;;; ruby-mode (elpa)
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;;;; rubydb3x
;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/rubydb3x.el
(lazyload (rubydb) "rubydb3x")

;;;; rust-mode (elpa)
;; autoload設定により、拡張子が .rs, .rc のファイルを開くと自動的に
;; rust-mode になる。
;; http://gifnksm.hatenablog.jp/entry/2013/07/15/170736 (Rust 基礎文法最速マスター)
(lazyload () "rust-mode"
  ;; flymakeの設定
  (require 'flymake)
  (defun flymake-rust-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "rustc" (list "--no-trans" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.r[cs]$" flymake-rust-init
                 flymake-simple-cleanup flymake-get-real-file-name)))

;;;; scala-mode2 (elpa)
;; autoload設定により、拡張子が .scala, .sbt のファイルを開くと自動的に
;; scala-mode になる。

;;;; sense-region
;;(when (locate-library "sense-region")
;;  (autoload 'sense-region-toggle "sense-region"))

;;;; setup-cygwin
;; Windows用のシンボリックリンクの設定など
(when (equal system-type 'windows-nt)
  (require 'setup-cygwin nil t))

;;;; session (elpa)
;; http://emacs-session.sourceforge.net/
;; 終了時にヒストリ関連の変数を保存
;; 標準の"desktop"（バッファリストなどを保存）と併用。
(add-hook 'after-init-hook 'session-initialize)
(lazyload () "session"
  (setq history-length t)
  (setq session-globals-max-string 10240
        session-registers-max-string 10240
        session-use-package t
        session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000))
        session-globals-exclude '(file-name-history load-history register-alist
                                  vc-comment-ring flyspell-auto-correct-ring))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  ;;(setq desktop-globals-to-save '(desktop-missing-file-warning))
  (setq session-undo-check -1))
(and
 (file-writable-p "~/.session")
 (locate-library "session")
 (require 'session nil t))

;;;; shell-toggle
(when (locate-library "shell-toggle")
  (autoload 'shell-toggle "shell-toggle"
    "Toggles between the *shell* buffer and whatever buffer you are editing."
    t)
  (autoload 'shell-toggle-cd "shell-toggle"
    "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
  (global-set-key (kbd "M-c") 'shell-toggle-cd))

;;;; skewer-mode
;; JavaScript統合開発環境。
;; Webページからのジャックインには、skewer-start 後、以下のBookmarkletを用いる。
;; javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
;; https://github.com/magnars/.emacs.d/blob/master/setup-skewer.el
(lazyload (skewer-demo) "skewer-mode"
  (defvar css-mode-hook nil) ;; for compatibility with skewer-css, which assumes nonstandard css-mode.
  (require 'skewer-repl)
  (require 'skewer-html)
  (require 'skewer-css)

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

  (when (require 'mouse-slider-mode nil t)
    (add-to-list 'mouse-slider-mode-eval-funcs
                 '(js2-mode . skewer-eval-defun))))

;;;; SKK
;;(setq skk-user-directory (concat user-emacs-directory "/skk/"))
;;(eval-after-load 'skk
;;  '(progn
;;     ;(global-set-key "\C-x\C-j" 'skk-mode)
;;     (global-set-key "\C-xj" 'skk-auto-fill-mode)
;;     (setq skk-byte-compile-init-file t)
;;     (setq skk-show-inline t)
;;     (setq skk-large-jisyo "~/.emacs.d/skk/SKK-JISYO.L")
;;     (setq skk-tut-file "~/.emacs.d/skk/SKK.tut")
;;     ;;(setq skk-show-inline 'vertical)
;;     ))

;;;; slime (elpa)
(lazyload ((add-hook 'lisp-mode-hook 'slimev-lisp-mode-hook)) "slime"
  (slime-setup '(slime-repl)))

;;;; slime-js (使用中止)
;; skewer-mode に移行するため使用中止。
;; % cd /path/to/npm-project
;; (prepare package.json)
;; % npm install -g swank-js
;;    → /usr/local/bin/swank-js がインストールされる。（必須）
;; % swank-js
;; M-x slime-connect
;; M-x slime-repl
;;(lazyload () "slime-js"
;;  (define-key slime-js-minor-mode-map (kbd "C-x C-e") 'slime-js-eval-current)
;;  (define-key slime-js-minor-mode-map (kbd "C-c C-e") 'slime-js-eval-and-replace-current))
;;(lazyload () "css-mode"
;;  (add-hook ‘css-mode-hook
;;              (lambda () (define-key css-mode-map “\M-\C-x” ‘slime-js-refresh-css)
;;                (define-key css-mode-map “\C-c\C-r” ‘slime-js-embed-css))))
;; https://raw.github.com/magnars/.emacs.d/master/setup-slime-js.el
;;(lazyload (slime-js-jack-in-node slime-js-jack-in-browser) "setup-slime-js"
;;  (global-set-key [f5] 'slime-js-reload)
;;  (add-hook 'js2-mode-hook
;;          (lambda ()
;;            (slime-js-minor-mode 1))))

;;;; smartchr
;; https://github.com/imakado/emacs-smartchr
;;(lazyload () "ruby-mode"
;;  (when (require 'smartchr nil t)
;;    (define-key ruby-mode-map (kbd "{") (smartchr '("{" "do |`!!'| end" "{|`!!'| }" "{{")))
;;    (define-key ruby-mode-map (kbd "#") (smartchr '("#" "##" "#{`!!'}")))
;;    (define-key ruby-mode-map (kbd "%") (smartchr '("%" "%%" "%{`!!'}")))
;;    (define-key ruby-mode-map (kbd "W") (smartchr '("W" "%w[`!!']" "WW" )))))

;;;; smartrep (elpa)
;; 例：C-c C-n の繰り返しを、C-c C-n C-n ... ですませられるようにする。
(lazyload () "outline-minor-mode"
  (when (require 'smartrep nil t)
    (smartrep-define-key
        outline-minor-mode-map "C-c"
      '(("C-n" . outline-next-visible-heading)
        ("C-p" . outline-previous-visible-heading)))))

(lazyload () "org"
  (when (require 'smartrep nil t)
    (smartrep-define-key
        org-mode-map "C-c"
      '(("C-n" . outline-next-visible-heading)
        ("C-p" . outline-previous-visible-heading)))))

(when (require 'smartrep nil t)
  ;;(global-set-key (kbd "M-R") 'move-to-window-line-top-bottom)
  (global-set-key (kbd "M-r") my-rotate-keymap) ;; move-to-window-line-top-bottom を上書き
  (global-set-key (kbd "M-R") my-rotate-keymap)
  ;; フォントセットを回転させる。
  (smartrep-define-rotate-key my-rotate-keymap "l"
    (rotate-fontspecs t) (rotate-fontspecs t -1))
  (smartrep-define-rotate-key my-rotate-keymap "k"
    (rotate-fontspecs 'kana) (rotate-fontspecs 'kana -1))
  (smartrep-define-rotate-key my-rotate-keymap "h"
    (rotate-fontspecs 'han) (rotate-fontspecs 'han -1))
  (smartrep-define-rotate-key my-rotate-keymap "s"
    (rotate-font-sizes) (rotate-font-sizes -1))
  ;; フレームの回転
  (smartrep-define-rotate-key my-rotate-keymap "f"
    (previous-multiframe-window) (next-multiframe-window))
  ;; テーマの回転
  (smartrep-define-rotate-key my-rotate-keymap "t"
    (rotate-theme) (rotate-theme -1)))

;;;; smex (elpa)
;; Smart Meta-X
;;(when (require 'smex nil t)
;;  (smex-initialize)
;;  (global-set-key (kbd "M-x") 'smex)
;;  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;  ;; This is your old M-x.
;;  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;;;; sml-modeline (elpa)
;;(when (require 'sml-modeline nil t)
;;  (sml-modeline-mode t)
;;  (if (functionp 'scroll-bar-mode)
;;      (scroll-bar-mode nil))
;;  )

;;;; smooth-scrolling (elpa)
;; 勝手に重要な関数にアドバイスするのでインストール・使用禁止。
;;;; sorter (使用中止)
;; "s" キーで、ファイル名・サイズ・日付・拡張子名順に並び替え。
;; dired-listing-switches が、"al" 以外だと動作しないため使用中止。
;; 独自に移植。 (dired-rotate-sort) 参照。

;;;; sokoban (marmalade)
;; M-x sokoban game

;;;; sql-indent (elpa)
(lazyload nil "sql"
  (when (require 'sql-indent nil t)
    (setq sql-indent-offset 4)
    (setq sql-indent-maybe-tab t)))

;;;; stripe-buffer (elpa)
;; バッファを縞々模様にする。
;; M-x stripe-buffer-mode

;;;; sunrise-commander (SC)
;; Two-pane file manager for Emacs based on Dired and inspired by MC
;; http://www.emacswiki.org/emacs/Sunrise_Commander
;; M-x sunrise

;;;; tabbar (elpa) (使用中止)
;; [注意] tabbarはバッファが増えると著しく重くなるので使用中止。
;; gnupack の設定を利用。
;(when (require 'tabbar nil t)
;  ;; tabbar有効化
;  (tabbar-mode -1)
;  ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
;  (tabbar-mwheel-mode -1)
;  ;; タブグループを使用（t：有効，nil：無効）
;  (setq tabbar-buffer-groups-function nil)
;  ;; ボタン非表示
;  (dolist (btn '(tabbar-buffer-home-button
;                 tabbar-scroll-left-button
;                 tabbar-scroll-right-button))
;    (set btn (cons (cons "" nil) (cons "" nil))))
;  ;; タブ表示 一時バッファ一覧
;  (defvar tabbar-displayed-buffers
;    '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*"
;      "*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
;    "*Regexps matches buffer names always included tabs.")
;  ;; 作業バッファの一部を非表示
;  (setq tabbar-buffer-list-function
;        (lambda ()
;          (let* ((hides (list ?\  ?\*))
;                 (re (regexp-opt tabbar-displayed-buffers))
;                 (cur-buf (current-buffer))
;                 (tabs (delq
;                        nil
;                        (mapcar
;                         (lambda (buf)
;                           (let ((name (buffer-name buf)))
;                             (when (or (string-match re name)
;                                       (not (memq (aref name 0) hides)))
;                               buf)))
;                         (buffer-list)))))
;            (if (memq cur-buf tabs)
;                tabs
;              (cons cur-buf tabs)))))
;  ;; キーバインド設定
;  ;(global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
;  ;(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
;  ;; タブ表示欄の見た目（フェイス）
;  (set-face-attribute 'tabbar-default nil
;                      :background "SystemMenuBar")
;  ;; 選択タブの見た目（フェイス）
;  (set-face-attribute 'tabbar-selected nil
;                      :foreground "red3"
;                      :background "SystemMenuBar"
;                      :box (list
;                            :line-width 1
;                            :color "gray80"
;                            :style 'released-button)
;                      :overline "#F3F2EF"
;                      :weight 'bold
;                      :family "Inconsolata"
;                      )
;  ;; 非選択タブの見た目（フェイス）
;  (set-face-attribute 'tabbar-unselected nil
;                      :foreground "black"
;                      :background "SystemMenuBar"
;                      :box (list
;                            :line-width 1
;                            :color "gray80"
;                            :style 'released-button)
;                      :overline "#F3F2EF"
;                      :family "Inconsolata"
;                      )
;  ;; タブ間隔の調整
;  (set-face-attribute 'tabbar-separator nil
;                      :height 0.1)
;  (defun rotate-tabbar (arg)
;    (interactive "P")
;    (if (null arg) (tabbar-forward-tab) (tabbar-backward-tab)))
;  (add-to-list 'rotate-command-set
;               '(rotate-tabbar . "Tabbar") t))

;;;; taskjuggler-mode
;; http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode (official)
;; org-taskjuggler is better replacement.
;; % gem install taskjuggler
;; ~/.rbenv/versions/2.0.0-p195/lib/ruby/gems/2.0.0/gems/taskjuggler-3.5.0/bin/tj3
(lazyload (taskjuggler-mode
           (add-to-list 'auto-mode-alist '("\\.tjp\\'" . taskjuggler-mode))
           (add-to-list 'auto-mode-alist '("\\.tji\\'" . taskjuggler-mode))
           (add-to-list 'auto-mode-alist '("\\.tjsp\\'" . taskjuggler-mode)))
    "taskjuggler-mode")

;;;; tern (javascript)
;; javascript analyzer
;; % git clone https://github.com/marijnh/tern
;; % cd tern
;; % sudo npm install
(let ((tern-dir "~/Dropbox/cvs/tern"))
  (when (file-directory-p tern-dir)
    (add-to-list 'load-path (concat tern-dir "/emacs"))
    (add-to-list 'exec-path (concat tern-dir "/bin"))))
(lazyload (tern-mode
           (add-hook 'js2-mode-hook (command (tern-mode t))))
    "tern")

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
;(lazyload (text-adjust-buffer text-adjust-region text-adjust) "text-adjust"
;  (setq text-adjust-rule-kutouten text-adjust-rule-kutouten-zkuten))

;;;; tss (emacs-tss)
;; https://github.com/aki2o/emacs-tss
;; typescript-tools (https://github.com/clausreinke/typescript-tools)
;; を使って、補完・ドキュメント表示・文法チェックを行なう。
(lazyload () "typescript"
  (when (and (executable-find "tss") (require 'tss nil t))
    (add-hook 'typescript-mode-hook 'tss-setup t)
    (setq tss-popup-help-key "C-:")
    (setq tss-jump-to-definition-key "C->")))

;;;; ttcn-mode
;; オリジナルファイルの (kill-all-local-variables) は削除すること。
;; https://github.com/dholm/ttcn-el/
(lazyload (ttcn-3-mode
           (add-to-list 'auto-mode-alist '("\\.ttcn3?\\'" . ttcn-3-mode)))
    "ttcn3")
(lazyload (forth-mode) "forth")
(lazyload (tm-functins) "tm")

;;;; twittering-mode (elpa)
;; doc: http://www.emacswiki.org/emacs/TwitteringMode#toc11
;; M-x twit (autoload) で開始
;; M-x twittering-icon-mode でアイコン表示
;; d … direct message
;; u … post message
;; C-c D … erase post
;; <RET> … reply
(lazyload () "twittering-mode"
  (setq twittering-username "kawabata")
  (setq twittering-timer-interval 6000)
  (add-hook 'twittering-mode-init-hook
            'twittering-icon-mode)
  (add-hook 'twittering-mode-init-hook
            'twittering-jojo-mode))
;; (setq twittering-password "Twitterのパスワード")

;;;; typescript
;; http://www.typescriptlang.org/
;; typescript.el は、公式Webの "tools" からダウンロード
;; reference は https://developers.google.com/chrome-developer-tools/ を参考に。
;; （旧）TypeScript.el は、typescript.el とリネームする。
;; （新）typescript.el は、emacs-tss に含まれているものを使用する。
(lazyload (typescript-mode
           (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode)))
           "typescript")

;;;; undo-tree (elpa)
;; C-/ : undo
;; C-? : redo
;; C-x u : show tree (p/n/f/b)
(lazyload () "undo-tree"
  (global-undo-tree-mode))
(require 'undo-tree nil t)

;;;; unicode-fonts (elpa)
;; M-x unicode-fonts-setup
;;(lazyload () "unicode-fonts"
;;  (defun my-push-font (range &rest fonts)
;;    (let ((default-fonts (car (alist-get range unicode-fonts-block-font-mapping))))
;;      (dolist (font fonts)
;;        (nconc default-fonts (list font)))
;;      (message "default-fonts=%s" default-fonts)
;;      (setf (alist-get range unicode-fonts-block-font-mapping)
;;            (list default-fonts))))
;;  (my-push-font "CJK Unified Ideographs" "Hiragino Mincho Pro"))

;;;; visual-basic-mode
(lazyload (visual-basic-mode
           (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" .
                                           visual-basic-mode)))
    "visual-basic-mode")

;;;; w32-print
;; http://www.bookshelf.jp/soft/meadow_15.html#SEC14
;; M-x w32-winprint-print-{buffer,region}-notepad … 早いけど汚い
;; M-x w32-winprint-print-{buffer,region}-htmlize … 遅いけど綺麗
(when (eq window-system 'w32)
  (require 'htmlize nil t)
  (require 'w32-winprint nil t))

;;;; w32-symlinks
(when (and (eq system-type 'windows-nt)
           (require 'w32-symlinks nil t))
  (defvar w32-symlinks-handle-shortcuts)
  (setq w32-symlinks-handle-shortcuts t))

;;;; w3m-mode (elpa)
(lazyload () "w3m"
  (when (coding-system-p 'cp51932)
    (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

;;;; web-mode (elpa)
;; http://web-mode.org/
;; http://fukuyama.co/web-mode
(lazyload
    (web-mode
     (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
     (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
     (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
     (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
     (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode)))
    "web-mode"
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))

;;;; wget (elpa)

;;;; wgrep (elpa)
;; You can edit the text in the *grep* buffer after typing C-c C-p.
;; After that the changed text is highlighted.
;; The following keybindings are defined:
;;
;; C-c C-e : Apply the changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-d : Mark as delete to current line (including newline).
;; C-c C-r : Remove the changes in the region (these changes are not
;;           applied to the files. Of course, the remaining
;;           changes can still be applied to the files.)
;; C-c C-p : Toggle read-only area.
;; C-c C-k : Discard all changes and exit.
;; C-x C-q : Exit wgrep mode.
(lazyload () "grep"
  (require 'wgrep nil t))

;;;; winhist
(lazyload (winhist-forward winhist-backward
           rotate-winhist
           (defun rotate-winhist (arg)
             (interactive "P")
             (let ((command (if (null arg) 'winhist-forward 'winhist-backward)))
               (call-interactively command)
               command)))
    "winhist"
  (winhist-mode 1))

(lazyload ((smartrep-define-rotate-key my-rotate-keymap "w"
             (rotate-winhist) (rotate-winhist -1)))
    "smartrep")

;;;; yaml-mode (elpa)

;;;; yang-mode
;; http://www.yang-central.org/twiki/pub/Main/YangTools/yang-mode.el
(add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))
(lazyload (yang-mode) "yang-mode")

;;;; yasnippet (elpa)
;; ・ELPAにはyasnippet（新）とyasnippet-bundle（旧）があるが、 新版を使う。
;; ・snippets は、~/.emacs.d/snippets ディレクトリに格納する。
;; ・snippets を使うときは、M-x yas-minor-mode
;; ・snippets を編集したら、 M-x yas-reload-all でリロード。
;; ・snippets の呼び出しは、 M-x yas-insert-snippet (C-c & C-s)
;; ・snippets の展開は、M-x yas-expand (<tab>)
;; 日本語のsnippetは、キーワード展開しにくいので、bindingのショートカットキーで
;; 呼び出す。
(lazyload () "yasnippet"
  ;; 青空文庫用スニペット
  (when (file-directory-p "~/.emacs.d/snippets/aozora-yasnippets")
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/aozora-yasnippets")
    (defun ruby-clean-up ()
      (when (looking-back "\\\cC\\(.+\\)《.+\\(\\1\\)》")
        (insert (match-string 1))
        (delete-region (match-beginning 2) (match-end 2))
        (delete-region (match-beginning 1) (match-end 1))))
    (add-hook 'yas-after-exit-snippet-hook 'ruby-clean-up))
  ;;(yas-global-mode 1) ; 重いので個々に使う場合のみ。
)

;;;; zencoding-mode (elpa)
;; emmet-mode へ移行。
;; http://www.emacswiki.org/emacs/ZenCoding
;; http://fukuyama.co/zencoding
;; M-x zencoding-mode で、 "ul#name>li.item*2" C-j で入力。
;;(lazyload ((global-set-key (kbd "C-x Z") 'zencoding-mode)) "zencoding-mode"
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
;;  (define-key zencoding-mode-keymap (kbd "C-,") 'zencoding-expand-yas))

;;;; zotero
;; C-c z prefix.

;;; 個人用アプリケーション
;;;; aozora-proc
(lazyload (aozora-proc aozora-proc-region aozora-proc-buffer) "aozora-proc")

;;;; aozora-view
(lazyload (aozora-view) "aozora-view")

;;;; Birthdays 誕生日
(lazyload () "calendar"
  (load "my-birthdays" nil t))

;;;; bbdb-export
(lazyload (bbdb-export-vcard
           (global-set-key (kbd "C-c V") 'bbdb-export-vcard)) "bbdb-export")

;;;; BibTeX CiNii検索
(lazyload (bib-cinii-bib-buffer) "bib-cinii")

;;;; BibTeX 国会図書館検索
(lazyload (bib-ndl-bib-buffer) "bib-ndl")

;;;; IDS編集モード
;;(makunbound 'ids-edit-mode)
(global-set-key (kbd "C-c 0") (command (insert "⿰")))
(global-set-key (kbd "C-c 1") (command (insert "⿱")))
(global-set-key (kbd "C-c 2") (command (insert "⿲")))
(global-set-key (kbd "C-c 3") (command (insert "⿳")))
(global-set-key (kbd "C-c 4") (command (insert "⿴")))
(global-set-key (kbd "C-c 5") (command (insert "⿵")))
(global-set-key (kbd "C-c 6") (command (insert "⿶")))
(global-set-key (kbd "C-c 7") (command (insert "⿷")))
(global-set-key (kbd "C-c 8") (command (insert "⿸")))
(global-set-key (kbd "C-c 9") (command (insert "⿹")))
(global-set-key (kbd "C-c -") (command (insert "⿺")))
(global-set-key (kbd "C-c =") (command (insert "⿻")))
(global-set-key (kbd "C-c Y") (command (insert "从")))
(lazyload (ids-edit-char ids-edit-mode
           (global-set-key (kbd "C-c i") 'ids-edit-mode)
           (global-set-key (kbd "M-U") 'ids-edit-char)) "ids-edit")

;;;; ivs-utils
(lazyload (ivs-edit
           (global-set-key (kbd "M-J") 'ivs-edit)) "ivs-utils")

;;;; math-symbols
(lazyload () "helm"
  (when (functionp 'math-symbols-helm)
    (define-key helm-command-map "M" 'math-symbols-helm)))

;;;; 異体字入力
(lazyload ((global-set-key (kbd "C-c v") 'variants-tree)) "variants-tree")
(lazyload (variants-insert
           (global-set-key (kbd "M-I") 'variants-insert)) "variants")

;;;; PDF辞書検索
(lazyload (view-ucs-pdf-at-point dict-view-pdf-at-point
           (global-set-key (kbd "M-S-d") 'dict-view-pdf-at-point)) "view-pdf-dict")

;;;; view-pdf
;; `browse-url-browser-function' 変数を拡張して、ローカルファイル上の
;; PDFを見れるようにする。lookup等で有用。
(lazyload () "browse-url"
  (require 'view-pdf nil t))



;;; 個人用関数
;;;; データ操作
(defun map-plist (plist func)
  (let (result)
    (while plist
      (push (funcall func (car plist) (cadr plist)) result)
      (setq plist (cddr plist)))
    result))

(defun plist-remove (plist prop)
  (if (equal (car plist) prop) (cddr plist)
    (let ((tail (cdr plist)))
      (while (cdr tail)
        (if (equal (cadr tail) prop) (setcdr tail (cdddr tail)))
        (setq tail (cddr tail)))
      plist)))

(defun assoc-all (key alist &optional test)
  (loop for cons in alist
        if (funcall (or test 'equal) (car cons) key)
        collect (cdr cons)))

(defun rassoc-all (key alist &optional test)
  (loop for cons in alist
        if (funcall (or test 'equal) (cdr cons) key)
        collect (car cons)))

(defun assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (equal (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

;;;; ハッシュテーブル関係
(defvar add-value) ;; special variable （構文スコープ対策）
(defun addhash (key value table &optional append)
  "Add VALUE to a list associated with KEY in table TABLE."
  (let ((add-value (gethash key table)))
    (add-to-list 'add-value value append)
    (puthash key add-value table)))

(defun add-char-code-property (char propname value &optional append)
  "Add VALUE to list contained in character CHAR's property  PROPNAME."
  (let ((add-value (get-char-code-property char propname)))
    (add-to-list 'add-value value append)
    (put-char-code-property char propname add-value)))

(defun clear-char-code-property (prop)
  (map-char-table ; char-code-property-table にある prop のみ対応。
   (lambda (char plist)
     (when (plist-member plist prop)
       (aset char-code-property-table char (plist-remove plist prop))))
   char-code-property-table))

;;;; リスト操作
(defun combinatorial (head &rest tail)
  "Make a list of a combinatorial of SEQuenceS.
That means to create the all possible combinations of sequences.
For example, if the first sequence contains 3 elements, and the
second one contains 5 elements, then 15 lists of length 2 will be
returned."
  (if tail
      (cl-mapcan (lambda (y) (mapcar (lambda (x) (cons x y)) head))
                 (apply 'combinatorial tail))
    (mapcar 'list head)))

(defun flatten (list)
  "Flatten nested LIST."
  (if (listp list)
      (apply 'nconc (mapcar 'flatten list))
    (list list)))

(defun list-rotate-forward (ring)
  "リストを前方向に回転させる。破壊的操作。"
  (let ((item (car ring))
        (last (last ring)))
    (setcdr last (list item))
    (cdr ring)))

(defun list-rotate-backward (ring)
  "リストを後方向に回転させる。破壊的操作。"
  (let ((item (car (last ring)))
        (last2 (last ring 2)))
    (setcdr last2 nil)
    (cons item ring)))

(defun list-rotate-to (ring target)
  "リストを目標場所まで回転させる。破壊的操作。"
  (if (equal (car ring) target) ring
    (let ((tail (member target ring))
          (head ring))
      (if (null tail)
          (error "target %s is not a member of ring %s" ring target)
        (while (not (equal (cadr ring) target))
          (setq ring (cdr ring)))
        (setcdr ring nil)
        (nconc tail head)))))

;;;; テキストプロパティ操作
(defun remove-overlays-region (from to)
  "指定した領域のoverlayを全て除去する。"
  (interactive "r")
  (remove-overlays from to))

(defun put-text-property-region (from to property value)
  "指定した領域に指定した名前のテキストプロパティを設定する。"
  (interactive "*r\nSproperty name: \nSproperty value: ")
  (put-text-property from to property value))

(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;;;; DropBox
(defun find-file-in-dropbox (&optional filename)
  "FILENAME を DropBox で開く。"
  ;; ffap 機能の活用例。
  (interactive)
  (or filename
      (setq filename
            (ffap-read-file-or-url
             "Find file in DropBox: " (ffap-guesser))))
  (setq filename (expand-file-name (file-truename filename)))
  (if (string-match "Dropbox/\\(.*\\)$" filename)
      (if (file-directory-p filename)
          (browse-url (concat "https://www.dropbox.com/home/"
                              (match-string 1 filename)))
        (browse-url (concat "https://www.dropbox.com/revisions/"
                            (match-string 1 filename))))
    (message "Not Dropbox Directory! %s -- " filename)))
(global-set-key (kbd "C-x M-f") 'find-file-in-dropbox)

(defun browse-dropbox-public-folder (&optional filename)
  "FILENAME を Dropbox Public フォルダとしてブラウザで開く。"
  (interactive)
  (or filename
      (setq filename
            (ffap-read-file-or-url
             "Find file in DropBox: " (ffap-guesser))))
  (setq filename (expand-file-name (file-truename filename)))
  (if (string-match "Dropbox/Public/\\(.*\\)$" filename)
          (browse-url (concat "https://dl.dropboxusercontent.com/u/463784/"
                              (match-string 1 filename)))
    (message "Not Dropbox Public Directory! %s -- " filename)))
(global-set-key (kbd "C-x M-F") 'browse-dropbox-public-folder)

;;;; その他の関数
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

(defun count-cjk-chars (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "." nil t)
          (let ((char (string-to-char (match-string 0))))
            (if (and (< #x1fff char) (< char #xe0000))
                (incf count))))
        (message "count=%d" count)))))

(lazyload () "org"
  (defun my-org-screenshot ()
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

(defun parse-init-global-key-settings ()
  "設定ファイルの global-set-key と lazyload の情報を読み込みキー一覧表を作成する。"
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/init.el")
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward
              (concat
               "^ *(global-set-key \\(?:(kbd \"\\(.+?\\)\")\\|\\(\".+?\"\\)\\|\\(\\[.+?\\]\\)\\) +"
               "\\(?:(lambda () (interactive) \\(.+\\))\\|'\\(.+?\\)\\))") nil t)
        (let ((sym (or (match-string 4) (match-string 5))))
          (cond ((match-string 1) (push (cons (match-string 1) sym) result))
                ((match-string 2)
                 (let ((key (match-string 2)))
                   (push (cons (format-kbd-macro (car (read-from-string key))) sym) result)))
                ((match-string 3)
                 (push (cons (match-string 3) sym) result)))))
      ;; lazyload
      (goto-char (point-min))
      ;; insert `backslash' before `l' to avoid this search program to
      ;; find itself.
      (while (re-search-forward "(\lazyload " nil t)
        (goto-char (match-beginning 0))
        ;; (message "debug=%s" (buffer-substring (point) (+ 100 (point))))
        (let* ((expr (read (current-buffer)))
               (funcs (cadr expr)))
          (dolist (func funcs)
            (and (listp func)
                 (push (cons (cadr func) (car func)) result)))))
      (dolist (item result)
        (message "| %s | %s |" (car item) (cdr item))))))

;;; Startup Files
(let ((files
       '(
         ;; シェルヒストリを開いておくと、作業中に履歴を簡単に検索でき
         ;; るので都合良い。
         ;; "~/.zsh_history"
        )))
  (dolist (file files)
    (if (file-exists-p file)
        (find-file file)
      (message "File `%s' does not exist!" file ))))
;;; Custom Settings & Local Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" #1="#6b82a7" "#a66bab" #1# "#505050"])
 '(background-color "#202020")
 '(background-mode dark)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-safe-themes (quote ("a30c29c5cb75a66378d473d03b264b0bc1ee445b9c3edd276385ffec0a685d3f" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "f38dd27d6462c0dac285aa95ae28aeb7df7e545f8930688c18960aeaf4e807ed" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "124e34f6ea0bc8a50d3769b9f251f99947d6b4d9422c6d85dc4bcc9c2e51b39c" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "5ce9c2d2ea2d789a7e8be2a095b8bc7db2e3b985f38c556439c358298827261c" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "466ae54a7b157ad02fd91da72b7871bccfb9bac98fdab95cf7a0d405c8572bd0" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "8b49009d04730bc5d904e7bb5c7ff733f3f9615c3d6b3446eca0766e6da2bea1" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "88b663861db4767f7881e5ecff9bb46d65161a20e40585c8128e8bed8747dae5" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "a3f42f216305516caab36369dfe1fba8e14358423f6faf1d403d46a3515cd842" "30f861ee9dc270afc2a9962c05e02d600c998905433c8b9211dc2b33caa97c51" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "998e84b018da1d7f887f39d71ff7222d68f08d694fe0a6978652fb5a447bdcd2" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "9562e9eb5fd01677ac6b76b66f9a81338991fa9d270270802eeae5232c8da0a6" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "817ce7168851955b2d67a9dfb2f4bb283504e3be87b17932bd8a3ee4b43cfeb1" "635518bf81b80533e3ee3a76d55f992494ea7bf3018bf58cd3d44f100d66fa8e" default)))
 '(debug-on-quit nil)
 '(default-input-method "math-symbols-bold")
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(foreground-color "#cccccc")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(linum-format " %7d ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)) (eval hide-sublevels 5) (mangle-whitespace . t) (require-final-newline . t) (eval hide-region-body (point-min) (point-max)) (outline-minor-mode . t) (coding-system . utf-8))))
 '(session-use-package t nil (session))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map (quote ((20 . "#437c7c") (40 . "#336c6c") (60 . "#205070") (80 . "#2f4070") (100 . "#1f3060") (120 . "#0f2050") (140 . "#a080a0") (160 . "#806080") (180 . "#704d70") (200 . "#603a60") (220 . "#502750") (240 . "#401440") (260 . "#6c1f1c") (280 . "#935f5c") (300 . "#834744") (320 . "#732f2c") (340 . "#6b400c") (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Local Variables:
;; outline-minor-mode: t
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; eval: (hide-sublevels 5)
;; End:
