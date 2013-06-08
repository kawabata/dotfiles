;;; .gnus.el --- setup for Gnus -*- coding: utf-8; lexical-binding: t -*-
;;
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Modified: 2013-06-09
;; URL: https://github.com/kawabata/dotfiles/
;;
;;; Gnusの構造
;;
;; mail-source.el
;; +-----------+     +----------+
;; | spool {d} +--+  | nntp     +--+     Gnus                          send-mail-function
;; +-----------+  |  +----------+  |  +---------+     message          +---------------+
;;                |                |  | Servers |   +-------------+    | sendmail      |
;; +-----------+  |  +----------+  |  | Groups  |   | gnus-msg.el |    |     or        |
;; | pop3 {d}  +--+->| nnml     +--+->| *Topics +-->| message.el  |--->| smtpmail {io} |
;; +-----------+  |  +----------+  |  | Summary |   | sendmail.el |    |     or        |
;;                |                |  | Article |   +-------------+    | qmail, etc.   |
;; +-----------+  |  +----------+  |  +---------+                      +---------------+
;; |procmail{d}+--+  | nnimap   +--+
;; +-----------+     +----------+  |
;;                                 |
;;                   +----------+  |
;;                   | nnir     +--+
;;                   +----------+  |
;;                                 |
;;                   +----------+  |
;;                   | nnrss    +--+
;;                   +----------+  |
;;                                 |
;;                   +----------+  |
;;                   |nnarchive +--+
;;                   +----------+  |
;;                                 |
;;                   +----------+  |
;;                   | nnfolder +--+
;;                   +----------+
;;
;; - sendmail.el は sendmail コマンドでメールを送るが smtpmail.el は直
;;   接 SMTPプロトコルを使用する。
;; - nnml は、１メール・１ファイルのメール・バックエンド

;; 初回起動時は "^" を押して (gnus-group-enter-server-mode) 購読ニュー
;; スを決定する。

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'gnus)
(require 'gnus-msg)
(require 'message)
(require 'nnimap)
(require 'auth-source)
(require 'mail-source)
(require 'smtpmail)

;;;; 環境変数と設定情報

;; | 環境変数     | 対応変数             | 使用ファイル   |
;; |--------------+----------------------+----------------|
;; | EMAIL        | user-mail-address    | startup.el     |
;; | MAILHOST     | mail-sources         | mail-source.el |
;; |              | (imap/pop3)          | pop3.el        |
;; | SMTPSERVER   | smtpmail-smtp-server | smtpmail.el    |
;; | NNTPSERVER   | gnus-select-method等 | gnus.el        |
;; |--------------+----------------------+----------------|
;; | LOGNAME      |                      |                |
;; | MAILCAPS     |                      |                |
;; | MAILDIR      |                      |                |
;; | MIMETYPES    |                      |                |
;; | ORGANIZATION |                      |                |
;; | SAVEDIR      |                      |                |
;; | USER         | mail-sources         |                |

;;; ユーザ情報設定
;;;; メール受信ソース
(defvar my-mail-spool
  (let ((spool "/var/mail/kawabata"))
    (when (file-exists-p spool)
          (list 'file :path spool))))

(defvar my-mail-sources
  '(("mailsv5.y.ecl.ntt.co.jp"
     :method  (nnml "")
     :sources ((pop :user "tk319")))
    ("pop.gmail.com"
     :method  (nnml "")
     :sources ((pop :user "kawabata.taichi@gmail.com"
                    :port 995 :connection ssl))
     :secondary ((nntp "news.gmane.org")))
    ("imap.gmail.com"
     :method
     (nnimap "imap.gmail.com"
             ;; nnimap のバグ? nnimap-open-server 関数では、
             ;; nnimap-address よりこちらが使われる場合がある。
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port 993)
             (nnimap-stream ssl)
             (nnir-search-engine imap)
             (nnimap-authinfo-file "~/.emacs.d/.authinfo.gpg")
             ;; @see [[info:gnus#IMAP]]
             ;; press 'E' to expire email
             (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
             (nnmail-expiry-wait 90))))
  "MAILHOST環境変数と合致するサーバの種類・ポート等の設定。")

;;;; メール送信サーバ
(defvar my-mail-servers
  '(("mailsv5.y.ecl.ntt.co.jp"
     :local-domain "ntt.co.jp")
    ("smtp.gmail.com"
     :smtp-service 465
     :stream-type ssl)
    ("vcclock.ocn.ne.jp")))

;;;; メールアカウント
(defvar my-mail-accounts
  '(("kawabata.taichi@lab.ntt.co.jp" :signature ".signature.ntt"
     :organization "NTT Network Innovation Laboratories"
     :extra-headers
     (("X-GPG-Fingerprint"
       "E31F FE94 5036 1E3D B76D 37CB 3E38 B803 DC18 167E\n")
      ("X-GPG-Key"
       "http://pgp.nic.ad.jp:11371/pks/lookup?op=index&search=kawabata.taichi@lab.ntt.co.jp")))
    ("kawabata.taichi@gmail.com" :signature ".signature.gmail"
     (("X-GPG-Fingerprint"
       "ECB2 051F 6D9B F347 DBEA 2792 2065 8961 AA50 61C2")
      ("X-Github" "http://github.com/kawabata")))
    ;; 以下は使用廃止
    ;;("kawabata@clock.ocn.ne.jp" :signature ".signature.ocn")
    ;;("kawabata@meadowy.org" :signature ".signature.meadowy"
    ;; :extra-headers
    ;; (("X-GPG-Fingerprint"
    ;;   "7EA6 81CE B9B0 CC0C BE30  FD8A 151A 7DC1 9BE4 3A79\n")
    ;;  ("X-GPG-Key"
    ;;   "http://pgp.nic.ad.jp:11371/pks/lookup?op=index&search=kawabata@meadowy.org")))
    ))

(defvar user-latin-name nil)

;; gnus-user-aget の末尾にmule-versionを繋げる
(when (not (stringp gnus-user-agent))
  (setq gnus-user-agent
        (concat
         (let ((gnus-user-agent '(emacs gnus type)))
           (gnus-extended-version))
         " Mule/" mule-version)))

;;;; 基本パラメータ設定
(defun my-gnus-setup ()
  "This assumes that `smtpmail-smtp-server', `user-mail-address' and `MAILHOST'
enviornment variable are already set."
  (message "mail setup: email=%s source=%s smtp-server=%s"
           user-mail-address (getenv "MAILHOST") smtpmail-smtp-server)
  (let* ((source        (getenv "MAILHOST"))
         (source-plist  (assoc-default source my-mail-sources))
         (server-plist  (assoc-default smtpmail-smtp-server my-mail-servers))
         (account-plist (assoc-default user-mail-address my-mail-accounts)))
    (or source source-plist server-plist account-plist
        (error "Improper mail settings! %s" source))
    (setq
     ;; gnus/gnus-msg.el
     gnus-posting-styles
     `((".*"
        (From ,(concat user-full-name " <" user-mail-address ">"
                       (if user-latin-name (concat " ( " user-latin-name " )"))))
        (organization ,(plist-get account-plist :organization))
        (signature :file ,(expand-file-name
                           (plist-get account-plist :signature)
                           "~/Dropbox/signatures/"))
        ;;(bcc nil)
        ,(when (executable-find "uname")
           (list "X-Operating-System" (shell-command-to-string "uname -mv")))
        ("User-Agent" ,(gnus-extended-version))
        ("Accept-Language" "ja, en;q=0.6, zh;q=0.3, fr;q=0.1, la;q=0.01\n")
        ,@(plist-get account-plist :extra-headers)
        ;; (x-url (getenv "WWW_HOME"))
        ;; (x-face-file "~/.emacs.d/xface")
        ))
     ;; gnus/gnus.el
     gnus-select-method (plist-get source-plist :method)
     ;; NNTPSERVER環境変数のニュースを付加する。
     gnus-secondary-select-methods
     (append (plist-get source-plist :secondary)
             (let ((nntpserver (gnus-getenv-nntpserver)))
               (and nntpserver `((nntp ,nntpserver)))))
     ;; gnus/auth-source.el
     ;; 詳細は [[info:auth]] 参照。
     auth-sources '("~/.emacs.d/.authinfo.gpg")
     ;; gnus/mail-source.el
     mail-sources (plist-get source-plist :sources)
     ;; mail/smtpmail.el
     smtpmail-mail-address user-mail-address
     smtpmail-default-smtp-server smtpmail-smtp-server
     smtpmail-smtp-service (or (plist-get server-plist :smtp-service)
                               smtpmail-smtp-service)
     smtpmail-local-domain (plist-get server-plist :local-domain)
     smtpmail-stream-type  (plist-get server-plist :stream-type))))

(my-gnus-setup)

;;; mail 設定
;;;; add-log.el
(lazyload () "add-log"
  (setq add-log-mailing-address user-mail-address))
;;;; mail/sendmail.el
(setq send-mail-function 'smtpmail-send-it
      mail-interactive t ; メール送信時に完了まで待機
      mail-self-blind t ; 自分自身にブラインドメールを送る
      mail-specify-envelope-from t ; envelope-from は、 From: ヘッダにする。
      mail-envelope-from 'header)

;;;; mail/smtpmail.el
;;(setq smtpmail-debug-info t
;;      smtpmail-debug-verb t)


;;; ldap 設定
;; http://www.nic.ecl.ntt.co.jp/info/ldap/index.html
;; http://www.atmarkit.co.jp/ait/articles/0011/15/news003.html
(when (string-match "ntt.co.jp" user-mail-address)
  (eval-and-compile (require 'ldap))
  (setq ldap-default-host "ldap.rdh.ecl.ntt.co.jp")
  (setq ldap-default-base "dc=ntt,dc=co,dc=jp")
  ;; ldapsearch 命令の引数
  ;; 利用LDAPサーバはSASL接続しないので、-xが必要。
  ;; また、"-tt" によりテンポラリファイルに出力される。
  (setq ldap-ldapsearch-args '("-tt" "-LL" "-x"))
  ;; % ldapsearch -x -h ldap.rdh.ecl.ntt.co.jp -b "dc=ntt,dc=co,dc=jp" "cn=山田*"
  ;; M-x trace-function ldap-search-internal
  (eval-and-compile (require 'eudc))
  (eudc-set-server ldap-default-host 'ldap t)
  (setq eudc-default-return-attributes nil
        eudc-strict-return-matches nil
        ;; BBDBv3はEUDC未対応。
        eudc-server-hotlist `(;;("localhost" . bbdb)
                              (,ldap-default-host . ldap))
        eudc-inline-expansion-servers 'hotlist)
  (eudc-protocol-set 'eudc-inline-expansion-format '("%s <%s>" displayName email)
                     'ldap)
  (eudc-protocol-set 'eudc-inline-query-format '((cn)
                                                 (mail)
                                                 (cn cn)
                                                 (cn cn cn)
                                                 (sn)
                                                 (uid)
                                                 (givenName)
                                                 (givenName name)
                                                 (name))
                     'ldap)
  ;; - Message-Mode では、
  ;;     <tab>   → EUDC補完
  ;;     <M-tab> → BBDB補完
  ;;
  ;; - 当面は M-x eudc-query-form でのみ利用する。
  ;;
  ;;(eudc-protocol-set 'eudc-inline-expansion-format '("%s %s < %s>" firstname lastname net)
  ;;                   'bbdb)
  ;;(eudc-protocol-set 'eudc-inline-query-format '((name)
  ;;                                               (firstname)
  ;;                                               (lastname)
  ;;                                               (firstname lastname)
  ;;                                               (net))
  ;;                   'bbdb)
  )
 

;;; gnus 設定
;;;; gnus/gnus-start.el
(setq gnus-save-newsrc-file nil) ; .newsrcは不要（他のNewsReaderは使わず）
;; 起動時はトピックモード。
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;;; gnus/gnus.el
(setq gnus-use-long-file-name t
      ;; gnus-invalid-group-regexp nil
      gnus-large-newsgroup 200 ; 200以上は大きいニュースグループ
      gnus-use-trees nil ; スレッドをツリーで表示
      gnus-novice-user nil ; Gnus 初心者 （メール削除の毎に確認）
      ;; gnus-group-use-permanent-levels 3
      gnus-use-adaptive-scoring t
      gnus-summary-line-format
      "%U%R%z%I %&user-date; %(%[%4L: %-23,23f%]%) %s\n")
;; 期限が過ぎたメールの自動削除機能
;;(setq gnus-auto-expirable-newsgroups
;;      (concat
;;       "^nnml:mail.java\\|"
;;       "^nnml:mail.nikkei\\|"
;;       "^nnml:mail.mag2\\|"
;;       "^nnml:mail.nagazines\\|"
;;       "^nnml:mail.hatena\\|"
;;       "^nnml:mail.atmarkit"))

;;;; gnus/gnus-art.el
(require 'gnus-art)
(setq gnus-treat-display-face 'head
      gnus-default-article-saver 'gnus-summary-save-in-folder
      ;; alternativeはボタンで選択する。
      gnus-buttonized-mime-types '("multipart/signed" "multipart/alternative"))

;;;; gnus/gnus-cache.el
(require 'gnus-cache)
(setq gnus-cacheable-groups "^nnimap")

;;;; gnus/gnus-group.el
(setq gnus-group-line-format "%M\%S\%p\%5y: %(%g%)%l\n")

;;;; gnus/gnus-notifications.el
;; (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

;;;; gnus/gnus-score.el
;;(require 'gnus-score)
;;(setq gnus-home-score-file 'gnus-hierarchial-home-score-file)
;;(setq gnus-home-score-file "all.SCORE")

;;;; gnus/gnus-sum.el
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today %H:%M")
        (t . "%y/%m/%d %a"))
      ;; gnus-summary-mark-below -300
      ;; gnus-auto-center-summary nil
      ;; gnus-summary-thread-gathering-function
      ;;   'gnus-gather-threads-by-references
      ;; gnus-summary-gather-subject-limit 10
      gnus-thread-indent-level 2)

;;;; gnus/mailcap.el
(require 'mailcap)
(dolist (extension
         '((".ppt"  . "application/vnd.ms-powerpoint")
           (".pptx"  . "application/vnd.ms-powerpoint")
           (".xlsx"  . "application/vnd.ms-excel")
           (".doc"  . "application/vnd.ms-word")
           (".docx"  . "application/vnd.ms-word")))
  (add-to-list 'mailcap-mime-extensions extension))

;;;; gnus/mail-source.el
;; メールのテンポラリであるIncoming*** ファイルは、一週間保存する。
(setq mail-source-delete-incoming 7)

;;;; gnus/message.el
(setq message-send-mail-partially-limit 5000000 ; 5メガバイト
      ;; smtpエンベロープのfromをメールのfromと合わせる。
      message-sendmail-envelope-from 'header
      message-mail-alias-type nil  ; abbrevは使わない。
      message-cite-function 'mu-cite-original
      ;;message-cite-function 'message-cite-original-without-signature
      ;;message-syntax-checks
      ;;'((long-lines . disabled)
      ;;  (control-chars . disabled)
      ;;  (sender . disabled))      ; ここがポイント
      message-send-mail-function 'smtpmail-send-it)

;;;; gnus/mm-decode.el
(setq mm-default-directory "~/Downloads/")

;;;; gnus/mm-sec.el
;; 署名は C-c C-m C-s C-p で。
;; 署名を自動化する。
;; (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)

;;;; gnus/nnimap.el
;; メールを読む際、添付ファイルはローカルにダウンロードしない。
(setq nnimap-fetch-partial-articles t)

;;;; gnus/nnmail.el
(lazyload () "nnmail"
  (setq ;;nnmail-treat-duplicates 'delete
        nnmail-treat-duplicates 'warn
        nnmail-keep-last-article nil
        nnmail-expiry-wait 60
        nnmail-split-methods 'nnmail-split-fancy)
  ;; Fancy Split のポリシー
  ;;
  ;; (1) まず、ML単位で振り分ける。
  ;;     … 個個人よりも、トピック単位でまとめた方が読みやすい。
  ;; (2) 次に、個人関係グループで分離する。
  ;;     … 知人関係をグループでまとめると便利なため。
  ;; (3) 最後に、所属や、メールマガジン単位で分離する。
  ;;     … 上記のいずれにも該当しない場合は、マッチさせる正規表現が書きやすい。
  ;; nnmail-split-fancy は暗号化する。
  (load-file (expand-file-name ".gnus-split-mail.el.gpg" user-emacs-directory)))

;; Fancy Split における、重複を防ぐ工夫
;;(setq nnmail-treat-duplicates 'delete)

;; nnmail-split-fancy でエラーがでたら、(nnmail-split-fancy) で
;; ログを調べること。

;;;; gnus/nnml.el
(require 'nnml)
(setq nnml-directory (expand-file-name "~/Mail/")
      nnml-get-new-mail t)

;;;; gnus/nnrss.el
;; M-x nnrss-generate-download-script で生成したスクリプトを
;; crontab で実行する。
(lazyload () "nnrss"
  (setq nnrss-use-local t))

;;;; gnus/pop3.el
(lazyload () "pop3"
  (setq pop3-password-required nil
        pop3-stream-type 'starttls))
;;; 外部ライブラリ
;;;; bbdb.el
(require 'bbdb)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus 'bbdb-insinuate-message)
(bbdb-initialize 'gnus 'message)
 ;; バッファでは表示のみ行い、確認や自動更新は行わない。
(setq bbdb-mua-update-interactive-p '(query . search))
(bbdb-mua-auto-update-init 'gnus)

;;;; gnus-spotlight.el
;; http://www.yoshidam.net/diary/Spotlight/
(when (and (executable-find "mdfind")
           (eval-and-compile (require 'gnus-spotlight nil t)))
  (gnus-spotlight-insinuate))

;;;; gnus-est.el
;; インデックス自動生成：下記のコマンドで必要十分に動作するが、現状では
;; 余計な引数を付けるため、後日それを削除する必要がある。
(lazyload () "gnus-est"
  (when (and (setq gnus-est-make-index-command
                   (executable-find "estcmd"))
             (require 'gnus-est nil t)
             (file-exists-p "/var/kawabata/hyperestraier/cascket"))
    (setq gnus-est-index-directory (expand-file-name "~/News/cascket"))
    (gnus-est-insinuate)
    (setq gnus-est-index-update-interval nil)))

;; 遅延配送 ;; http://www.bookshelf.jp/soft/meadow_55.html#SEC781
(gnus-delay-initialize)

;;;; mu-cite.el
(require 'mu-bbdb)
;; citeをつけない行をlooking-atでチェックするときのregexp.
;; オリジナルはこちら →            "\\(^[^ \t\n<>]+>+[ \t]*\\|^[ \t]*$\\)"
;; これだと、単なる">"や"|"はひっかからないので、以下に変更する。
;; (setq mu-cite-cited-prefix-regexp "\\(^[^ \t>]*>+\\|^[ \t]*$\\|^[:|｜]+\\)")
(setq mu-cite-top-format
      '("川幡です。\n\n"
        ">> " id " で, \n"
        ">> " bbdb-prefix-register-verbose " は書きました. \n\n"
        "<" bbdb-prefix ">\n\n" ))
(setq mu-cite-prefix-format '("> "))

;;;; supercite.el
;; mu-cite を使うため使用しない。
;;(setq message-cite-function 'sc-cite-original)
;;(setq sc-citation-leader "")
;; supercite (使用しない)
;;(require 'bbdb-sc)
;;(setq sc-preferred-attribution-list
;;      '("sc-lastchoice" "x-attribution" "sc-consult"))
;;(add-to-list 'sc-attrib-selection-list
;;             '("sc-from-address"
;;               ((".*" . (bbdb/sc-consult-attr
;;                         (sc-mail-field "sc-from-address"))))))

;;; ローカル変数設定
;; Local Variables:
;; outline-minor-mode: t
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; eval: (hide-sublevels 5)
;; End:
