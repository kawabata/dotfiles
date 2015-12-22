;;; .gnus.el --- setup for Gnus -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Modified: 2015-12-21
;; Namespace: tkw-mail-
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
;; |getmail{d} +--+  | nnimap   +--+
;; +-----------+     +----------+  |
;;                                 |
;;                   +----------+  |
;;                   | nnmaildir+--+
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

;; fetchmail+procmail よりも、getmail の方が設定は楽（振り分けない限り）。
;; % mkdir -p ~/.getmail
;; % touch ~/.getmail/getmailrc-kawabata@clock.ocn.ne.jp (設定)
;; % mkdir -p ~/Maildirs/kawabata@clock.ocn.ne.jp/{cur,new,tmp}
;; % getmail --rcfile getmailrc-kawabata@clock.ocn.ne.jp

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'gnus)
(require 'gnus-msg)
(require 'message)
(require 'nnimap)
(require 'nnir)
(require 'auth-source)
(require 'mail-source)
(require 'smtpmail)
(require 'dash)

(unless (require 'bind-key nil t)
  (defun bind-key (key cmd &optional keymap)
    (define-key (or keymap global-map) (kbd key) cmd))
  (defun bind-key* (key cmd) (global-set-key (kbd key) cmd)))

;;;; 環境変数と設定情報

;; | 設定変数名                    | 定義ファイル   | 環境変数     | 注記            |
;; |-------------------------------+----------------+--------------+-----------------|
;; | user-mail-address             | startup.el     | EMAIL        |                 |
;; | mail-sources                  | mail-source.el | MAIL         | file            |
;; |                               |                | MAILHOST     | pop,imap        |
;; |                               |                | LOGNAME      | pop             |
;; |                               |                | USER         | pop             |
;; |-------------------------------+----------------+--------------+-----------------|
;; | gnus-select-method            | gnus.el        | NNTPSERVER   |                 |
;; | gnus-secondary-select-methods | gnus.el        |              |                 |
;; |-------------------------------+----------------+--------------+-----------------|
;; | gnus-posting-styles           | gnus-msg.el    |              |                 |
;; | (message-from-style)          | message.el     |              |                 |
;; | (message-signature)           | message.el     |              |                 |
;; | (message-user-organization)   | message.el     | ORGANIZATION |                 |
;; |-------------------------------+----------------+--------------+-----------------|
;; | X-Message-SMTP-Method         |                |              |                 |
;; | (smtpmail-smtp-server)        | smtpmail.el    | SMTPSERVER   |                 |
;; | (smtpmail-smtp-service)       |                |              |                 |
;; | (smtpmail-smtp-user)          |                |              |                 |
;; |-------------------------------+----------------+--------------+-----------------|
;; | (user-login-name)             |                |              | pop,imapで使用  |
;; | rmail-spool-directory         | rmail.el       |              | /var/mail, etc. |

;;; ユーザ情報設定
;;;; メールアカウント

(defun tkw-mail-pop-src (mail-address)
  "MAIL-ADDRESS に対応する SSL POP サーバの source を返す."
  `(pop :server
        ,(if (string-match "gmail" mail-address) "pop.gmail.com"
           "pop.mail.yahoo.co.jp")
        :user ,mail-address :port 995 :connection ssl))

;; tkw-mail-address・nnmail-split-fancy や、その他のプライベート情報は暗号化する。
(defvar tkw-mail-secondary-properties)
(load-file (expand-file-name "gnus-extra.el.gpg" user-emacs-directory))

(defvar tkw-mail-extra-headers
  `(("Accept-Language" "ja, en;q=0.6, zh;q=0.3, fr;q=0.1, la;q=0.01\n")
    ,@(when (executable-find "uname")
        (list (list "X-Operating-System" (shell-command-to-string "uname -mv"))))
    ("User-Agent" ,(gnus-extended-version))))

(defvar tkw-mail-spool
  (let ((spool "/var/mail/kawabata"))
    (when (file-exists-p spool)
      `((file :path ,spool)))))

(set-variable 'message-signature-directory
                "~/share/signatures/")

(defvar tkw-mail-properties nil
  "Alist of mail address vs. properties.")
(setq tkw-mail-properties
  `(("kawabata.taichi@lab.ntt.co.jp"
     (""
      :from "川幡 太一 <kawabata.taichi@lab.ntt.co.jp> ( Taichi KAWABATA )"
      :signature ".signature.ntt"
      :organization "NTT Network Innovation Laboratories"
      :extra-headers
      (("X-GPG-Fingerprint" . "E31F FE94 5036 1E3D B76D 37CB 3E38 B803 DC18 167E")
       ("X-GPG-Key" . "http://pgp.nic.ad.jp:11371/pks/lookup?op=index&search=kawabata.taichi@lab.ntt.co.jp")
       ;;,@tkw-mail-extra-headers
       ("X-Message-SMTP-Method" . "smtp 129.60.125.3 10025"))) ;; mailsv5
     ("-en"
      :refer "kawabata.taichi@lab.ntt.co.jp"
      :from "Taichi Kawabata <kawabata.taichi@lab.ntt.co.jp>"
      :signature ".signature.ntt.en"))
    ("kawabata.taichi@gmail.com"
     (""
      :from "川幡 太一 <kawabata.taichi@gmail.com> ( Taichi KAWABATA )"
      :signature ".signature.gmail"
      :organization nil
      :extra-headers
      (("X-GPG-Fingerprint" . "ECB2 051F 6D9B F347 DBEA 2792 2065 8961 AA50 61C2")
       ("X-Github" . "http://github.com/kawabata")
       ("X-GPG-Key" . "http://pgp.nic.ad.jp:11371/pks/lookup?op=index&search=kawabata.taichi@gmail.com")
       ;;,@tkw-mail-extra-headers
       ("X-Message-SMTP-Method" . "smtp smtp.gmail.com 465")))
     ("-en"
      :refer "kawabata.taichi@gmail.com"
      :from "Taichi Kawabata <kawabata.taichi@gmail.com>"
      :signature ".signature.gmail.en")
     ;;,@tkw-mail-secondary-properties
     )))

(defvar tkw-mail-host-properties)
(setq tkw-mail-host-properties
  `(("mailsv.y.ecl.ntt.co.jp" ;; mailsv5
     :primary-source (nnml "kawabata.taichi@lab.ntt.co.jp"
                           ;; メールスプールが大きすぎて、読み込めなくなる場合は、
                           ;; (1) 下記のサーバを元の mailsv に戻してメールを読む。
                           ;; (2) C:\Users\kawabata\AppData\Roaming\NTT SOFT\CipherCraft\MAIL_H\isolated
                           ;;     以下のフォルダを削除する。
                           (pop :server "129.60.125.3" :port 10111 :user "tk319")
                           ;;(pop :server "mailsv.y.ecl.ntt.co.jp" :user "tk319")
                           ))
    ("pop.gmail.com"
     :primary-source (nnml "kawabata.taichi@gmail.com"
                           ,(tkw-mail-pop-src "kawabata.taichi@gmail.com"))
     :secondary-sources (;;(nnmaildir "kawabata@meadowy.org")
                         (nnmaildir "kawabata@clock.ocn.ne.jp")
                         (nnmaildir "xprajna99@gmail.com" . maildir)
                         (nnmaildir "teufelsdrock@gmail.com" . maildir)
                         (nnimap "tckwik@gmail.com")
                         (nnimap "golconda@yahoo.co.jp")
                         (nnmaildir "kawabata@clock.ocn.ne.jp")
                         (nnimap "kawabata_taichi@yahoo.co.jp")
                         (nnimap "xkawabata@yahoo.co.jp")
                         (nntp "news.gmane.org")
                         (nntp "nntp.aioe.org")))
    ("imap.gmail.com"
     :primary-source (nnimap "kawabata.taichi@gmail.com")
     :secondary-sources ((nnmaildir "xprajna99@gmail.com")
                         (nnimap "teufelsdrock@gmail.com")
                         (nnimap "tckwik@gmail.com")
                         (nnimap "golconda@yahoo.co.jp")
                         (nnimap "kawabata_taichi@yahoo.co.jp")
                         (nnimap "xkawabata@yahoo.co.jp")
                         (nntp "news.gmane.org")
                         (nntp "nntp.aioe.org")))))

(defun tkw-mail-source-to-gnus-method (source)
  "Convert SOURCE to Gnus Method."
  (let* ((method     (car source))
         (address    (cadr source)))
    (cond ((eq method 'nnml)
           (add-to-list 'mail-sources
                        ;;(plist-get mail-props :pop-source))
                        (elt source 2))
           '(nnml ""))
          ((eq method 'nnimap)
           `(nnimap ,address
                    (nnimap-address
                     ,(cond ((string-match "gmail" address) "imap.gmail.com")
                            ((string-match "yahoo" address) "imap.mail.yahoo.co.jp")
                            (t (error "Improper imap server"))))
                    (nnimap-user ,address)))
          ((eq method 'nnmaildir)
           `(nnmaildir ,address
                       (directory ,(concat "~/Maildirs/" address))))
          ((eq method 'nntp) `(nntp ,address))
          (t (error "Improper method - %s" method)))))

(defun tkw-mail-from-p (mail-address)
  "Check if MAIL-ADDRESS match to the intended _From_ field of new mail."
  (let ((delivered-to
         (and (get-buffer gnus-article-buffer)
              (with-current-buffer gnus-article-buffer
                (message-fetch-field "Delivered-To")))))
    (message "deliverd-to=%s" delivered-to)
    (if (or
         ;; グループ名が空か "^mail" で、メールアドレスと user-mail-address が一致
         (and (or (null gnus-newsgroup-name)
                  (equal "" gnus-newsgroup-name)
                  (string-match "^mail" gnus-newsgroup-name)
                  (string-match "^nnvirtual" gnus-newsgroup-name))
              (equal mail-address user-mail-address))
         ;; グループ名とメールアドレスがマッチ (nnimap)
         (string-match mail-address gnus-newsgroup-name)
         ;; Delivered-To に mail-address 含まれる
         (and delivered-to
              (string-match mail-address delivered-to)))
        t)))

(defun tkw-mail-english-p ()
  "Check if mail is reply to English.")

;; 設定概要 (2014/2/1 改訂)
;; (tkw-mail-setup)
;; マシン環境 (system-name)
;; - iridium, scalliion, dualism, denali, golconda...
;; (1) user-mail-address は、EMAIL環境変数で決定する。（自動）
;;     または、必要に応じて手動で変更する。
;; (2) gnus-select-method は、user-mail-address で決定する。
;;     (tkw-mail-properties の :method で設定)
;; (3) mail-sources は、user-mail-address で決定する。
;;     (tkw-mail-properties の :source で設定)
;; (3-1) tkw-mail-spool が nil でなければ、これも mail-sources に使用する。
;; (4) gnus-secondary-select-methods は、user-mail-address で決定する。
;; (4-1) nnimap で読むメール
;;     (tkw-mail-secondary-addresses)
;; (4-2) nntp で読むニュース
;;     (tkw-mail-nntp-methods)
;; (5) posting-style は、user-mail-address と、tkw-mail-secondary-addresses で設定。
;;     (tkw-mail-setup-posting-styles)
;; (6) smtp 暗号化の有無は、user-mail-address で設定。
;; (7) smtp サーバは、メール送信時に posting-style の　X-Message-SMTP-Method で設定。

(defun tkw-mail-setup ()
  "メール関連変数のセットアップ.
MAILHOST環境変数に基いて設定するので、必要に応じて変更する。"
  (interactive)
  (setq mail-sources nil
        gnus-secondary-select-methods nil)
  (let* ((mail-host-props   (assoc-default (getenv "MAILHOST") tkw-mail-host-properties))
         (primary-source    (plist-get mail-host-props :primary-source))
         (secondary-sources (plist-get mail-host-props :secondary-sources))
         (mail-address      (cadr primary-source)))
    (setq user-mail-address mail-address)
    (setq gnus-select-method (tkw-mail-source-to-gnus-method primary-source))
    (dolist (source secondary-sources)
      (let ((method (tkw-mail-source-to-gnus-method source)))
        ;; (message "*method=%s" method) ; debug
        (when method
          (setq gnus-secondary-select-methods
                (add-to-list 'gnus-secondary-select-methods method t)))))
    ;; gnus-posting-styles は、マッチするものが順に蓄積されるので注意する。
    ;;(setq gnus-posting-styles
    ;;      (mapcar (lambda (props)
    ;;                `((tkw-mail-from-p ,(car props))
    ;;                  ,@(plist-get (cdr props) :posting-style)))
    ;;              tkw-mail-properties))
    (setq smtpmail-stream-type
          (unless (string-match "ntt.co.jp" user-mail-address) 'ssl)))
  ;; output for debug
  (message "user-mail-address=%s" user-mail-address)
  (message "mail-sources=%s" (pp-to-string mail-sources))
  (message "gnus-select-method=%s" gnus-select-method)
  (message "gnus-secondary-select-methods=\n%s"
           (pp-to-string gnus-secondary-select-methods))
  ;;(message "gnus-posting-styles=\n%s" (pp-to-string gnus-posting-styles))
  )

(tkw-mail-setup)

;; ------------------------------------------------------------------------

;; gnus-user-aget の末尾にmule-versionを繋げる
(when (not (stringp gnus-user-agent))
  (setq gnus-user-agent
        (concat
         (let ((gnus-user-agent '(emacs gnus type)))
           (gnus-extended-version))
         " Mule/" mule-version)))

;;; mail 設定
;;;; add-log.el
(with-eval-after-load 'add-log
  (setq add-log-mailing-address user-mail-address))
;;;; mail/sendmail.el
(setq send-mail-function 'smtpmail-send-it
      mail-interactive t ; メール送信時に完了まで待機
      mail-self-blind t ; 自分自身にブラインドメールを送る
      ;; 下記の２設定により、 smtpmail-mail-address は、From ヘッダで決定される。
      mail-specify-envelope-from t ; envelope-from は、 From: ヘッダにする。
      mail-envelope-from 'header)

;;;; mail/smtpmail.el
;;(setq smtpmail-debug-info t
;;      smtpmail-debug-verb t)


;;;; net/ldap.el
;; http://www.nic.ecl.ntt.co.jp/info/ldap/index.html
;; http://www.atmarkit.co.jp/ait/articles/0011/15/news003.html
;; 使い方 :: M-x eudc-query-form
(defvar ldap-default-host)
(defvar ldap-default-base)
(with-eval-after-load 'ldap
  ;; (when (string-match "ntt.co.jp" user-mail-address))
  (set-variable 'ldap-default-host "ldap.rdh.ecl.ntt.co.jp")
  (set-variable 'ldap-default-base "dc=ntt,dc=co,dc=jp")
  ;; ldapsearch 命令の引数
  ;; 利用LDAPサーバはSASL接続しないので、-xが必要。
  ;; また、"-tt" によりテンポラリファイルに出力される。
  (set-variable 'ldap-ldapsearch-args '("-tt" "-LL" "-x"))
  ;; % ldapsearch -x -h ldap.rdh.ecl.ntt.co.jp -b "dc=ntt,dc=co,dc=jp" "cn=山田*"
  ;; M-x trace-function ldap-search-internal
  )

;;;; net/eudc.el
;; Emacs Unified Directory Client
(declare-function eudc-protocol-set "eudc")
(with-eval-after-load 'eudc
  (require 'ldap)
  (eudc-set-server ldap-default-host 'ldap t)
  (set-variable 'eudc-default-return-attributes nil)
  (set-variable 'eudc-strict-return-matches nil)
  ;; BBDBv3はEUDC未対応。
  (set-variable 'eudc-server-hotlist `(;;("localhost" . bbdb)
                                       (,ldap-default-host . ldap)))
  (set-variable 'eudc-inline-expansion-servers 'hotlist)
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
;; .newsrcは不要（他のNewsReaderは使わず）
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)
;; 起動時はトピックモード。
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;;; gnus/gnus.el
(setq gnus-use-long-file-name t
      ;; gnus-invalid-group-regexp nil
      gnus-large-newsgroup 200 ; 200以上は大きいニュースグループ
      gnus-use-trees nil ; スレッドをツリーで表示
      gnus-novice-user nil ; Gnus 初心者 （メール削除の毎に確認）
      ;; gnus-group-use-permanent-levels 3
      gnus-use-adaptive-scoring t)
(setq gnus-summary-line-format
      ;;"%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
      "%U%R%z%I %&user-date; %(%[%4L: %-23,23f%]%) %s\n"
      )
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

;;;; gnus/gnus-dired.el
;; C-c C-m C-a … gnus-dired-attach は、自動的に diredバッファの内容を
;; メールにアタッチする。
;; C-c C-m C-l … gnus-dired-find-file-mailcap
;; C-c C-m C-p … gnus-dired-print
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

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
;; http://www.emacswiki.org/emacs/TomRauchenwald
(setq ;;gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n";; "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
      gnus-summary-line-format "%U%R%d %-5,5L %-20,20n %B%-80,80S\n"
      gnus-summary-dummy-line-format "   %(:                             :%) %S\n" ;; "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "┏● " 
      gnus-sum-thread-tree-false-root " ○ "
      gnus-sum-thread-tree-single-indent " ● "
      gnus-sum-thread-tree-leaf-with-other "┣━━❯ " 
      gnus-sum-thread-tree-vertical "┃"
      gnus-sum-thread-tree-single-leaf "┗━━❯ ")

;;;; gnus/mailcap.el
(require 'mailcap)
(dolist (extension
         '((".ppt"  . "application/vnd.ms-powerpoint")
           (".pptx"  . "application/vnd.ms-powerpoint")
           (".xls"  . "application/vnd.ms-excel")
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
      message-send-mail-function 'smtpmail-send-it) ; Emacs の SMTP 実装
;; 下記暫定
(fset 'message-expand-name 'bbdb-complete-name)

;;;; gnus/mm-decode.el
(setq mm-default-directory "~/Downloads/")

;;;; gnus/mm-sec.el
;; 署名は C-c C-m C-s C-p で。
;; 署名を自動化する。
;; (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)

;;;; gnus/nnimap.el
;; gmail/yahoo 等で共通の基本設定
(setq nnimap-server-port 993)
(setq nnimap-stream 'ssl)
(setq nnmail-expiry-wait 90)
;; メールを読む際、添付ファイルはローカルにダウンロードしない。
(setq nnimap-fetch-partial-articles t)
;; デバッグ用ログを *imap-log* に記録する。
(setq nnimap-record-commands t)

;;;; gnus/nnir.el
  
(use-package nnir-est :no-require t :defer t :ensure t
  :if (executable-find "estcmd")
  :init
  (with-eval-after-load 'nnir
    (require 'nnir-est)
    ;;(set-variable 'nnir-method-default-engines
    (setq nnir-method-default-engines
                  '((nnmaildir . est)
                    (nnimap . imap)
                    (nnml . est)
                    (nntp . gmane)))))
;; mail spool importer が動かなくなったので一時停止。
;;(when (and (executable-find "mdfind")
;;           (require 'nnir-spotlight nil t))
;;  (setq nnir-method-default-engines
;;        '((nnmaildir . spotlight)
;;          (nnimap . imap)
;;          (nnml . spotlight)
;;          (nntp . gmane))))

;;;; gnus/nnmail.el
(with-eval-after-load 'nnmail
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
  )

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
(defvar nnrss-use-local)
(with-eval-after-load 'nnrss
  (setq nnrss-use-local t))

;;;; gnus/pop3.el
(with-eval-after-load 'pop3
  (set-variable 'pop3-password-required nil)
  (set-variable 'pop3-stream-type 'starttls))

;;; 外部ライブラリ
;;;; bbdb.el
(use-package bbdb :no-require t :defer t :ensure t
  :init
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus 'bbdb-insinuate-message)
  :config
  (bbdb-initialize 'gnus 'message)
  ;; バッファでは表示のみ行い、確認や自動更新は行わない。
  (set-variable 'bbdb-mua-update-interactive-p '(query . search))
  (set-variable 'bbdb-mua-pop-up-window-size 0.3)
  (bbdb-mua-auto-update-init 'gnus))

;;;; gnus-alias
(use-package gnus-alias :no-require t :defer t :ensure t
  :init
  (with-eval-after-load 'message (require 'gnus-alias))
  :config
  (defun tkw-set-gnus-alias-identity-rules ()
    (set-variable 'gnus-alias-identity-alist
                  (cl-mapcan
                   (lambda (x)
                     (let ((address (car x))
                           (props (cdr x)))
                       (mapcar
                        (lambda (prop)
                          (let* ((suffix (car prop))
                                 (plist (cdr prop))
                                 (signature (plist-get plist :signature)))
                            `(,(concat address suffix)
                              ,(plist-get plist :refer)
                              ,(plist-get plist :from)
                              ,(plist-get plist :organization)
                              ,(plist-get plist :extra-headers)
                              ,(plist-get plist :body)
                              ,(and signature
                                    (expand-file-name
                                     signature message-signature-directory)))))
                        props)))
                   tkw-mail-properties))
    (setq gnus-alias-identity-rules
                ;;(("Description"
                ;;  ("header-name" "regexp" 'both/'current/'previous)
                ;;  "identity"))
                (mapcar
                 (lambda (x)
                   `(,(car x)
                     ("any" ,(regexp-quote (car x)) 'both)
                     ,(car x)))
                 tkw-mail-properties))
    (gnus-alias-init))
  (set-variable 'gnus-alias-verbosity          9)
  (set-variable 'gnus-alias-point-position     'empty-header-or-body)
  (set-variable 'gnus-alias-overlay-identities nil)
  (set-variable 'gnus-alias-default-identity   user-mail-address)
  (tkw-set-gnus-alias-identity-rules)
  ;;(set-variable 'gnus-alias-identity-rules...)
  ;; キーマップ設定
  (define-key message-mode-map "\C-c\C-p" 'gnus-alias-select-identity))

;;;; gnus-spotlight.el (obsoleted by nnir-spotlight.el)
;; http://www.yoshidam.net/diary/Spotlight/
;;(when (and (executable-find "mdfind")
;;           (eval-and-compile (require 'gnus-spotlight nil t)))
;;  (bind-key (kbd "C-c s") 'gnus-spotlight-search)
;;  (gnus-spotlight-insinuate))

;;;; gnus-est.el (obsoleted by nnir-est.el)
;; インデックス自動生成：下記のコマンドで必要十分に動作するが、現状では
;; 余計な引数を付けるため、後日それを削除する必要がある。
;;(lazyload () "gnus-est"
;;  (when (and (setq gnus-est-make-index-command
;;                   (executable-find "estcmd"))
;;             (require 'gnus-est nil t)
;;             (file-exists-p "/var/kawabata/hyperestraier/cascket"))
;;    (setq gnus-est-index-directory (expand-file-name "~/News/cascket"))
;;    (gnus-est-insinuate)
;;    (setq gnus-est-index-update-interval nil)))

;; 遅延配送 ;; http://www.bookshelf.jp/soft/meadow_55.html#SEC781
(gnus-delay-initialize)

;;;; ido-gnus.el <elpa>
;; (use-package ido-gnus :defer t :ensure t)

;;;; mu-cite.el
(use-package mu-cite :no-require t
  :commands (mu-cite-original)
  :config
  ;; citeをつけない行をlooking-atでチェックするときのregexp.
  ;; オリジナルはこちら →            "\\(^[^ \t\n<>]+>+[ \t]*\\|^[ \t]*$\\)"
  ;; これだと、単なる">"や"|"はひっかからないので、以下に変更する。
  ;; (setq mu-cite-cited-prefix-regexp "\\(^[^ \t>]*>+\\|^[ \t]*$\\|^[:|｜]+\\)")
  (set-variable 'mu-cite-top-format
                '("川幡です。\n\n"
                  ">> " id " で,\n"
                  ">> " bbdb-prefix-register-verbose " は書きました.\n\n"
                  "<" bbdb-prefix ">\n\n" ))
  (set-variable 'mu-cite-prefix-format '("> ")))

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
