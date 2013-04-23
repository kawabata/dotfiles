;; -*- coding: utf-8; lexical-binding: t -*-

;;;
;;; Lookup settings
;;;

(setq lookup-enable-debug t)
(setq lookup-max-hits 20)
(setq lookup-title-width 26)
(setq lookup-mecab-coding-system 'utf-8)

(setq lookup-search-agents
      `(
        ;;;;
        ;;;; 日本語
        ;;;;
        ;;; 国語辞典
        (ndeb  "~/edicts/KOJIEN6")
        (ndeb  "~/edicts/SKP") ; スーパー日本語大辞典
        (ndeb  "~/edicts/MANYO") ; 万葉集
        (ndeb  "~/edicts/KQJEXPRS") ;; 日本語表現活用辞典
        (ndmecab)
        (ndweb "dictionary.goo.ne.jp/jn2")
        (ndweb "dictionary.goo.ne.jp/thsrs")
        ;;; 百科事典
        (ndeb  "~/edicts/ENCY") ; 平凡社・世界大百科事典
        (ndeb  "~/edicts/SSN") ; 小学館・スーパーニッポニカ
        (ndeb  "~/edicts/britannica")
        (ndeb  "~/edicts/MYPAEDIA") ; Mypedia98
        (ndeb  "~/edicts/WorldFactBook2004")
        (ndeb  "~/edicts/PDH03")
        (ndeb  "~/edicts/GENDAI19/GENDAI10")
        (ndeb  "~/edicts/GENDAI19/Gen2009")
        (ndweb "www.google.com")
        (ndweb "ja.wikipedia.org")
        (ndweb "k.hatena.ne.jp")
        (ndweb "kotobank.jp")
        (ndweb "mycroft:daijirin")
        (ndweb "mycroft:sanseidoyojijukugo-c")
        (ndzim  "~/edicts/OpenZIM")
        ;;; 特殊辞典
        (ndweb "mycroft:goo-lyrics")
        (ndweb "mycroft:goo-music")
        (ndweb "rnnnews.jp")

        ;;;;
        ;;;; 英語
        ;;;;
        ;;; 英和辞典
        (ndweb "dictionary.goo.ne.jp/ej3")
        (ndweb "mycroft:kenkwaeichujiten-cl")
        (ndweb "mycroft:yahoodicenjp")
        (ndweb "mycroft:eijiro")
        (ndtext "~/edicts/eijiro/EIJIRO"
                :extension ".utf8.TXT")
        (ndeb "~/edicts/OXFORD")
        (ndeb "~/edicts/Unno5")
        (ndeb "~/edicts/LDOCE5")
        (ndeb "~/edicts/KENKYUSHA_COLLOCATIONS")
        (ndeb "~/edicts/KENKYUSHA_READERS_PLUS_V2")
        (ndeb "~/edicts/RIKAGAKU_EIWA")
        (ndeb "~/edicts/SAITOWAEI_EP")
        (ndeb "~/edicts/daijirin")
        ;;(ndpdic "~/edicts/eijiro/PDIC-UNI")
        (ndsrd  "~/edicts/SRD/DATA")
        ;; :fmt "~/edicts/SRD/DATA/csrd.fmt")
        (ndtext "~/edicts/sdic" :extension ".sdic")
        (ndspell "--lang=en")
        ;; 和英辞典
        (ndweb "dictionary.goo.ne.jp/je2")
        (ndweb "mycroft:yahoo_dictionary")
        (ndsary "~/edicts/JMdict") ; Jim Breen's JMDict
        ;; 英語辞典・百科事典
        (ndweb "en.wikipedia.org")
        (ndweb "en.wiktionary.org")
        (ndweb "dictionary.cambridge.org")
        (ndweb "mycroft:webster-med")
        (ndweb "mycroft:webster-thsrs")
        (ndweb "mycroft:oald8")
        (ndweb "www.oxfordjournals.org")
        (ndweb "mycroft:blaut")
        (ndweb "mycroft:bltit")
        (ndweb "mycroft:collins-cobuild")
        (ndweb "mycroft:webster")
        (ndweb "www.ldoceonline.com")
        (ndweb "mycroft:bitex")
        (ndweb "mycroft:hjenglish-j2c")

        ;;;; 
        ;;;; 中国語
        ;;;;
        ;;; 日中辞書
        (ndeb  "~/edicts/ZHONG_RI")
        (ndpdic "~/edicts/cj2") ; Karak 日中辞書
        (ndweb "dictionary.goo.ne.jp/jc")
        ;;(ndweb "www.frelax.com" :title "書虫 Pinyin"
        ;; :results
        ;; "http://www.frelax.com/cgi-local/pinyin/hz2py.cgi?hanzi={searchTerms}&mark=&jthz="
        ;; :http-method "post")
        (ndweb "dictionary.goo.ne.jp/cj")
        (ndweb "www.zdic.net"
               :title "漢典"
               :charsets (han)
               :results "http://www.zdic.net/sousou/?q={searchTerms}"
               :method "post")
        (ndweb "www.thefreedictionary.com")
        (ndweb "www.iciba.com")
        (ndweb "bitex-cn.com")

        ;;;;
        ;;;; 漢字・漢文
        ;;;;
        ;;(ndbuffer "~/edicts/KDP"
        ;;        :extension ".txt")
        (ndtext "~/edicts/KDP"
                :extension ".xml")
        (ndeb   "~/edicts/KANJIGN4") ; 漢字源
        (ndjitsuu "~/edicts/Jitsuu") ; 字通
        (ndsary "~/edicts/Jitsuu/JitsuuFuroku")
        (nddsl "~/edicts/hydcd" :backend ndbuffer)
        (ndsary "~/edicts/WaseiKanji")
        (ndbuffer "~/edicts/zigen" :extension ".xml")
        (ndsary "~/Dropbox/cvs/kanji-database/data")
        (ndpdic "~/edicts/pdic/cj2")
        (ndtext "~/edicts/chinakan" :extension ".csv")
        (ndtext "~/edicts/UCD" :extension ".xml")

        ;;;;
        ;;;; 仏教・サンスクリット語辞典
        ;;;;
        (ndeb   "~/edicts/monier-morph-allidx")
        (ndeb   "~/edicts/CEBD") ; 
        (ndeb   "~/edicts/MWSEDIC")
        (ndtext "~/edicts/Rangjung")
        (ndsary "~/edicts/acmuller") ; A.C.Muller's Index

        ;;;;
        ;;;; ラテン語
        ;;;;
        (ndlatin)
        (ndeb "~/edicts/homerpack-allidx")
        (ndeb "~/edicts/caesarpack-allidx")
        (ndeb "~/edicts/shakespack-allidx")
        (ndeb "~/edicts/biblepack-allidx-wav")

        ;;;;
        ;;;; その他の言語
        ;;;;
        ;;; フランス語
        (ndeb   "~/edicts/CROWNF_E")
        ;;(ndspell "--lang=fr")
        ;;; ドイツ語
        (ndeb   "~/edicts/CROWND_E")
        ;(ndspell "--lang=de")
        ;;; 韓国語
        ;;(ndpdic "~/edicts/pdic/kazuo_korean")
        ;;; タイ語
        (ndpdic "~/edicts/PdicThai")

        ;;;;
        ;;;; 検索エンジン
        ;;;;
        (ndspotlight)
        ;;(ndspotlight "~/Documents/01_研究関連")
        ;;(ndwinsearch)
        ;;(ndest "http://athlon64.fsij.org/~mikio/wikipedia/estfraud.cgi/casket")

        ;;;;
        ;;;; その他の辞典
        ;;;;
        ;;(ndic  "~/edicts/foldoc") ; dict がコンパイルできないのでサポート中止
        ;;;; その他
        (ndbuffer "~/edicts/rfc" :extension ".xz") ; RFC辞書
        (ndbtonic "~/edicts/OnMusic") ; 音楽中辞典
        (ndeb   "~/edicts/Cocktail") ; カクテル辞典
        (ndeb   "~/edicts/BIBLE") ; 聖書辞典
        (ndeb   "~/edicts/DEVIL")  ; 悪魔の辞典
        (ndeb   "~/edicts/FLOWER") ; 花の咲く草木名小辞典
        (ndeb   "~/edicts/PEJV") ; エスペラント語辞典
        (ndeb   "~/edicts/ROGET") ; 
        (ndeb   "~/edicts/VERA") ; 
        (ndeb   "~/edicts/WEB") ; WebStar's 1913 Dictionary
        (ndeb   "~/edicts/YASOU") ; 野草の辞典
        (ndwnj  "~/edicts/wordnet/wnjpn-1.1.db")

        ;;;;
        ;;;; おまけ
        ;;;;
        ,@(cond ((file-exists-p "~/cvs/emacs-trunk/etc")
                 '((ndcookie "~/cvs/emacs-trunk/etc")))
                ((file-exists-p "~/cvs/emacs/etc")
                 '((ndcookie "~/cvs/emacs/etc"))))))

(setq lookup-search-agents
      (cl-delete-if (lambda (x) (and (or (equal (car x) 'ndtext)
                                         (equal (car x) 'ndeb)
                                         (equal (car x) 'ndsary)
                                         (equal (car x) 'ndpdic))
                                     (not (file-exists-p (cadr x))))) 
                    lookup-search-agents))

;;;
;;; Agent Settings
;;; 

;; ndspotlight
(setq ndspotlight-content-program "arch")
(setq ndspotlight-content-program-options '("-i386" "mdimport" "-n" "-d2"))

;; ndlatin
(dolist (file (list "/Applications/Interpres.app/Contents/Resources/words"
                    "~/bin/words-1.97Ed/words"
                    "~/bin/words/words.exe"))
  (when (file-executable-p file) (setq ndlatin-program file)))


;;;
;;; Dictionary Settings
;;;

(setq lookup-dictionary-option-alist
      '(("ndeb:~/edicts/monier-morph-allidx/clsskt" :title "Monier Sanskrit")))

;;;
;;; Support File Settings
;;; 

(setq lookup-support-autoload-alist
      '(
        ;("/swjz\\.xml" . "support-swjz")
        ;("/sbgy\\.xml" . "support-sbgy")
        ;("/xszd\\.txt" . "support-xszd")
        ;("/dkw-word\\.txt" . "support-dkw-word")
        ;("/dkw2ucs\\.txt" . "support-dkw2ucs")
        ;("/mochizuki\\.txt" . "support-mochizuki")
        ))

(setq support-chinakan-url-format
      (concat "file://" (expand-file-name"~/edicts/chinakan/chinakan.pdf") "&page=%s"))
(setq support-unihan-kangxi-url-format
      (concat "file://" (expand-file-name"~/Resources/KangXi/kangxi-irg.pdf") "&page=%s"))
