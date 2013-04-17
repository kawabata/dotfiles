;;; ebibrc.el --- my ebib setting files  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;;; Commentary:

;;; Code:
(require 'ebib)

(setq ebib-common-optional-fields
      '(translator keywords origlanguage url file location
        partinfo subtitle edition abstract note annotator
        crossref urldate address subtitle language))

(setq ebib-entry-types
      `((article
         (author sortname title sorttitle journaltitle year date)
         (volume number pages month note
                 ,@ebib-common-optional-fields))
        (book
         (author sortname title sorttitle publisher year date)
         (volume part volumes series edition
                  ,@ebib-common-optional-fields))
        (mvbook
         (author sortname title sortitle publisher year date)
         (volume volumes series edition
                  ,@ebib-common-optional-fields))
        (inbook
         (author sortname title sorttitle booktitle publisher year date)
         (bookauthor volume part pages edition volumes series month
                     note ,@ebib-common-optional-fields))
        (bookinbook
         (author sortname title sorttitle booktitle publisher year date)
         (bookauthor volume part pages edition volumes series month
                     note ,@ebib-common-optional-fields))
        (collection
         (editor sortname title sorttitle publisher year date)
         (series ,@(remq 'editor ebib-common-optional-fields)))
        (incollection
         (author sortname editor sorttitle title booktitle publisher year date)
         (pages ,@(remq 'editor ebib-common-optional-fields)))
        (manual
         (author sortname editor title sorttitle year date)
         (pages organization note ,@ebib-common-optional-fields))
        (misc
         (author sortname editor title sorttitle year date)
         (howpublished month note ,@ebib-common-optional-fields))
        (online
         (author sortname editor title sorttitle year date url)
         (subtitle language version note organization month
                   urldate))
        (thesis
         (author sortname title sorttitle type institution year date)
         (subtitle language note address url urldate month))
        (phdthesis
         (author sortname title sorttitle institution year date)
         (subtitle language note address month note url urldate))
        (unpublished
         (author sortname title sorttitle year date)
         (howpublished month note url urldate address))))
