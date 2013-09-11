#!/bin/sh
# -*- outline-regexp: "^#\\*+"; -*- 

#* 対話型シェルの設定
# シェルに依存しない一般的なプロフィール設定を行う。

#  |                 | bash             | zsh       |   |
#  |-----------------+------------------+-----------+---|
#  | login           | .bash_profile or | .zprofile | 2 |
#  |                 | .bash_login or   | .zlogin   | 4 |
#  |                 | .profile         |           |   |
#  | interacitve     | .bashrc          | .zshrc or | 3 |
#  |                 |                  | .profile  |   |
#  | non-interactive | $BASH_ENV        | .zshenv   | 1 |
#  |-----------------+------------------+-----------+---|
#  | logout          | .bash_logout     | .zlogout  | 5 |
# 
# - .bash_login は、.bashrc を sourceして、BASH_ENVを設定する。
# - .bashrc は、.profileをsourceして、bash特有の設定を行う。
# - .zshrc は、zsh特有の設定を行う。
# - .zshenv は、.environ をsourceする。
# - .profile は、両者に共通の対話型設定を行う。（エイリアス・関数など）
# - .environ は、両者に共通の非対話な環境変数設定を行う。
#
# Mac の場合は、PATH/MANPATH は、/etc/paths.d, /etc/manpaths.d も参照。

#* 初期化
stty sane # デフォルトに戻す。(stty -a で確認可能)

#* ユーザへの環境情報の表示
if [ ! -z "$HTTP_PROXY" ]; then
    echo "HTTP_PROXY=$HTTP_PROXY"
fi
echo "JAVA_HOME=$JAVA_HOME"
if [ ! -z "$GPG_AGENT_INFO" ]; then
    echo "GPG_AGENT_INFO=$GPG_AGENT_INFO"
    # Macの場合は https://gpgtools.org/installer/ をインストールして、
    # pinentry-mac を ~/.gnupg/ で設定する。
    export GPG_TTY=$(tty)
fi
if [ ! -L $HOME/.zsh_history ]; then
    echo "~/.zsh_history is not symbolic link." 
fi

#* Z
[[ -f ~/Dropbox/zsh/z/z.sh ]] && source ~/Dropbox/zsh/z/z.sh

#* aliases
#** ls コマンド
# GNU ls (coreutils) をインストールしておくこと。
# loF (long, except group, Decorated), はデフォルト。R (t/S/X) を適宜追加。
alias l='ls -loF'
alias lt='ls -loFt'
alias lS='ls -loFS'
alias lx='ls -loFX'
alias la='ls -laoF'
alias lat='ls -laoFt'
alias laS='ls -laoFS'
alias lax='ls -laoFX'
alias lr='ls -loFR'
alias lrt='ls -loFRt'
alias lrs='ls -loFRS'
alias lrx='ls -loFRX'
alias lar='ls -laoFR'
alias lart='ls -laoFRt'
alias lars='ls -laoFRS'
alias larx='ls -laoFRX'
#alias lsd='ls -ld *(-/DN)'	# List only directories and symbolic
				# links that point to directories
#alias lsa='ls -ld .*'		# List only file beginning with "."
# BSD版 ls 命令で、端末にも8bitコードをそのまま出力するよう指示する。
#if [ `uname` = "Darwin" ]; then
#    alias ls='ls -w'
#fi

#** git
# ~/.gitconfigに設定する。
#git config --global user.name "Taichi KAWABATA"
#git config --global user.email kawabata.taichi@gmail.com

#** cd
alias cdsl='cd ~/.emacs.d/site-lisp'

#** MacTeX
# /usr/texbin, /Library/TeX/Distributions/

#** history
alias h=history
alias hall='history -E 1'

#** dirs
# cd または directory stack 関係 (http://www.b2pi.com/zsh/Intro/intro_6.html)
# `=x' で、x番のスタックを展開する。
alias dh='dirs -v'

#** その他
#alias mv='nocorrect mv'		# no spelling correction on mv
#alias cp='nocorrect cp'		# no spelling correction on cp
#alias mkdir='nocorrect mkdir'	# no spelling correction on mkdir
alias j=jobs
alias po=popd

# ユーティリティ関数の定義
alias less="less -X" # less 終了後に画面をクリアしない
alias mkdir='mkdir -p'
alias emacs='emacs --debug-init'
alias g='grep -a'
alias cl='clear'
alias d='ls -lFo'
#alias cctl=compctl
alias ns='netstat'
alias em='emacs'

#** alias (新しい命令)
alias root='su -l'
alias null='cat /dev/null >'
alias bench='ab -c10 -n100'	# Apache Benchmarking
#** デフォルトオプションの設定
alias diff='diff -Naur'
alias scp='scp -p'

#** Google Chrome debugging
alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-shell-port=9222'

#* functions

#** AFDKO
function spotp {
    spot -Proof $1 > `basename $1 .otf`.ps
    /usr/bin/pstopdf `basename $1 .otf`.ps `basename $1.otf`.pdf
    rm `basename $1 .otf`.ps
    open `basename $1 .otf`.pdf
}

#** csh compatibility
function setenv {
    typeset -x "${1}${1:+=}${(@)argv[2,$#]}"
}

function settitle { echo "\e]0;${1}\a" }

#** AHFormatter
alias ahf='/usr/local/AHFormatterV61/run.sh'
function ahf {
  /usr/local/AHFormatterV61/run.sh -peb 1 -x 4 -d $1 -o $1.pdf
}

# 再帰的grep
function rgrep {
  find "." -type f -name $2 -exec grep $1 {} /dev/null \;
}

function clean_tex {
 rm *.aux *.bbl *.blg *.brf *.ccs *.dvi *.ent *.fdb_latexmk *.fff \
    *.glo *.idx *.idv *.ilg *.ind *.ioa *.lg *.log *.lot *.mtc *.mlf \
    *.out *.pdfsync *.toc *.ttt *.wrm *.xref *.4ct *.4tc *.nav *.snm \
    *.bcf *.fls *.run.xml *.vrb *~ \
    *.cp *.fn *.ky *.pg *.tp *.vr
}

function bell {
  echo '\007\c'
}

# jsdoc は実行ディレクトリに移動する必要がある。
# e.g. % jsdoc-auto kanbun.js
function jsdoc-auto () {
  DIR=`pwd`
  cd ~/Dropbox/cvs/jsdoc
  ./jsdoc --verbose --destination $DIR/doc $DIR/$1
  cd -
}

source ~/.profile_extra
