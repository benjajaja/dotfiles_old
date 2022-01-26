[[ $- != *i* ]] && return

# vi mode, see .inputrc
set -o vi

# auto cd
shopt -s autocd

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

shopt -s expand_aliases

# export QT_SELECT=4

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# CUSTOM:

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

alias bprofile="nv ~/.bash_profile && source ~/.bash_profile"
alias nv=nvim


# https://github.com/NerdyPepper/pista/issues/12
export PROMPT_CHAR=">"
export HIDE_HOME_CWD=1
export PROMPT_CHAR_COLOR="green"
export PROMPT_CHAR_ROOT_COLOR="green"
export SHORTEN_CWD=1
export CWD_COLOR="yellow"
export PS1='$(pista -m)'


alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
#dotfiles config --local status.showUntrackedFiles no
alias dotfiles-tig='GIT_DIR=$HOME/.dotfiles/ GIT_WORK_TREE=$HOME tig'


export GOPATH=$HOME/go
export PATH="$PATH:$HOME/.cargo/bin:$HOME/.bin:$HOME/.yarn/bin:$HOME/.local/bin:/usr/local/go/bin:$GOPATH/bin"

#[[ $XDG_VTNR -le 2 ]] && tbsm

alias ls='ls --color=auto'
alias xc='xclip -selection clipboard'

. "$HOME/.cargo/env"
source /usr/share/nvm/init-nvm.sh
nvm use default --silent
