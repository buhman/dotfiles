autoload -Uz edit-command-line run-help zmv
zmodload zsh/complist
autoload -Uz colors && colors
autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit
autoload -Uz vcs_info

zle -N edit-command-line
zle -N zle-line-init
zle -N zle-keymap-select

setopt auto_cd \
    glob_dots \
    hist_verify \
    hist_append \
    prompt_subst \
    share_history \
    extended_glob \
    rm_star_silent \
    hist_fcntl_lock \
    print_exit_value \
    complete_aliases \
    numeric_glob_sort \
    hist_save_no_dups \
    hist_ignore_space \
    hist_reduce_blanks \
    inc_append_history \
    hist_ignore_all_dups \
    interactive_comments

unsetopt multios

READNULLCMD=$PAGER

HISTFILE=$HOME/.zsh_history
HISTSIZE=25000
SAVEHIST=$HISTSIZE

# As we can't track directories alone with git and zsh won't make the needful
# directories either, we make them ourselves instead.
mkdir -p "$HISTFILE:h"

HELPDIR=/usr/share/zsh/$ZSH_VERSION/help

PROMPT='${ret_status} %{$fg[cyan]%}%c%{$reset_color%} ${vcs_info_msg_0_} '

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' rehash yes
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true

zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '*'
local vcs_type="%{$fg[white]%}%s "
local vcs_unstaged="%{$fg[red]%}%u"
local vcs_staged="%{$fg[green]%}%c"
local vcs_branch="%{$fg[blue]%}%b"
local vcs_format="${vcs_type}${vcs_branch}${vcs_staged}${vcs_unstaged}"
zstyle ':vcs_info:*' formats "${vcs_format}%{$reset_color%} "
local vcs_action="%{$fg[yellow]%}%a"
zstyle ':vcs_info:*' actionformats "${vcs_format}%{$reset_color%}|${vcs_action}%{$reset_color%} "

function precmd {
    vcs_info
}

# Print the current running command's name to the window title.
function preexec {
    local cmd=${1//\%/}; cmd=${cmd//\$/}
    case $TERM in
        xterm-*) print -Pn "\e]2;$cmd:q\a"
    esac
}

# Replace vimode indicators.
function zle-line-init zle-keymap-select {
    vimode=${${KEYMAP/vicmd/c}/(main|viins)/i}
    zle reset-prompt
}

# Keybinds, use emacs mode explicitly.
bindkey -e

alias rr='rm -rvI'
alias rm='rm -vI'
alias cp='cp -vi'
alias mv='mv -vi'
alias ln='ln -vi'
alias mkdir='mkdir -vp'
alias grep='grep --color=auto'

alias chmod='chmod -c --preserve-root'
alias chown='chown -c --preserve-root'
alias chgrp='chgrp -c --preserve-root'

alias ls='ls --color=auto --show-control-chars --group-directories-first -AhlXF'

# Bash-like help.
unalias run-help
alias help='run-help'

# Enable C-S-t in (vte) termite which opens a new terminal in the same working
# directory.
if [[ $VTE_VERSION ]]; then
    source /etc/profile.d/vte.sh
    __vte_prompt_command
fi
