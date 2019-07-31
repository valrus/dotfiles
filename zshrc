# User configuration sourced by interactive shells
# source ~/.zshenv

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

### Improve vi mode
# http://stratus3d.com/blog/2017/10/26/better-vi-mode-in-zshell/

# Set vi mode in the first place
bindkey -v

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Better searching
bindkey -M vicmd '?' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-forward

# Begin search with arrow keys
bindkey "^[0A" up-line-or-beginning-search
bindkey "^[0B" down-line-or-beginning-search
bindkey -M vicmd "k" up-line-or-beginning-search
bindkey -M vicmd "j" down-line-or-beginning-search

# Visual mode hack
autoload edit-command-line
zle -N edit-command-line
bindkey -M vicmd "e" edit-command-line

# Use vim without vimrc for default editor
export EDITOR='vim -u none'

# More responsive mode transitions
export KEYTIMEOUT=1

# Mode indicator in prompt
function zle-keymap-select() {
  zle reset-prompt
  zle -R
}

zle -N zle-keymap-select

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/[% NORMAL]%}/(main|viins)/[% INSERT]%}"
}

# define right prompt, regardless of whether the theme defined it
RPS1='$(vi_mode_prompt_info)'
RPS2=$RPS1

# Incorporate local settings
source ~/.zshrc.local

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
