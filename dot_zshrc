# Load Oh My Posh
if [ "$TERM_PROGRAM" != "Apple_Terminal" ]; then
  eval "$(oh-my-posh init zsh --config ~/.config/bash_theme.omp.json)"
fi

# Setup zsh history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory

# Load/install znap
[[ -r ~/.znap/znap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git ~/.znap/znap
source ~/.znap/znap/znap.zsh  # Start Znap

# Start compinit
autoload -Uz compinit # https://unix.stackexchange.com/questions/339954/zsh-command-not-found-compinstall-compinit-compdef
compinit

# fsf autocompletion
znap source Aloxaf/fzf-tab

# Fish like gohst autocompletion
znap source marlonrichert/zsh-autocomplete

# Better zsh autocompletion
znap source zsh-users/zsh-autosuggestions

# Setup zoxide
eval "$(zoxide init zsh)"

# Path and Variables
PATH=$PATH:~/.local/bin
PATH=$PATH:~/.cargo/bin
PATH=$PATH:~/Scripts

# If not within tmux, bind ctrl+f to session starter
if [ -z "$TMUX" ]; then
    bindkey -s "^f" "zzz\n"
fi

# Alias
alias ls='eza --icons'
alias la='eza --icons -A'
alias ll='eza --icons -al'
alias lsg='eza --icons -D'
alias lag='eza --icons -AD'
alias llg='eza --icons -alD'
alias wal="feh --bg-fill"
alias ww='wget'
alias ree='redshift -P -O 3500'
alias vi='nvim'
alias vim='nvim'
alias del='sudo rm -r'
alias comp='sudo make install'
alias gl='git clone'
# alias update='sudo apt update && sudo apt upgrade'
# alias instal='sudo apt install'

. "$HOME/.cargo/env"
