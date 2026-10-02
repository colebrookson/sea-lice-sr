# .zshrc
# Container shell config — copied to /root/.zshrc during Docker build

export ZSH="/root/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

# plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

# venv always on PATH 
export PATH="/opt/venv/bin:$PATH"

export EDITOR=micro
setopt AUTO_CD          
setopt HIST_IGNORE_DUPS 
