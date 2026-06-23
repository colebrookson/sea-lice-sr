# .zshrc
# Container shell config — copied to /root/.zshrc during Docker build.
# This is NOT your Mac's ~/.zshrc.

export ZSH="/root/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

# plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

# venv always on PATH (belt-and-suspenders alongside the Dockerfile ENV)
export PATH="/opt/venv/bin:$PATH"

# quality of life
export EDITOR=micro
setopt AUTO_CD          # type a directory name to cd into it
setopt HIST_IGNORE_DUPS # don't record duplicate history entries
