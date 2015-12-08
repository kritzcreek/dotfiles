source ~/dotfiles/antigen/antigen.zsh

for file in ~/.{aliases,exports,extras,functions}; do
    [ -r "$file" ] && source "$file"
done
unset file

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle cabal
antigen bundle command-not-found

# Load the theme.
antigen bundle mafredri/zsh-async
antigen bundle sindresorhus/pure

# Syntax highlighting bundle.
# antigen bundle zsh-users/zsh-syntax-highlighting

# Tell antigen that you're done.
antigen apply
