#
# Funky fishsoup
#

# Find all my dotfiles
set -g -x DOTFILES ~/.config/..dotfiles

# Load aliases
source $DOTFILES/shells/.aliases

# Volume stuff
source $DOTFILES/shells/volume.fish
