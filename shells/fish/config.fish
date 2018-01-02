#
# Funky fishsoup
#

# Modify environment
# Find all my dotfile-executables
set -gx PATH $DOTFILES/executables $PATH

# Load aliases
source $DOTFILES/shells/fish/.aliases

# Volume stuff
source $DOTFILES/shells/fish/volume.fish
