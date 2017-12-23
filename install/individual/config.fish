echo "set -g -x DOTFILES \"$(pwd)\"" > ${HOME}/.config/fish/config.fish
echo "source \"$(pwd)/shells/fish/config.fish\"" >> ${HOME}/.config/fish/config.fish
