{ }:
''
# TMux
alias tmux-end-inactive="tmux ls | grep --invert-match attached | grep -oP '^[^:]+' | xargs -L 1 tmux kill-session -t"

# Lampa
alias lampa='bash -c \'curl "https://lampa.click/lampa?mode=rgb&time=1000&hex=$0"\' '

# Fish helper
alias prefixTime='while read pong; echo [(date +%H:%M:%S)] $pong; end'
alias prefixDatetime='while read pong; echo [(date +"%d/%m %H:%M:%S")] $pong; end'
alias trackCon='ping -i 20 1.1.1.1 | prefixTime'

alias r="vi -R"

# ELM
alias elm-env="nix-shell -I nixpkgs='https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz' -p elmPackages.elm"

# Git shorthands
alias gs="git status"
alias gsu="gs -uno"
alias ga="git add"
alias gc="git commit"
alias gcm="gc -m"
alias gcam="gc -am"
alias gf="git fetch"
alias gm="git merge"
alias gp="git push"
alias gd="git diff"
alias gl="git log"
alias gl1="gl -n 1"
alias gprune="gf --prune"

# LaTeX stuff
alias mkxe='sh -c \'xelatex -halt-on-error -output-directory=./out $0 > ./out/std_out\' '

# Move about and look around
alias l="ls -lAh"
alias la="ls -A"
alias ld="l -d"
alias llast='sh -c \'ls -hlt $0 | head -2\' '
alias llasts='sh -c \'ls -hlt $1 | head -$(( 1 + $0 ))\' '

# Copy paste pipe
alias pbcopy="xclip -selection clipboard"
alias cpy=pbcopy
alias pbpaste="xclip -o -selection clipboard"
alias pst=pbpaste

# Quick clear
alias cls=clear

# Edit aliases
alias aliasstore="vi $DOTFILES/userspace/fish/aliases.sh.nix"

# WiFi
alias wifi="nmcli device wifi list"
alias addcon_open="sudo nmcli device wifi con"
alias addcon='sudo cat | head -1 | awk \'{print "password\n" $0}\' | xargs -d \'\n\' sudo nmcli device wifi con'
alias cons='nmcli con'
alias delcon='sudo nmcli con delete id'

# Sound
alias lssound='pacmd list-sinks | grep -e device.string -e \'name:\' -e index -e sample'
alias lsaudio='pacmd list-sources | grep -e device.string -e \'name:\' -e index'

# Power mgmt
alias hibernate-pc="i3lock-fancy& systemctl hibernate"
alias sleep-pc="i3lock-fancy& systemctl suspend"

# Nixos shortcut
#alias nxc="vi /etc/nixos/configuration.nix"
#alias snxc="sudo nvim /etc/nixos/configuration.nix"
alias nxc="vi -R $DOTFILES/nix/src"
alias snxc="bash $DOTFILES/executables/enix"
alias nix-pkgsearch='nix-env -qaP | grep '
alias nix-hspackages="nix-env -f \"<nixpkgs>\" -qaP -A haskellPackages"
alias nix-storefolders="find /nix/store -maxdepth 1 -type d -name '*' -a ! -name 'trash' -a ! -wholename '/nix/store' -a ! -name '.links'"
alias nix-storefind='nix-storefolders | grep'

alias nix-garbagecollect='nix-store --gc'
alias nix-mygens='nix-env --list-generations'
alias nix-gens="sudo nix-env -p /nix/var/nix/profiles/system --list-generations"
alias nix-delgen="sudo nix-env -p /nix/var/nix/profiles/system --dry-run --delete-generations"
alias nix-dodelgen="sudo nix-env -p /nix/var/nix/profiles/system --delete-generations"

alias bootentry="sudo nixos-rebuild switch -p"

alias unlinkResults='find -name result | xargs -t -L 1 unlink'

alias nx-shell='nix-shell --command fish'

# Battery
alias battery="upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E 'state|time\ to\ full|time\ to\ empty|percentage'"

# Screen
alias brightness="xrandr --output eDP1 --brightness"

# Screenshots
alias screenshot-last='ls -t ~/screenshots/ | head -1 | awk \'{print "/home/kiren/screenshots/" $1}\' '
alias screenshot-view-last='screenshot-last | xargs feh'
alias screenshot-edit-last='screenshot-last | xargs gimp'
alias screenshot-remove-last='screenshot-last  | xargs rm'
alias screenshot-edit-remove-last='screenshot-edit-last; screenshot-remove-last'

alias screenshots="feh --draw-filename --thumbnails -E 200 -y 350 ~/screenshots/"

# xterm-color
alias dark="xrdb -merge ~/.config/..dotfiles/shells/xterm/themes/solarized.dark"
alias light="xrdb -merge ~/.config/..dotfiles/shells/xterm/themes/solarized.light"
''
