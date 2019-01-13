{ }:
''
# Prevent unintentional overwrites
alias mv='mv -i'
alias cp='cp -i'

# TMux
alias tmux-end-inactive="tmux ls | grep --invert-match attached | grep -oP '^[^:]+' | xargs -L 1 tmux kill-session -t"

# Fish helper
alias prefixTime='while read pong; echo [(date +%H:%M:%S)] $pong; end'
alias prefixDatetime='while read pong; echo [(date +"%d/%m %H:%M:%S")] $pong; end'
alias trackCon='ping -i 20 1.1.1.1 | prefixTime'

alias r="vi -R"

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
alias cpy=pbcopy
alias pst=pbpaste

# Sound
alias lssound='pacmd list-sinks | grep -e device.string -e \'name:\' -e index -e sample'
alias lsaudio='pacmd list-sources | grep -e device.string -e \'name:\' -e index'

# Power mgmt
alias hibernate-pc="i3lock-fancy& systemctl hibernate"
alias sleep-pc="i3lock-fancy& systemctl suspend"

# Nixos shortcut
alias nix-storefolders="find /nix/store -maxdepth 1 -type d -name '*' -a ! -name 'trash' -a ! -wholename '/nix/store' -a ! -name '.links'"
alias nix-storefind='nix-storefolders | grep'

alias nix-system="sudo nix-env -p /nix/var/nix/profiles/system"
alias bootentry="sudo nixos-rebuild switch -p"

# Battery
alias battery="upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E 'state|time\ to\ full|time\ to\ empty|percentage'"

# Screenshots
alias screenshot-last='ls -t ~/screenshots/ | head -1 | awk \'{print "/home/kiren/screenshots/" $1}\' '
alias screenshot-view-last='screenshot-last | xargs feh'
alias screenshot-edit-last='screenshot-last | xargs gimp'
alias screenshot-remove-last='screenshot-last  | xargs rm'
alias screenshot-edit-remove-last='screenshot-edit-last; screenshot-remove-last'
''
