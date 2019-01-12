{ expr, powerline, bash, tput }:
''
# Greeting prompt
function fish_greeting
  echo "Welcome to fish, the friendly interactive shell"
  echo Current shell depth is (shell-lvl)
end



### Set optinos ###
# Print full names
set -x fish_prompt_pwd_dir_length 0


## Git Shell stuff
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
#set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch $fish_color_param
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red
# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'


function __fish_prompt_fixMargin
  # Keep margin at bottom 1/3
  set -l cRow (${bash} -c 'IFS=\';\' read -sdR -p $\'\E[6n\' ROW COL;echo "''${ROW#*[}"')
  set -l lines (${tput} lines)
  set -l ROWS (${expr} "$lines" / 3)
  set -l ROW (${expr} "$lines" - "$ROWS" - 1)
  if test "$cRow" -ge "$ROW"
    for CHAR in (seq "$ROWS")
        printf "\n"
    end
    ${tput} cup "$ROW"
  end
end

# Prompt non-zero exits
function __fish_prompt_status
  if test "$argv[1]" -ne 0
    printf [
    set_color red
    printf "$argv[1]"
    set_color normal
    printf '] '
  end
end

# The function that is executed on prompt before cmd input
function fish_prompt
  set last_status $status

  __fish_prompt_fixMargin

  if type -q ${powerline}
    ${powerline} --shell bare --error $last_status -max-width (${tput} cols)
    printf '\n> '
  else
    # Print everything
    echo -s (__fish_prompt_status "$last_status") "$USER" @ (prompt_hostname) \
      ' ' (set_color $fish_color_cwd) (prompt_pwd) \
      (__fish_git_prompt)
    echo -n -s "> "
  end
end
''
