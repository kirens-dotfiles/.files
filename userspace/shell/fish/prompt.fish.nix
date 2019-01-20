{ powerline, bash, tput }:
''
# Greeting prompt
function fish_greeting
  echo "Welcome to fish, the friendly interactive shell"
  echo Current shell depth is (shell-lvl)
end

set -g __fish_prompt_last_lines 0
set -g __fish_prompt_last_cols 0
set -g __fish_prompt_last_scroll 0

function __fish_prompt_fixMargin
  set -l lines (${tput} lines)
  set -l cols (${tput} cols)

  test $lines != $__fish_prompt_last_lines -o $cols != $__fish_prompt_last_cols
  set -l hasScaled $status

  set -g __fish_prompt_last_lines $lines
  set -g __fish_prompt_last_cols $cols

  if test $hasScaled = 0
    # Fish automatically removes last prompt when scaling; we need to readd it
    for CHAR in (seq $__fish_prompt_last_scroll)
      ${tput} ind
    end
    # Clear any leftovers from last prompt
    ${tput} ed
    return 0
  end

  # Clear any leftovers from last prompt
  ${tput} ed
  set -l pos (${bash} -c \
    'read -sdR -p $\'\E[6n\' POS; echo "''${POS#*[}" | sed -e "s/;/\n/"')
  set -l ROWS (math $lines / 3)
  set -l ROW (math $lines - $ROWS - 1)
  if test $pos[1] -ge $ROW
    set -g __fish_prompt_last_scroll $ROWS
    # Scroll down
    for CHAR in (seq $ROWS)
      ${tput} ind
    end
    # Jump back up
    ${tput} cup $ROW
  else
    # If no scrolling occured reset last scroll
    set -g __fish_prompt_last_scroll 0
  end
end

# The function that is executed on prompt before cmd input
function fish_prompt
  set -l last_status $status

  __fish_prompt_fixMargin

  ${powerline} --shell bare --error $last_status -max-width (${tput} cols)
  printf '\n> '
end
''
