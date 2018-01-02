### Set optinos ###
# Print full names
set -x fish_prompt_pwd_dir_length 0


# The function that is executed on prompt before cmd input
function fish_prompt
  # Keep margin at bottom 1/3
    set -l cRow (bash -c 'IFS=\';\' read -sdR -p $\'\E[6n\' ROW COL;echo "${ROW#*[}"')
    set -l lines (tput lines)
    set -l ROWS (expr "$lines" / 3)
    set -l ROW (expr "$lines" - "$ROWS" - 1)
    if test "$cRow" -ge "$ROW"
    for CHAR in (seq "$ROWS")
            printf "\n"
        end
        tput cup "$ROW"
    end

  # Which branch are we on?
    set -l git_branch (git branch ^/dev/null | sed -n '/\* /s///p')
    set -l git ""
    if test (string length "$git_branch") -ne 0
        set git "{$git_branch}"
    end

  # Print everything
    echo -s "$USER" @ (prompt_hostname) \
         ' ' (set_color $fish_color_cwd) (prompt_pwd) \
         (set_color $fish_color_param) " $git " (set_color normal)
    echo -n -s "> "
end
