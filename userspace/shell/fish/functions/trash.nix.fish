{ cut, du, echo, mkdir, mv, rm, test, timeout }:
''
function __trash_clear
  for t in /tmp/trash/*
    set file (cat $t/orig)
    read \
      --local \
      --nchars=1 \
      --prompt-str="Permanently remove `$file` [y/N] " \
      confirmation

    switch $confirmation
      case Y
        ${rm} -rf $t
      case y
        ${rm} -r $t
      case '*'
        # Do nothing
    end
  end
end

function __trash_determineSize
  if set size (${timeout} 1s ${du} -s $argv[1] | ${cut} -f1)
    if ${test} $size -lt 512000
      ${echo} "Good"
      return 0
    else
      ${echo} "TooBig"
      return 1
    end
  else
    ${echo} "Undetermined"
    return 2
  end
end

function __trash_testSize
  switch (__trash_determineSize $argv[1])
    case TooBig Undetermined
      read \
        --local \
        --prompt-str='Size > 0.5G or undetermined; remove anyways? [y/N] ' \
        confirmation

      switch $confirmation
        case y Y
          return 0
        case '*'
          return 1
      end
  end
end

function trash
  if test (count $argv) = 0
    set argv --help
  end

  switch $argv[1]
    case --help
      ${echo} 'Usage: trash [OPTION] FILE'
      ${echo}
      ${echo} 'Trash or permanently remove files'
      ${echo}
      ${echo} 'OPTIONS'
      ${echo} '  -d  Permanently remove FILE'
      ${echo} '  -D  Permanently remove the directory FILE'
      ${echo} '  -Df Permanently forcefully remove the directory FILE'
      ${echo}
      ${echo} 'FILE'
      ${echo} '  File or directory to trash or remove'
      return 0
    case --clean
      __trash_clear
      return 0
    case -d
      for file in $argv[2..-1]
        if __trash_testSize $file
          ${rm} $file
        end
      end
    case -D
      for file in $argv[2..-1]
        if __trash_testSize $file
          ${rm} -r $file
        end
      end
    case -Df
      for file in $argv[2..-1]
        if __trash_testSize $file
          ${rm} -rf $file
        end
      end
    case '*'
      for file in $argv[1..-1]
        if __trash_testSize $file
          set trash_dir /tmp/trash/(date -Iseconds)-(random)
          ${mkdir} -p $trash_dir
          ${echo} (pwd)/$file > $trash_dir/orig
          ${mv} $file $trash_dir/data
        end
      end
  end
end
''
