function trash
  set trash_dir /tmp/trash/(date -Iseconds)-(random)
  mkdir -p $trash_dir
  echo (pwd)/$argv[1] > $trash_dir/orig
  mv $argv[1] $trash_dir/data
end trash
