function prgm -d lookingFor
  readlink (whereis $argv[1] | grep -Po '/.*?((?!\\\\\\\\) |$)')
end
