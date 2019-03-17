{ curl }:
''
function lampa
  ${curl}/bin/curl "https://lampa.click/lampa?mode=rgb&time=1000&hex=$argv[1]"
end
''
