{ curl, config }:
''
function lampa
  ${curl}/bin/curl \
     -H 'Authorization: ${config.systemConfig.myCfg.lampaPass}' \
    "https://lampa.click/lampa?mode=rgb&ciel&desk&duration=1000&hex=$argv[1]"
end
''
