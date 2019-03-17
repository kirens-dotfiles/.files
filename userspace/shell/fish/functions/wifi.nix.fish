{ networkmanager, echo }:
let
  nmcli = "${networkmanager}/bin/nmcli";
in ''
function wifi
  switch "''$argv[1]"
    case --help '?' -h
      ${echo} 'Usage: wifi [OPTION] [SSID]'
      ${echo}
      ${echo} 'Simple interfacing with the computers WiFi'
      ${echo}
      ${echo} 'OPTIONS'
      ${echo} '  -o, --pen  Create a new connection to `SSID` without password'
      ${echo} '  -d, --del  Remove a stored connection specified by `SSID`'
      ${echo} '  -c, --cons List saved connections'
      ${echo}
      ${echo} 'SSID'
      ${echo} '  If given an SSID without any other arguments a connection'
      ${echo} '  will be created, you will be prompted for a password if the'
      ${echo} '  connection is unknown'
    case ""
      # List available connections
      ${nmcli} device wifi list
    case --open -o
      ${nmcli} device wifi con "''$argv[2]"
    case --del -d
      ${nmcli} con delete id "''$argv[2]"
    case --cons
      ${nmcli} con
    case '*'
      if ${nmcli} connection show "''$argv[1]"
        ${nmcli} connection up "''$argv[1]"
      else
        read --local --silent --prompt-str='Enter WiFi password: ' passwd
        ${nmcli} device wifi con "''$argv[1]" "''$passwd"
      end
  end
end
''
