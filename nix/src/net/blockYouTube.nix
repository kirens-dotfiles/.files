{ }:
let
  domains = [ "youtube.com" "www.youtube.com" ];
in
{
  "127.0.0.1" = domains;
  "::1" = domains;
}
