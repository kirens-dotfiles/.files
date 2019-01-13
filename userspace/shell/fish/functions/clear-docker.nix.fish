{ docker, coreutils, gnugrep }:
let
  docker' = "${docker}/bin/docker";
  tail' = "${coreutils}/bin/tail";
  grep' = "${gnugrep}/bin/grep";
in
''
function clear-docker
  # Stop all running containers
  ${docker'} stop ( \
    ${docker'} ps \
    | ${tail'} -n +2 \
    | ${grep'} -oP '^[0-9a-z]{12}' \
  )

  # Remove the instances
  ${docker'} rm (${docker'} ps -aq)

  # Remove their images
  ${docker'} rmi ( \
    ${docker'} images \
    | ${tail'} -n +2 \
    | ${grep'} -oP '[0-9a-z]{12}' \
  )

  # Remove all volumes
  ${docker'} volume rm ( \
    ${docker'} volume ls \
    | ${tail'} -n +2 \
    | ${grep'} -oP '[^ ]+$' \
  )
end
''
