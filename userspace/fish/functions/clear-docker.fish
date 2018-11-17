function clear-docker
  docker stop (docker ps | tail -n +2 | grep -oP '^[0-9a-z]{12}')
  docker rm (docker ps -aq)
  docker rmi (docker images | tail -n +2 | grep -oP '[0-9a-z]{12}')
  docker volume rm (docker volume ls | tail -n +2 | grep -oP '[^ ]+$')
end
