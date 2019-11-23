#!/bin/bash
[[ ! "$(docker ps | grep 'scala')" ]] && ./setup_docker.sh
docker-compose exec scala scala ${1}
