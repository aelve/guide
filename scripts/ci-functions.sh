function run_back() {
  containerID=$(docker run -d -p 4400:4400 "quay.io/aelve/guide:$1--back")
  until docker logs $containerID 2>&1 | grep -m 1 "API is running"; do :; done
}
