export default function (path) {
  return path.split('#').shift().split('-').pop()
}
