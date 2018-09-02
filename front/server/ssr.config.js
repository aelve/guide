const { appName } = require('../build/build-config')
const moment = require('moment')

function templateEnvs () {
  return {
    title: appName,
    isSSR: true,
    renderTime: moment(new Date()).format('YYYY-MM-DD HH:mm:ss')
  }
}

module.exports = {
  templateEnvs
}
