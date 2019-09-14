import axios from 'axios'

import appConfig from '~/config'

const apiRequest = axios.create()
apiRequest.interceptors.request.use(function (requestConfig) {
  requestConfig.baseURL = axios.$app.$isServer ? `http://localhost:${appConfig.port}/api/` : '/api/'
  return requestConfig
})
apiRequest.interceptors.response.use(
  ({ data }) => data,
  (error) => {
    // skipErrorCodes used for catching 409 conflict error and displaying merge conflict dialog for some requests
    const { skipErrorCodes, requestName } = error.config

    const responseCode = error.response && error.response.status
    const isSkipErorr = skipErrorCodes && skipErrorCodes.includes(responseCode)

    if (axios.$app.$isServer || !responseCode || isSkipErorr) {
      throw error
    }

    axios.$app.errorToast({
      message: `Could not process${requestName ? ` "${requestName}"` : ''} request.`,
      details: {
        path: error.request.responseURL,
        responseCode,
        message: error.response.data
      }
    })
    throw error
  })

export default apiRequest
