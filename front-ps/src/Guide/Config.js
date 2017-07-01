exports.config = {
  title: 'Aelve guide',
  public_path: process.env.NODE_ENV === 'production'
               ? '/dist'
               : 'http://localhost:8080/static/dist',
  isProduction: false, // TODO (sectore): Set it by DefinePlugin (webpack), which failed currently
  isServer: false, // TODO (sectore): Set it by DefinePlugin (webpack), which failed currently

}
