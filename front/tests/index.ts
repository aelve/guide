import createTestCafe from 'testcafe'

(async () => {
  try {
    const isProduction = process.env.NODE_ENV === 'production'
    const startingServer = isProduction
      ? (await import('../dist/server'))
      : (await import('../server'))

    await startingServer.default
    const testcafe = await createTestCafe('localhost')

    const failedCount = await testcafe
      .createRunner()
      .src('tests/tests.js')
      .browsers('chrome')
      .run()

    testcafe.close()
    const exitCode = failedCount ? 1 : 0
    process.exit(exitCode)
  } catch (error) {
    console.error(error)
    process.exit(1)
  }
})()
