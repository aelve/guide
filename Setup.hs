import Distribution.Simple
import System.Process
import System.Environment (lookupEnv)

main = do
  js <- lookupEnv "NO_JS"
  case js of
    Just "true" -> defaultMain
    _           -> do
      hooks <- buildJS simpleUserHooks
      defaultMainWithHooks hooks

buildJS hooks = do
  let originalPostBuild = postBuild hooks
  return $ hooks {
    postBuild = \args flags pkgDesc localBuildInfo  -> do
      let npmbuild = proc "sh" ["./scripts/buildjs.sh"]
      (_, _, _, buildHandle) <- createProcess npmbuild
      waitForProcess buildHandle
      originalPostBuild args flags pkgDesc localBuildInfo
  }
