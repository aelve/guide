import Distribution.Simple
import System.Process

main = do
  hooks <- buildJS simpleUserHooks
  defaultMainWithHooks hooks

buildJS hooks = do
  let originalpreBuild = preBuild hooks  
  return $ hooks {
    preBuild = \args flags -> do
      let npmbuild = proc "sh" ["./scripts/buildjs.sh"]
      (_, _, _, buildHandle) <- createProcess npmbuild
      waitForProcess buildHandle
      originalpreBuild args flags
  }
