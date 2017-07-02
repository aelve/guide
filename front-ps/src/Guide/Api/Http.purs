module Guide.Http where

import Prelude

import Control.Monad.Aff (Aff)
import Lib.IsomorphicFetch (FETCH, fetch)

fetchGithubUsers :: forall eff. Int -> Aff (fetch :: FETCH | eff) String
-- fetchGithubUsers since = fetch $ "https://api.github.com/users?since=" <> show since
fetchGithubUsers _ = fetch "https://jsonplaceholder.typicode.com/users"
