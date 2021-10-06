{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Kind

import Prelude hiding (zipWith)

-- zipWith :: forall f. f -> FuncArgs f
-- zipWith f = countArgs

main :: IO ()
main = do
  -- print $ zipWith (,,) [1, 2, 3] [True, False] ["abc", "de"]