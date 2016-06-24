
module Reddit.Pipes.Listing
    ( getListingsP
    , getListingsAfterP
    , afterPager
    ) where

import           Pipes
import qualified Pipes.Prelude        as Pipes
import           Reddit
import           Reddit.Types.Listing

getListingsAfterP
    :: Monad m
    => (Options t -> m (Listing t a))
    -> Options t
    -> Producer [a] m ()
getListingsAfterP = getListingsP afterPager

afterPager
    :: Monad m
    => Options t
    -> Listing t a
    -> m (Maybe (Options t))
afterPager page listing = return $ do
    next <- after listing
    return $ page { pagination = Just (After next) }

getListingsP
    :: Monad m
    => (x -> Listing t a -> m (Maybe x))
    -> (x -> m (Listing t a))
    -> x
    -> Producer [a] m ()
getListingsP pager step start = Pipes.unfoldr go (Just start)
  where
    go Nothing  = return $ Left ()
    go (Just x) = do
        listing <- step x
        x' <- pager x listing
        return $ Right (contents listing, x')
