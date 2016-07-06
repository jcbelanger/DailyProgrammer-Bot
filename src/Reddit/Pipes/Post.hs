
module Reddit.Pipes.Post
    ( getPostsAfterP
    ) where

import           Pipes
import           Pipes.Prelude        as Pipes
import           Reddit
import           Reddit.Actions.Post
import           Reddit.Pipes.Listing
import           Reddit.Types.Listing

getPostsAfterP
    :: Monad m
    => Options PostID
    -> ListingType
    -> Maybe SubredditName
    -> Producer [Post] (RedditT m) ()
getPostsAfterP start listing subreddit = getListingsAfterP (\page -> getPosts' page listing subreddit) start
