
module Reddit.Pipes.Post
    ( getPostsAfterP
    ) where

import           Pipes
import           Reddit
import           Reddit.Pipes.Listing
import           Reddit.Actions.Post
import           Reddit.Types.Listing

getPostsAfterP
    :: Monad m
    => Options PostID
    -> ListingType
    -> Maybe SubredditName
    -> Producer [Post] (RedditT m) ()
getPostsAfterP start listing subreddit = getListingsAfterP (\page -> getPosts' page listing subreddit) start
