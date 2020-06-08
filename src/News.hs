module News
  ( module News.CreateNews
  , module News.UpdateNews
  , module News.PublishNews
  , module News.SetNewsMainPhoto
  , module News.AddNewsPhoto
  , module News.DeleteNewsPhoto
  , module News.AddNewsTag
  , module News.DeleteNewsTag
  , module News.AddNewsComment
  , module News.DeleteNewsComment
  , module News.GetNewsComments
  , module News.DeleteNews
  , module News.GetNews
  )
where

import           News.AddNewsComment
import           News.AddNewsPhoto
import           News.AddNewsTag
import           News.CreateNews
import           News.DeleteNews
import           News.DeleteNewsComment
import           News.DeleteNewsPhoto
import           News.DeleteNewsTag
import           News.GetNews
import           News.GetNewsComments
import           News.PublishNews
import           News.SetNewsMainPhoto
import           News.UpdateNews
