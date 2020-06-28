import           Test.Hspec

import qualified Lib.DB.Impl.Testing           as DB.Impl.Testing
import qualified Lib.Handlers                  as Handlers

import qualified Lib.Types.AddNewsComment      as AddNewsComment
import qualified Lib.Types.AddNewsPhoto        as AddNewsPhoto
import qualified Lib.Types.AddNewsTag          as AddNewsTag
import qualified Lib.Types.CreateCategory      as CreateCategory
import qualified Lib.Types.CreateNews          as CreateNews
import qualified Lib.Types.CreateTag           as CreateTag
import qualified Lib.Types.CreateUser          as CreateUser
import qualified Lib.Types.DeleteAuthor        as DeleteAuthor
import qualified Lib.Types.DeleteCategory      as DeleteCategory
import qualified Lib.Types.DeleteNews          as DeleteNews
import qualified Lib.Types.DeleteNewsComment   as DeleteNewsComment
import qualified Lib.Types.DeleteNewsPhoto     as DeleteNewsPhoto
import qualified Lib.Types.DeleteNewsTag       as DeleteNewsTag
import qualified Lib.Types.DeleteTag           as DeleteTag
import qualified Lib.Types.DeleteUser          as DeleteUser
import qualified Lib.Types.EditAuthor          as EditAuthor
import qualified Lib.Types.EditCategory        as EditCategory
import qualified Lib.Types.EditTag             as EditTag
import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
import qualified Lib.Types.GetDrafts           as GetDrafts
import qualified Lib.Types.GetNews             as GetNews
import qualified Lib.Types.GetNewsComments     as GetNewsComments
import qualified Lib.Types.GetTags             as GetTags
import qualified Lib.Types.GetUsers            as GetUsers
import qualified Lib.Types.LoginUser           as LoginUser
import qualified Lib.Types.MakeAuthor          as MakeAuthor
import qualified Lib.Types.PublishNews         as PublishNews
import qualified Lib.Types.SetNewsMainPhoto    as SetNewsMainPhoto
import qualified Lib.Types.UpdateNews          as UpdateNews

main :: IO ()
main = hspec $ do
  let dbH = DB.Impl.Testing.newHandle

  describe "create user" $ do

    it "user without photo"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Nothing
                                          , CreateUser.photo_type = Nothing
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login      = "test"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "user with photo without ext"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Just "1"
                                          , CreateUser.photo_type = Nothing
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login      = "test"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "user with photo with ext"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Just "1"
                                          , CreateUser.photo_type = Just "2"
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login      = "test"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "user with empty data (throw exception)"
      $             Handlers.createUser
                      dbH
                      CreateUser.Request { CreateUser.photo      = Nothing
                                         , CreateUser.photo_type = Nothing
                                         , CreateUser.name       = ""
                                         , CreateUser.lastname   = ""
                                         , CreateUser.login      = ""
                                         , CreateUser.password   = ""
                                         }
      `shouldThrow` anyException

    it "get users"
      $ Handlers.getUsers dbH GetUsers.Request { GetUsers.page = 1 }
      `shouldReturn` ([] :: [GetUsers.User])

    it "get users wrong page (throw exception)"
      $ Handlers.getUsers dbH GetUsers.Request { GetUsers.page = -1 }
      `shouldThrow` anyException
