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

  describe "Handlers.createUser" $ do

    it "creates user without photo"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Nothing
                                          , CreateUser.photo_type = Nothing
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login = "notexistlogin"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "creates user with photo without ext"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Just "1"
                                          , CreateUser.photo_type = Nothing
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login = "notexistlogin"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "creates user with photo with ext"
      $              Handlers.createUser
                       dbH
                       CreateUser.Request { CreateUser.photo      = Just "1"
                                          , CreateUser.photo_type = Just "2"
                                          , CreateUser.name       = "test"
                                          , CreateUser.lastname   = "test"
                                          , CreateUser.login = "notexistlogin"
                                          , CreateUser.password   = "test"
                                          }
      `shouldReturn` "ok"

    it "throws exception if empty data"
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

    it "throws exception if login exist"
      $             Handlers.createUser
                      dbH
                      CreateUser.Request { CreateUser.photo      = Nothing
                                         , CreateUser.photo_type = Nothing
                                         , CreateUser.name       = ""
                                         , CreateUser.lastname   = ""
                                         , CreateUser.login      = "existlogin"
                                         , CreateUser.password   = ""
                                         }
      `shouldThrow` anyException

  describe "Handlers.getUsers" $ do

    it "returns users"
      $ Handlers.getUsers dbH GetUsers.Request { GetUsers.page = 1 }
      `shouldReturn` ([] :: [GetUsers.User])

    it "throws exception if page negative"
      $ Handlers.getUsers dbH GetUsers.Request { GetUsers.page = -1 }
      `shouldThrow` anyException

  describe "Handlers.deleteUser" $ do

    it "deletes user"
      $              Handlers.deleteUser
                       dbH
                       DeleteUser.Request { DeleteUser.token = "admin"
                                          , DeleteUser.login = "existlogin"
                                          }
      `shouldReturn` "ok"


    it "throws exception if user not admin"
      $             Handlers.deleteUser
                      dbH
                      DeleteUser.Request { DeleteUser.token = "noadmin"
                                         , DeleteUser.login = "existlogin"
                                         }
      `shouldThrow` anyException

    it "throws exception if login not exist"
      $             Handlers.deleteUser
                      dbH
                      DeleteUser.Request { DeleteUser.token = "admin"
                                         , DeleteUser.login = "notexistlogin"
                                         }
      `shouldThrow` anyException
