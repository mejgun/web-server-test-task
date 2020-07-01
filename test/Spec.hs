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

justOK :: String
justOK = "ok"

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
      `shouldReturn` justOK

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
      `shouldReturn` justOK

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
      `shouldReturn` justOK

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
      `shouldReturn` justOK

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

  describe "Handlers.loginUser" $ do

    it "logins user"
      $              Handlers.loginUser
                       dbH
                       LoginUser.Request { LoginUser.password = "password"
                                         , LoginUser.login    = "login"
                                         }
      `shouldReturn` LoginUser.Token { LoginUser.token = "token" }

    it "throws exception if password not valid"
      $             Handlers.loginUser
                      dbH
                      LoginUser.Request { LoginUser.password = "nopassword"
                                        , LoginUser.login    = "login"
                                        }
      `shouldThrow` anyException

    it "throws exception if login not valid"
      $             Handlers.loginUser
                      dbH
                      LoginUser.Request { LoginUser.password = "admin"
                                        , LoginUser.login    = "nologin"
                                        }
      `shouldThrow` anyException

  describe "Handlers.deleteAuthor" $ do

    it "deletes author"
      $              Handlers.deleteAuthor
                       dbH
                       DeleteAuthor.Request { DeleteAuthor.token = "admin"
                                            , DeleteAuthor.login = "authorlogin"
                                            }
      `shouldReturn` justOK

    it "throws exception if user not admin"
      $             Handlers.deleteAuthor
                      dbH
                      DeleteAuthor.Request { DeleteAuthor.token = "notadmin"
                                           , DeleteAuthor.login = "authorlogin"
                                           }
      `shouldThrow` anyException

    it "throws exception if author not exist"
      $             Handlers.deleteAuthor
                      dbH
                      DeleteAuthor.Request { DeleteAuthor.token = "admin"
                                           , DeleteAuthor.login = "notauthorlogin"
                                           }
      `shouldThrow` anyException

  describe "Handlers.editAuthor" $ do

    it "edits author"
      $              Handlers.editAuthor
                       dbH
                       EditAuthor.Request { EditAuthor.token = "admin"
                                          , EditAuthor.login = "authorlogin"
                                          , EditAuthor.descr = "descr"
                                          }
      `shouldReturn` justOK

    it "throws exception if user not admin"
      $             Handlers.editAuthor
                      dbH
                      EditAuthor.Request { EditAuthor.token = "notadmin"
                                         , EditAuthor.descr = "descr"
                                         , EditAuthor.login = "authorlogin"
                                         }
      `shouldThrow` anyException

    it "throws exception if author not exist"
      $             Handlers.editAuthor
                      dbH
                      EditAuthor.Request { EditAuthor.token = "admin"
                                         , EditAuthor.descr = "descr"
                                         , EditAuthor.login = "notauthorlogin"
                                         }
      `shouldThrow` anyException

  describe "Handlers.getAuthors" $ do

    it
      "returns authors"
      (              Handlers.getAuthors
          dbH
          GetAuthors.Request { GetAuthors.token = "admin", GetAuthors.page = 1 }
      `shouldReturn` ([] :: [GetAuthors.Author])
      )

    it "throws exception if user not admin"
      $             Handlers.getAuthors
                      dbH
                      GetAuthors.Request { GetAuthors.token = "notadmin"
                                         , GetAuthors.page  = 1
                                         }
      `shouldThrow` anyException

    it "throws exception if page<1"
      $             Handlers.getAuthors
                      dbH
                      GetAuthors.Request { GetAuthors.token = "admin", GetAuthors.page = 0 }
      `shouldThrow` anyException

  describe "Handlers.makeAuthor" $ do

    it "makes author from user"
      $              Handlers.makeAuthor
                       dbH
                       MakeAuthor.Request { MakeAuthor.token = "admin"
                                          , MakeAuthor.login = "existlogin"
                                          , MakeAuthor.descr = "descr"
                                          }
      `shouldReturn` justOK

    it "throws exception if user not admin"
      $             Handlers.makeAuthor
                      dbH
                      MakeAuthor.Request { MakeAuthor.token = "notadmin"
                                         , MakeAuthor.descr = "descr"
                                         , MakeAuthor.login = "authorlogin"
                                         }
      `shouldThrow` anyException

    it "throws exception if user not exist"
      $             Handlers.makeAuthor
                      dbH
                      MakeAuthor.Request { MakeAuthor.token = "admin"
                                         , MakeAuthor.descr = "descr"
                                         , MakeAuthor.login = "login"
                                         }
      `shouldThrow` anyException

  describe "Handlers.createCategory" $ do

    it "creates category"
      $              Handlers.createCategory
                       dbH
                       CreateCategory.Request { CreateCategory.token  = "admin"
                                              , CreateCategory.name   = "name"
                                              , CreateCategory.parent = Nothing
                                              }
      `shouldReturn` justOK

    it "throws exception if user not admin"
      $             Handlers.createCategory
                      dbH
                      CreateCategory.Request { CreateCategory.token = "notadmin"
                                             , CreateCategory.name = "name"
                                             , CreateCategory.parent = Nothing
                                             }
      `shouldThrow` anyException

  describe "Handlers.deleteCategory" $ do

    it "deletes category"
      $              Handlers.deleteCategory
                       dbH
                       DeleteCategory.Request { DeleteCategory.token  = "admin"
                                              , DeleteCategory.cat_id = 1
                                              }
      `shouldReturn` justOK

    it "throws exception if category not exist"
      $             Handlers.deleteCategory
                      dbH
                      DeleteCategory.Request { DeleteCategory.token  = "admin"
                                             , DeleteCategory.cat_id = 0
                                             }
      `shouldThrow` anyException

    it "throws exception if user not admin"
      $             Handlers.deleteCategory
                      dbH
                      DeleteCategory.Request { DeleteCategory.token = "notadmin"
                                             , DeleteCategory.cat_id = 1
                                             }
      `shouldThrow` anyException

  describe "Handlers.editCategory" $ do

    it "deletes category"
      $              Handlers.editCategory
                       dbH
                       EditCategory.Request { EditCategory.token  = "admin"
                                            , EditCategory.cat_id = 1
                                            , EditCategory.name   = "test"
                                            , EditCategory.parent = Nothing
                                            }
      `shouldReturn` justOK

    it "throws exception if category not exist"
      $             Handlers.editCategory
                      dbH
                      EditCategory.Request { EditCategory.token  = "admin"
                                           , EditCategory.name   = "test"
                                           , EditCategory.parent = Nothing
                                           , EditCategory.cat_id = 0
                                           }
      `shouldThrow` anyException

    it "throws exception if user not admin"
      $             Handlers.editCategory
                      dbH
                      EditCategory.Request { EditCategory.token  = "notadmin"
                                           , EditCategory.name   = "test"
                                           , EditCategory.parent = Nothing
                                           , EditCategory.cat_id = 1
                                           }
      `shouldThrow` anyException

  describe "Handlers.getCategories" $ do

    it "returns categories"
      $              Handlers.getCategories
                       dbH
                       GetCategories.Request { GetCategories.page = 1 }
      `shouldReturn` ([] :: [GetCategories.Cat])

    it "throws exception if page negative"
      $             Handlers.getCategories
                      dbH
                      GetCategories.Request { GetCategories.page = -1 }
      `shouldThrow` anyException

  describe "Handlers.createTag" $ do

    it "creates tag"
      $              Handlers.createTag
                       dbH
                       CreateTag.Request { CreateTag.token = "admin"
                                         , CreateTag.name  = "test"
                                         }
      `shouldReturn` justOK

    it "throws exception if tag name already exist"
      $             Handlers.createTag
                      dbH
                      CreateTag.Request { CreateTag.token = "admin"
                                        , CreateTag.name  = "existtag"
                                        }
      `shouldThrow` anyException

    it "throws exception if user not admin"
      $             Handlers.createTag
                      dbH
                      CreateTag.Request { CreateTag.token = "notadmin"
                                        , CreateTag.name  = "test"
                                        }
      `shouldThrow` anyException

  describe "Handlers.deleteTag" $ do

    it "deletes tag"
      $              Handlers.deleteTag
                       dbH
                       DeleteTag.Request { DeleteTag.token = "admin", DeleteTag.tag_id = 1 }
      `shouldReturn` justOK

    it "throws exception if tag_id not exist"
      $             Handlers.deleteTag
                      dbH
                      DeleteTag.Request { DeleteTag.token = "admin", DeleteTag.tag_id = 0 }
      `shouldThrow` anyException

    it "throws exception if user not admin"
      $             Handlers.deleteTag
                      dbH
                      DeleteTag.Request { DeleteTag.token  = "notadmin"
                                        , DeleteTag.tag_id = 1
                                        }
      `shouldThrow` anyException

  describe "Handlers.editTag" $ do

    it "edits tag"
      $              Handlers.editTag
                       dbH
                       EditTag.Request { EditTag.token  = "admin"
                                       , EditTag.tag_id = 1
                                       , EditTag.name   = "test"
                                       }
      `shouldReturn` justOK

    it "throws exception if tag not exist"
      $             Handlers.editTag
                      dbH
                      EditTag.Request { EditTag.token  = "admin"
                                      , EditTag.name   = "test"
                                      , EditTag.tag_id = 0
                                      }
      `shouldThrow` anyException

    it "throws exception if user not admin"
      $             Handlers.editTag
                      dbH
                      EditTag.Request { EditTag.token  = "notadmin"
                                      , EditTag.name   = "test"
                                      , EditTag.tag_id = 1
                                      }
      `shouldThrow` anyException

  describe "Handlers.getTags" $ do

    it "returns tags"
      $              Handlers.getTags dbH GetTags.Request { GetTags.page = 1 }
      `shouldReturn` ([] :: [GetTags.Tag])

    it "throws exception if page<1"
      $             Handlers.getTags dbH GetTags.Request { GetTags.page = -1 }
      `shouldThrow` anyException

  describe "Handlers.addNewsComment" $ do

    it "adds comment to published news"
      $              Handlers.addNewsComment
                       dbH
                       AddNewsComment.Request { AddNewsComment.token   = "user"
                                              , AddNewsComment.news_id = 1
                                              , AddNewsComment.text    = "test"
                                              }
      `shouldReturn` justOK

    it "throws exception if news not published"
      $             Handlers.addNewsComment
                      dbH
                      AddNewsComment.Request { AddNewsComment.token   = "user"
                                             , AddNewsComment.text    = "test"
                                             , AddNewsComment.news_id = 0
                                             }
      `shouldThrow` anyException

    it "throws exception if user not user (not logged)"
      $             Handlers.addNewsComment
                      dbH
                      AddNewsComment.Request { AddNewsComment.token = "notuser"
                                             , AddNewsComment.text = "test"
                                             , AddNewsComment.news_id = 1
                                             }
      `shouldThrow` anyException

  describe "Handlers.addNewsPhoto" $ do

    it "adds photo to news"
      $              Handlers.addNewsPhoto
                       dbH
                       AddNewsPhoto.Request { AddNewsPhoto.photo      = "1"
                                            , AddNewsPhoto.photo_type = Just "png"
                                            , AddNewsPhoto.token      = "author"
                                            , AddNewsPhoto.news_id    = 1
                                            }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.addNewsPhoto
                      dbH
                      AddNewsPhoto.Request { AddNewsPhoto.photo      = "1"
                                           , AddNewsPhoto.photo_type = Nothing
                                           , AddNewsPhoto.token = "notauthor"
                                           , AddNewsPhoto.news_id    = 1
                                           }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.addNewsPhoto
                      dbH
                      AddNewsPhoto.Request { AddNewsPhoto.photo      = "1"
                                           , AddNewsPhoto.photo_type = Just "2"
                                           , AddNewsPhoto.token = "authortoken"
                                           , AddNewsPhoto.news_id    = 0
                                           }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.addNewsPhoto
                      dbH
                      AddNewsPhoto.Request { AddNewsPhoto.photo      = "a"
                                           , AddNewsPhoto.photo_type = Nothing
                                           , AddNewsPhoto.token      = "author2"
                                           , AddNewsPhoto.news_id    = 1
                                           }
      `shouldThrow` anyException
