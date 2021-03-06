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

  describe "Handlers.addNewsTag" $ do

    it "adds tag to news"
      $              Handlers.addNewsTag
                       dbH
                       AddNewsTag.Request { AddNewsTag.tag_id  = 1
                                          , AddNewsTag.token   = "author"
                                          , AddNewsTag.news_id = 1
                                          }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.addNewsTag
                      dbH
                      AddNewsTag.Request { AddNewsTag.tag_id  = 1
                                         , AddNewsTag.token   = "notauthor"
                                         , AddNewsTag.news_id = 1
                                         }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.addNewsTag
                      dbH
                      AddNewsTag.Request { AddNewsTag.tag_id  = 1
                                         , AddNewsTag.token   = "authortoken"
                                         , AddNewsTag.news_id = 0
                                         }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.addNewsTag
                      dbH
                      AddNewsTag.Request { AddNewsTag.tag_id  = 1
                                         , AddNewsTag.token   = "author2"
                                         , AddNewsTag.news_id = 1
                                         }
      `shouldThrow` anyException

    it "throws exception if tag not exist"
      $             Handlers.addNewsTag
                      dbH
                      AddNewsTag.Request { AddNewsTag.tag_id  = 0
                                         , AddNewsTag.token   = "author"
                                         , AddNewsTag.news_id = 1
                                         }
      `shouldThrow` anyException

  describe "Handlers.createNews" $ do

    it "creates news (draft)"
      $              Handlers.createNews
                       dbH
                       CreateNews.Request { CreateNews.cat_id = 1
                                          , CreateNews.token  = "author"
                                          , CreateNews.name   = "name"
                                          , CreateNews.text   = "text"
                                          }
      `shouldReturn` CreateNews.NewsId { CreateNews.news_id = 1 }

    it "throws exception if user not author"
      $             Handlers.createNews
                      dbH
                      CreateNews.Request { CreateNews.cat_id = 1
                                         , CreateNews.token  = "notauthor"
                                         , CreateNews.name   = "name"
                                         , CreateNews.text   = "text"
                                         }
      `shouldThrow` anyException

    it "throws exception if category not exist"
      $             Handlers.createNews
                      dbH
                      CreateNews.Request { CreateNews.cat_id = 0
                                         , CreateNews.token  = "author"
                                         , CreateNews.name   = "name"
                                         , CreateNews.text   = "text"
                                         }
      `shouldThrow` anyException

  describe "Handlers.deleteNews" $ do

    it "deletes news"
      $              Handlers.deleteNews
                       dbH
                       DeleteNews.Request { DeleteNews.token   = "author"
                                          , DeleteNews.news_id = 1
                                          }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.deleteNews
                      dbH
                      DeleteNews.Request { DeleteNews.token   = "notauthor"
                                         , DeleteNews.news_id = 1
                                         }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.deleteNews
                      dbH
                      DeleteNews.Request { DeleteNews.token   = "authortoken"
                                         , DeleteNews.news_id = 0
                                         }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.deleteNews
                      dbH
                      DeleteNews.Request { DeleteNews.token   = "author2"
                                         , DeleteNews.news_id = 1
                                         }
      `shouldThrow` anyException

  describe "Handlers.deleteNewsComment" $ do

    it "deletes comment"
      $              Handlers.deleteNewsComment
                       dbH
                       DeleteNewsComment.Request { DeleteNewsComment.token = "admin"
                                                 , DeleteNewsComment.comment_id = 1
                                                 }
      `shouldReturn` justOK

    it "throws exception if user not admin"
      $             Handlers.deleteNewsComment
                      dbH
                      DeleteNewsComment.Request { DeleteNewsComment.token = "notadmin"
                                                , DeleteNewsComment.comment_id = 1
                                                }
      `shouldThrow` anyException

  describe "Handlers.deleteNewsPhoto" $ do

    it "deletes news photo"
      $              Handlers.deleteNewsPhoto
                       dbH
                       DeleteNewsPhoto.Request { DeleteNewsPhoto.token = "author"
                                               , DeleteNewsPhoto.news_id = 1
                                               , DeleteNewsPhoto.photo_id = 1
                                               }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.deleteNewsPhoto
                      dbH
                      DeleteNewsPhoto.Request { DeleteNewsPhoto.token = "notauthor"
                                              , DeleteNewsPhoto.news_id = 1
                                              , DeleteNewsPhoto.photo_id = 1
                                              }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.deleteNewsPhoto
                      dbH
                      DeleteNewsPhoto.Request { DeleteNewsPhoto.token = "authortoken"
                                              , DeleteNewsPhoto.news_id = 0
                                              , DeleteNewsPhoto.photo_id = 0
                                              }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.deleteNewsPhoto
                      dbH
                      DeleteNewsPhoto.Request { DeleteNewsPhoto.token = "author2"
                                              , DeleteNewsPhoto.news_id = 1
                                              , DeleteNewsPhoto.photo_id = 1
                                              }
      `shouldThrow` anyException

  describe "Handlers.deleteNewsTag" $ do

    it "deletes tag from news"
      $              Handlers.deleteNewsTag
                       dbH
                       DeleteNewsTag.Request { DeleteNewsTag.tag_id  = 1
                                             , DeleteNewsTag.token   = "author"
                                             , DeleteNewsTag.news_id = 1
                                             }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.deleteNewsTag
                      dbH
                      DeleteNewsTag.Request { DeleteNewsTag.tag_id = 1
                                            , DeleteNewsTag.token = "notauthor"
                                            , DeleteNewsTag.news_id = 1
                                            }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.deleteNewsTag
                      dbH
                      DeleteNewsTag.Request { DeleteNewsTag.tag_id = 1
                                            , DeleteNewsTag.token = "authortoken"
                                            , DeleteNewsTag.news_id = 0
                                            }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.deleteNewsTag
                      dbH
                      DeleteNewsTag.Request { DeleteNewsTag.tag_id  = 1
                                            , DeleteNewsTag.token   = "author2"
                                            , DeleteNewsTag.news_id = 1
                                            }
      `shouldThrow` anyException

    it "throws exception if tag not exist"
      $             Handlers.deleteNewsTag
                      dbH
                      DeleteNewsTag.Request { DeleteNewsTag.tag_id  = 0
                                            , DeleteNewsTag.token   = "author"
                                            , DeleteNewsTag.news_id = 1
                                            }
      `shouldThrow` anyException

  describe "Handlers.publishNews" $ do

    it "publishes news"
      $              Handlers.publishNews
                       dbH
                       PublishNews.Request { PublishNews.publish = True
                                           , PublishNews.token   = "author"
                                           , PublishNews.news_id = 1
                                           }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.publishNews
                      dbH
                      PublishNews.Request { PublishNews.publish = True
                                          , PublishNews.token   = "notauthor"
                                          , PublishNews.news_id = 1
                                          }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.publishNews
                      dbH
                      PublishNews.Request { PublishNews.publish = True
                                          , PublishNews.token   = "authortoken"
                                          , PublishNews.news_id = 0
                                          }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.publishNews
                      dbH
                      PublishNews.Request { PublishNews.publish = True
                                          , PublishNews.token   = "author2"
                                          , PublishNews.news_id = 1
                                          }
      `shouldThrow` anyException

  describe "Handlers.setNewsMainPhoto" $ do

    it "sets news' main photo"
      $              Handlers.setNewsMainPhoto
                       dbH
                       SetNewsMainPhoto.Request { SetNewsMainPhoto.photo = "photo"
                                                , SetNewsMainPhoto.photo_type = Nothing
                                                , SetNewsMainPhoto.token = "author"
                                                , SetNewsMainPhoto.news_id = 1
                                                }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.setNewsMainPhoto
                      dbH
                      SetNewsMainPhoto.Request { SetNewsMainPhoto.photo = "photo"
                                               , SetNewsMainPhoto.photo_type = Nothing
                                               , SetNewsMainPhoto.token = "notauthor"
                                               , SetNewsMainPhoto.news_id = 1
                                               }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.setNewsMainPhoto
                      dbH
                      SetNewsMainPhoto.Request { SetNewsMainPhoto.photo = "photo"
                                               , SetNewsMainPhoto.photo_type = Nothing
                                               , SetNewsMainPhoto.token = "authortoken"
                                               , SetNewsMainPhoto.news_id = 0
                                               }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.setNewsMainPhoto
                      dbH
                      SetNewsMainPhoto.Request { SetNewsMainPhoto.photo = "photo"
                                               , SetNewsMainPhoto.photo_type = Nothing
                                               , SetNewsMainPhoto.token = "author2"
                                               , SetNewsMainPhoto.news_id = 1
                                               }
      `shouldThrow` anyException

  describe "Handlers.updateNews" $ do

    it "updates news"
      $              Handlers.updateNews
                       dbH
                       UpdateNews.Request { UpdateNews.text    = "text"
                                          , UpdateNews.cat_id  = 1
                                          , UpdateNews.token   = "author"
                                          , UpdateNews.news_id = 1
                                          , UpdateNews.name    = "name"
                                          }
      `shouldReturn` justOK

    it "throws exception if user not author"
      $             Handlers.updateNews
                      dbH
                      UpdateNews.Request { UpdateNews.text    = "text"
                                         , UpdateNews.cat_id  = 1
                                         , UpdateNews.token   = "notauthor"
                                         , UpdateNews.news_id = 1
                                         , UpdateNews.name    = "name"
                                         }
      `shouldThrow` anyException

    it "throws exception if news not exist"
      $             Handlers.updateNews
                      dbH
                      UpdateNews.Request { UpdateNews.text    = "text"
                                         , UpdateNews.cat_id  = 1
                                         , UpdateNews.token   = "authortoken"
                                         , UpdateNews.news_id = 0
                                         , UpdateNews.name    = "name"
                                         }
      `shouldThrow` anyException

    it "throws exception if not this author's news"
      $             Handlers.updateNews
                      dbH
                      UpdateNews.Request { UpdateNews.text    = "text"
                                         , UpdateNews.cat_id  = 1
                                         , UpdateNews.token   = "author2"
                                         , UpdateNews.news_id = 1
                                         , UpdateNews.name    = "name"
                                         }
      `shouldThrow` anyException

    it "throws exception if category not exist"
      $             Handlers.updateNews
                      dbH
                      UpdateNews.Request { UpdateNews.text    = "text"
                                         , UpdateNews.cat_id  = 0
                                         , UpdateNews.token   = "author"
                                         , UpdateNews.news_id = 1
                                         , UpdateNews.name    = "name"
                                         }
      `shouldThrow` anyException

  describe "Handlers.getNewsComments" $ do
    it
      "returns news comments"
      (              Handlers.getNewsComments
          dbH
          GetNewsComments.Request { GetNewsComments.page    = 1
                                  , GetNewsComments.news_id = 1
                                  }
      `shouldReturn` ([] :: [GetNewsComments.Comment])
      )

    it "throws exception if page<1"
      $             Handlers.getNewsComments
                      dbH
                      GetNewsComments.Request { GetNewsComments.page    = 0
                                              , GetNewsComments.news_id = 1
                                              }
      `shouldThrow` anyException

    it "throws exception if news not published"
      $             Handlers.getNewsComments
                      dbH
                      GetNewsComments.Request { GetNewsComments.page    = 1
                                              , GetNewsComments.news_id = 0
                                              }
      `shouldThrow` anyException

  describe "Handlers.getDrafts" $ do
    it
      "returns authors drafts"
      (              Handlers.getDrafts
          dbH
          GetDrafts.Request { GetDrafts.page = 1, GetDrafts.token = "author" }
      `shouldReturn` ([] :: [GetDrafts.Draft])
      )

    it "throws exception if page<1"
      $             Handlers.getDrafts
                      dbH
                      GetDrafts.Request { GetDrafts.page = 0, GetDrafts.token = "author" }
      `shouldThrow` anyException

    it "throws exception if user not author"
      $             Handlers.getDrafts
                      dbH
                      GetDrafts.Request { GetDrafts.page  = 1
                                        , GetDrafts.token = "notauthor"
                                        }
      `shouldThrow` anyException

  describe "Handlers.getNews" $ do
    it
      "returns news"
      (              Handlers.getNews
          dbH
          GetNews.Request { GetNews.page              = 1
                          , GetNews.created_at        = Nothing
                          , GetNews.created_before    = Nothing
                          , GetNews.created_after     = Nothing
                          , GetNews.author_contains   = Nothing
                          , GetNews.name_contains     = Nothing
                          , GetNews.text_contains     = Nothing
                          , GetNews.anything_contains = Nothing
                          , GetNews.cat_id            = Nothing
                          , GetNews.tags_all          = Nothing
                          , GetNews.tags_any          = Nothing
                          , GetNews.sort_by           = Nothing
                          }
      `shouldReturn` ([] :: [GetNews.News])
      )

    it "throws exception if page<1"
      $             Handlers.getNews
                      dbH
                      GetNews.Request { GetNews.page              = 0
                                      , GetNews.created_at        = Nothing
                                      , GetNews.created_before    = Nothing
                                      , GetNews.created_after     = Nothing
                                      , GetNews.author_contains   = Nothing
                                      , GetNews.name_contains     = Nothing
                                      , GetNews.text_contains     = Nothing
                                      , GetNews.anything_contains = Nothing
                                      , GetNews.cat_id            = Nothing
                                      , GetNews.tags_all          = Nothing
                                      , GetNews.tags_any          = Nothing
                                      , GetNews.sort_by           = Nothing
                                      }
      `shouldThrow` anyException
