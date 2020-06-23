# API

## Roles

- All - no token required
- User
- Author
- Admin

## Endpoints

HTTP method always POST

### Users

API Method | Body (JSON) | Access
---|---|---
user/create | name: string<br>lastname: string<br>login: string<br>password: string<br>photo: base64 encoded photo (optional)<br>photo_type: string (photo file type (extension), optional) | All
user/get | page: int | All
user/delete | login: string<br>token: string | Admin
user/login | login: string<br>password: string | All

### Authors

API Method | Body (JSON) | Access
---|---|---
author/make | login: string<br>description: string<br>token: string | Admin
author/edit | login: string<br>description: string<br>token: string | Admin
author/delete | login: string<br>token: string | Admin
author/get | token: string<br>page: int | Admin

### Categories

API Method | Body (JSON) | Access
---|---|---
category/create | name: string<br>parent: int (optional)<br>token: string | Admin
category/edit | cat_id: int<br>name: string<br>parent: int (optional)<br>token: string | Admin
category/delete | cat_id: int<br>token: string | Admin
category/get | page: int | All

### Tags

API Method | Body (JSON) | Access
---|---|---
tag/create | name: string<br>token: string | Admin
tag/edit | tag_id: int<br>name: string<br>token: string | Admin
tag/delete | tag_id: int<br>token: string | Admin
tag/get | page: int | All

### News & Drafts

API Method | Body (JSON) | Access
---|---|---
news/create | name: string<br>cat_id: int<br>text: string<br>token: string | Author
news/update | news_id: int<br>name: string<br>cat_id: int<br>text: string<br>token: string | Author
news/publish | news_id: int<br>publish: bool<br>token: string | Author
news/setmainphoto | news_id: int<br>photo: base64 encoded photo<br>photo_type: string (photo file type (extension), optional)<br>token: string | Author
news/addphoto | news_id: int<br>photo: base64 encoded photo<br>photo_type: string (photo file type (extension), optional)<br>token: string | Author
news/deletephoto | news_id: int<br>photo_id: int<br>token: string | Author
news/addtag | news_id: int<br>token: string<br>tag_id: int | Author
news/deletetag | news_id: int<br>token: string<br>tag_id: int | Author
news/addcomment | news_id: int<br>text: string<br>token: string | User
news/deletecomment | comment_id: int<br>token: string | Admin
news/getcomments | news_id: int<br>page: int | All
news/delete | news_id: int<br>token: int | Author
news/getdrafts | token: string<br>page: int | Author
news/get | created_at: string (optional)<br>created_before: string (optional)<br>created_after: string (optional)<br>author_contains: string (optional)<br>name_contains: string (optional)<br>text_contains: string (optional)<br>anything_contains:  string (optional)<br>cat_id: int (optional)<br>tags_all: list int (optional)<br>tags_any: list int (optional)<br>sort_by: "author" or "category" or "photos" (optional)<br>page: i | All