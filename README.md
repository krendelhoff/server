# server

## Table of contents
* [Introduction](#Introduction)
* [Setup](#setup)
* [API](#api)

## Introduction
Will Kurt's Lesson 41 application from "Get Programming with Haskell" rewritten as a server app with REST API.

## Setup
At first, you have to build the project, using the stack tool:
```
$ stack build
```
Then initialize the database with script:
```
$ psql -U <user_name> -d <db_name> -a -f init.sql
```
Fix `connSettings` at *Main.hs* to match your Postgres settings.

Now, you can start the server using that command:
```
$ stack exec server-exe
```
## API
All information we got serialized in JSON format.
- GET /users - list all users
- POST /users?username=<username> - add user with that username
- GET /tools - list all tools
- POST /tools?name=<tool_name>&desc=<tool_desc> - add tool with following name and description
- GET /checkedout - list all checkedout entities
- GET /checkedin - list all free tools
- DELETE /checkin/:tool_id - check in tool and update tool entity in tools table
- POST /checkout?user_id=<user_id>&tool_id=<tool_id> - check out tool to user
```
$ curl -X POST 'http://localhost:8080/users?username=testuser'
$ curl -X POST 'http://localhost:8080/checkout?user_id=3&tool_id=1'
$ curl -X GET 'http://localhost:8080/checkedout'
$ curl -X DELETE 'http://localhost:8080/checkin/1'
$ curl -X GET 'http://localhost:8080/tools'
```
