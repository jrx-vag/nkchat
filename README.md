
# NkCHAT


## API

All commands must use `class: "chat"`.


### User Commands

Use subclass `user`.

#### create

Use command `create`. Supported fields are

Field|Desc
---|---
`login`|Login to use (email-like)
`name`|Name of the user
`surname`|Surname
`password`|Password

The sever will return the newly created `user_id`.


Example:

```javascript
{
    class: "chat",
    subclass: "user",
    cmd: "create",
    data: {
        login: "user@domain.com",
        name: "Name",
        surname: "Surname",
        password: "1234"
    },
    tid: 1
}

-->

{
    result: "ok",
    data: {
        user_id: "59c03l30ddlekvd93"
    },
    tid: 1
}
```


#### delete

Use command `delete` to remove and user. Must use the field `user_id`.


#### search

Use command `search` to find users. The following fields are available:

Field|Desc
---|---
`fields`|List of fields to include in the response. Use `"_all"` to get _all_ fields
`filter`|Filter to apply to the response (for example, `{field1:"value1", field2:"value2"}`
`size`|Numer of objects to return
`from`|Position to start returning objects (starts at 0)
`sort_by`|List of fields to sort on
`sort_order`|Sort order (`"asc"` or `"desc"`)


### Conversation Commands

Use subclass `conversation`

#### create

Use command `create` to create a new conversation. Supported fields are:

Field|Desc
---|---
`name`|Short name for the conversation
`description`|Description
`user_ids`|initial list of users

The sever will return the newly created `conversation_id`.


#### delete

Use command `delete`, and field `conversation_id`.


#### add_members

Use command `add_members` to add users to a conversation. Must set the `conversation_id` field and the list of users in the field `user_ids`.


#### remove_members

Use command `remove_members` to remove users from a conversation. Must set the `conversation_id` field and the list of users in the field `user_ids`.


#### get_members

Use command `get_members` to get the list of users from a conversation. Must set the field `conversation_id`.


#### search

Use command `search` to find conversations. Use the same fields as for [user search](#search).






### Message Commands

Use subclass `message`

#### create

Use command `create` to create a new message. Supported fields are:

Field|Desc
---|---
`conversation_id`|Id of the conversation
`user_id`|User creating the message
`message`|Text of the message

The sever will return the newly created `message_id`.


#### delete

Use command `delete`, and field `message_id`.


#### update

Use command `update`, and fields `message_id` and `message` to update message's content.


#### search

Use command `search` to find messages. Use the same fields as for [user search](#search).






