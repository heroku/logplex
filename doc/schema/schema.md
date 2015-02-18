## Channel
A channel is a log stream.

### Attributes
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **drains** | *array* | drains under the channel | `[{"id"=>123456, "token"=>"d.01234567-89ab-cdef-0123-456789abcdef", "url"=>"https://example.org"}]` |
| **channel_id** | *integer* | unique identifier of channel | `123456` |
| **tokens** | *array* | tokens under the channel | `[{"token"=>"t.01234567-89ab-cdef-0123-456789abcdef", "name"=>"my-token"}]` |
### Channel Create
Create a new channel.

```
POST /channels
```

#### Required Parameters
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **name** | *string* | a name for the channel | `"my-channel"` |


#### Optional Parameters
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **tokens** | *array* | tokens to create | `[{"name":"my-token"}]` |


#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/channels \
  -H "Content-Type: application/json" \
 \
  -d '{
  "name": "my-channel",
  "tokens": [
    {
      "name": "my-token"
    }
  ]
}'

```


#### Response Example
```
HTTP/1.1 201 Created
```
```json
{
  "drains": [
    {
      "id": 123456,
      "token": "d.01234567-89ab-cdef-0123-456789abcdef",
      "url": "https://example.org"
    }
  ],
  "channel_id": 123456,
  "tokens": [
    {
      "token": "t.01234567-89ab-cdef-0123-456789abcdef",
      "name": "my-token"
    }
  ]
}
```

### Channel Delete
Delete an existing channel.

```
DELETE /v2/channels/{channel_channel_id}
```


#### Curl Example
```bash
$ curl -n -X DELETE https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID \
  -H "Content-Type: application/json" \

```


#### Response Example
```
HTTP/1.1 200 OK
```
```json
{
  "drains": [
    {
      "id": 123456,
      "token": "d.01234567-89ab-cdef-0123-456789abcdef",
      "url": "https://example.org"
    }
  ],
  "channel_id": 123456,
  "tokens": [
    {
      "token": "t.01234567-89ab-cdef-0123-456789abcdef",
      "name": "my-token"
    }
  ]
}
```

### Channel Info
Info for existing channel.

```
GET /v2/channels/{channel_channel_id}
```


#### Curl Example
```bash
$ curl -n -X GET https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID

```


#### Response Example
```
HTTP/1.1 200 OK
```
```json
{
  "drains": [
    {
      "id": 123456,
      "token": "d.01234567-89ab-cdef-0123-456789abcdef",
      "url": "https://example.org"
    }
  ],
  "channel_id": 123456,
  "tokens": [
    {
      "token": "t.01234567-89ab-cdef-0123-456789abcdef",
      "name": "my-token"
    }
  ]
}
```


## Drain
Drains are log stream tee targets.

### Attributes
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **id** | *integer* | unique identifier of drain | `123456` |
| **token** | *string* | drain token | `"d.01234567-89ab-cdef-0123-456789abcdef"` |
| **url** | *string* | drain destination | `"https://example.org"` |
### Drain Create
Create a new drain.

```
POST /v2/channels/{channel_channel_id}/drains
```

#### Required Parameters
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **url** | *string* | drain destination | `"https://example.org"` |



#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID/drains \
  -H "Content-Type: application/json" \
 \
  -d '{
  "url": "https://example.org"
}'

```


#### Response Example
```
HTTP/1.1 201 Created
```
```json
{
  "id": 123456,
  "token": "d.01234567-89ab-cdef-0123-456789abcdef",
  "url": "https://example.org"
}
```

### Drain Delete
Delete an existing drain.

```
DELETE /v2/channels/{channel_channel_id}/drains/{drain_id}
```


#### Curl Example
```bash
$ curl -n -X DELETE https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID/drains/$DRAIN_ID \
  -H "Content-Type: application/json" \

```


#### Response Example
```
HTTP/1.1 200 OK
```
```json
{
  "id": 123456,
  "token": "d.01234567-89ab-cdef-0123-456789abcdef",
  "url": "https://example.org"
}
```

### Drain Update
Update an existing drain.

```
POST /v2/channels/{channel_channel_id}/drains/{drain_id}
```

#### Required Parameters
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **url** | *string* | drain destination | `"https://example.org"` |



#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID/drains/$DRAIN_ID \
  -H "Content-Type: application/json" \
 \
  -d '{
  "url": "https://example.org"
}'

```


#### Response Example
```
HTTP/1.1 200 OK
```
```json
{
  "id": 123456,
  "token": "d.01234567-89ab-cdef-0123-456789abcdef",
  "url": "https://example.org"
}
```


## Token
Tokens are log producers.

### Attributes
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **token** | *string* | unique identifier of token | `"t.01234567-89ab-cdef-0123-456789abcdef"` |
| **name** | *string* | name of token | `"my-token"` |
### Token Create
Create a new token.

```
POST /v2/channels/{channel_channel_id}/tokens
```

#### Required Parameters
| Name | Type | Description | Example |
| ------- | ------- | ------- | ------- |
| **name** | *string* | name of token | `"my-token"` |



#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/v2/channels/$CHANNEL_CHANNEL_ID/tokens \
  -H "Content-Type: application/json" \
 \
  -d '{
  "name": "my-token"
}'

```


#### Response Example
```
HTTP/1.1 201 Created
```
```json
{
  "token": "t.01234567-89ab-cdef-0123-456789abcdef",
  "name": "my-token"
}
```


