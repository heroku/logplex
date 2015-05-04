# Deprecated API Calls not found schema document

## Channels

### Channel Delete

Delete a specific channel

```
DELETE /channels/{channel_channel_id}
```

#### Curl Example
```bash
$ curl -n -X DELETE https://logplex.heroku.com/channels/$CHANNEL_CHANNEL_ID

```

#### Response Example
```
HTTP/1.1 200 OK
```
```text
OK
```

### Channel Info

Obtain information about a channel

```
GET /channels/{channel_channel_id}/info
```

#### Curl Example
```bash
$ curl -n -X GET https://logplex.heroku.com/channels/$CHANNEL_CHANNEL_ID/info

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

## Drains

### Drain Id Reserve

Reserve a drain id and token

```
POST /channels/{channel_channel_id}/drains/tokens
```

#### Curl Example
```bash
$ curl -v -X POST http://logplex.heroku.com/channels/$CHANNEL_CHANNEL_ID/drains/tokens
```

#### Response Example
```
HTTP/1.1 201 OK
```
```json
{
  "id": 123456,
  "token": "d.97da6842-57a3-4ade-b34d-097f9c83d5bb",
  "msg": "Successfully reserved drain token"
}
```
