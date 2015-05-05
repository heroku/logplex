# Deprecated API Calls not found schema document that are known to still be in use by clients

## Channels

### Delete Channel

Deletes a channel

```
DELETE /channels/{channel_id}
```

#### Curl Example

```bash
$ curl -v -X DELETE https://logplex.heroku.com/channels/$CHANNEL_CHANNEL_ID
```

## Drains

### Drain Id Reserve

Reserve a drain id and token

```
POST /channels/{channel_channel_id}/drains/tokens
```

#### Curl Example
```bash
$ curl -v -X POST https://logplex.heroku.com/channels/$CHANNEL_CHANNEL_ID/drains/tokens
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

## Sessions

### Create Session

Create a sesssion for a given log channel

```
POST /sessions
```

#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/sessions \
 -H "Content-Type: application/json" \
 \
 -d '{
  name: 123456,
  channel_id: 123456,
}'
```

#### Response Example
```
/sessions/b15ca55d-4bc6-4d78-9061-20a0a18d400b
```

## Tokens

### Create Token

Create a token for a log channel

```
POST /channels/{channel_id}/tokens
```

#### Curl Example
```bash
$ curl -n -X POST https://logplex.heroku.com/channels/1234/tokens \
 -H "Content-Type: application/json" \
 \
 -d '{
  name: "my new token"
}'
```
