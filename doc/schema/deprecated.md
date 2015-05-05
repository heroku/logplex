# Deprecated API Calls not found schema document that are known to still be in use by clients

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
