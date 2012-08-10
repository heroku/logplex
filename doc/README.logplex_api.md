An Guide to the Logplex API
---------------------------


All the API methods:

    +------+-------------+---------------------------------------+---------------------+---------------------+
    |Method|Authorization|Path                                   |Parameters           |Returns              |
    |      |Required     |                                       |                     |                     |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |GET   |yes          |/healthcheck                           |none                 |200 if healthy       |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/load                                  |json list of modules |200 If all modules   |
    |      |             |                                       |                     |can be successfully  |
    |      |             |                                       |                     |loaded               |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/channels                              |json struct tokens ->|201 on channel       |
    |      |             |                                       |[token_name, ...]    |creation + json      |
    |      |             |                                       |                     |description of       |
    |      |             |                                       |                     |channel              |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |GET   |yes          |/v2/channels/$channelId                |none                 |200 + json           |
    |      |             |                                       |                     |description of       |
    |      |             |                                       |                     |channel (v2 desc)    |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |DELETE|yes          |/channels/$channelId                   |none                 |200 on channel       |
    |      |             |                                       |                     |deletion             |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |DELETE|yes          |/v2/channels/$channelId                |none                 |200 on channel       |
    |      |             |                                       |                     |deletion             |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/channels/$channelId/token             |json struct          |201 on token creation|
    |      |             |                                       |of name ->           |+ tokenId            |
    |      |             |                                       |token_name           |                     |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/v2/channels/$channelId/token          |json struct of name  |201 on token creation|
    |      |             |                                       |-> token_name        |+ json description of|
    |      |             |                                       |                     |token                |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/sessions                              |json struct of       |201 + session URL    |
    |      |             |                                       |channel_id ->        |                     |
    |      |             |                                       |$channelId, num ->   |                     |
    |      |             |                                       |$NumberOfLogsToFetch,|                     |
    |      |             |                                       |tail -> bool         |                     |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/v2/sessions                           |json struct of       |201 + json struct:   |
    |      |             |                                       |channel_id ->        |url -> $sessionURL   |
    |      |             |                                       |$channelID, num ->   |                     |
    |      |             |                                       |$NumberOfLogsToFetch,|                     |
    |      |             |                                       |tail -> bool         |                     |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |GET   |yes          |/sessions/$sessionID ($sessionURL)     |none                 |200, logplex_tail    |
    |      |             |                                       |                     |data (non-http)      |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |GET   |yes          |/channels/$channelId/info              |none                 |200, json struct:    |
    |      |             |                                       |                     |channel_id ->        |
    |      |             |                                       |                     |$channelId, tokens ->|
    |      |             |                                       |                     |list(tokens), drains |
    |      |             |                                       |                     |-> list(drains)      |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/channels/$channelId/drains/tokens     |none                 |201, json struct: id |
    |      |             |                                       |                     |-> $drainId, token ->|
    |      |             |                                       |                     |$drainToken, msg ->  |
    |      |             |                                       |                     |$usertext            |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/v2/channels/$channelId/drains/$drainId|json struct: url ->  |201 on creation, 409 |
    |      |             |                                       |$drainDest           |on duplicate drain,  |
    |      |             |                                       |                     |422 on invalid drain |
    |      |             |                                       |                     |url                  |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |DELETE|yes          |/v2/channels/$channelId/drains/$drainId|none                 |200 on delete, 404 on|
    |      |             |                                       |                     |not found            |
    +------+-------------+---------------------------------------+---------------------+---------------------+
    |POST  |yes          |/v2/channels/$channelId/drains         |json struct url ->   |201 on create, 409 on|
    |      |             |                                       |$drainDest           |duplicate, 422 if    |
    |      |             |                                       |                     |invalid              |
    +------+-------------+---------------------------------------+---------------------+---------------------+
