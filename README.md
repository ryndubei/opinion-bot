opinion-bot
===========

A Discord bot that predicts a user's opinion on something by analysing past messages.

Build
-----
Debian-based Linux distributions:
```
sudo apt install haskell-stack
cd opinion-bot
stack setup
stack build
```

Usage
-----
First register a bot with the message content privileged intent. Add it to your server with `33280` permissions.

Create `.secrets/auth-token.secret` and `.secrets/guildid.secret` in `opinion-bot`, containing your bot authentication token and your server's ID, respectively. Run the bot with `stack run`.

The bot takes three slash commands:

- `ping` : test command, returns pong
- `import` : import old messages from the channel the command was called in
- `analyse` [Keyword] [Optional Channel] [Optional User] : calculate the average sentiment of messages containing a certain keyword, and optionally posted in a certain channel or by a certain user

Acknowledgements
----------------

Positive and negative wordlists:

Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, Washington, USA, 

License
-------
AGPL-3.0-or-later
