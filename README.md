RSSTwit: Commandline utility to send entries from RSS feeds to Twitter
======================================================================

RSSTwit is a sort of inverse to [TwitRSS.me](https://github.com/ciderpunx/twitrssme). It tweets entries from RSS feeds on your behalf so you can enjoy your life and come back to a fascinating autocurated twitter feed without having to actually tweet stuff yourself.

It came about in response to twitterfeed announcing that they would be shutting down at the end of October 2016.

RSSTwit is intended to be used on Linux. It may work on FreeBSD, Macs and such. May even work on Windows, but it has not been tested on those other platforms.

RSSTwit is written in Haskell, which I am learning at present. Please excuse any horrific newbie mistakes and even better tell me where I messed up!

RSSTwit is a prototype and may well be broken in all sorts of horrific ways. At present it doesn't even have tests.

Installing
----------

You will need some credentials from Twitter for the app.

You set up a new app at [https://apps.twitter.com/](https://apps.twitter.com/). Then get write access for it and make yourself an oauth_token and oauth_token secret.

You will need to create a file called src/TwitterCredsPrivate.hs containing this code -- proper config file coming soon, I promise. 

  import Web.Twitter.Conduit

  tokens :: OAuth
  tokens = twitterOAuth
      { oauthConsumerKey    = "YOUR_CONSUMER_KEY"
      , oauthConsumerSecret = "YOUR_CONSUMER_SECRET"
      }

  credential :: Credential
  credential = Credential
      [ ("oauth_token",        "YOUR_OAUTH_TOKEN")
      , ("oauth_token_secret", "YOUR_OAUTH_SECRET")
      ]

  twInfo = setCredential tokens credential def

By default rsstwit stores its database at /tmp/rsstwit.sqlite3. You can change that in src/Db.hs to somewhere more sensible.

Next, you will want a working install of [stack](https://docs.haskellstack.org/en/stable/README/). Probably a libsqlite3 too. I should test that.

Once you have stack up and running you can clone this repo:

    [TODO]

Then do:

    stack build

And the executable will be in:

    ./.stack-work/install/x86_64-linux/lts-7.4/8.0.1/bin/rsstwit

You may want to move it somewhere more convenient.

Running
-------

You can use rsstwit from the commandline to add, remove and list feeds. In addition it has a cron mode, so it can be run as a cronjob and take care of posting your tweets at regular intervals.

In the following I assume you have moved the rsstwit binary to /usr/local/bin or somewhere on your path.

### Initialize your database

    rsstwit init

### Set up the cron job

I like to use:

    crontab -e

Add a line running the cron command every so often:

    */20 * * * * /usr/local/bin/rsstwit cron

### Add a new feed

    rsstwit add

And you will be asked the following before your feed is added to your database:

    Feed URL: http://charlieharvey.com/page/feed/rss

URL of feed -- should be RSS, atom will probably work but is less tested
BEWARE you can enter invalid URLs here

    Feed name: Charlie's blog

A human-readable name for the feed

    Prepend to tweets (up to 10 chars): #before

This text is prepended to tweets from this feed and is optional

    Append to tweets (up to 20 chars - length of prepend): via @someone

This text is appended to tweets from this feed and is also optional

    Tweets per run (1-5): 1

Each time the cron runs send this many tweets from this feed

    Check every n minutes (5 and above): 60

Update the feed at intervals of about this length (depends on when your cron job is runnign too)

    OK? (Y for yes anything else for no): Y
    Feed created, will start tweeting when the next cron job runs.

### List your feeds

    rsstwit list
    1: Charlie's blog (http://charlieharvey.org.uk/page/feed/rss) Next check: 2016-10-23 21:33:54.683585 UTC

### Delete a feed

    rsstwit delete 1

### Advanced noodling

The database is just a sqlite database so you can mess around with things if rsstwit is not working as expected.


Why make this?
--------------

Two reasons really.

1. Twitterfeed recently announced they were shutting down on 31 Oct ([25 Dec](https://news.ycombinator.com/item?id=9117195)?!) 
2. I wanted a Haskell project to get my teeth into.

Alternatives
------------

RSSTwit is very much a prototype. If you need to do this reliably, here are some alternatives that people on Twitter [suggested](https://twitter.com/ciderpunx/status/789783130513301504).

* [Buffer](https://buffer.com) - paid for RSS
* [dlvr.it](https://dlvr.it) - paid for more than 5 feeds
* [IFTT](https://iftt.com) - not tried, but have seen others use it
* [twibble.io](https://twibble.io)
* [Zapier](https://zapier.com)
* [RSSToTwitterPy3](https://github.com/engdeathmatch/RSSToTwitterPy3)
