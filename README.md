# tweetdelete_hs

Haskell version of [tweet delete cli](https://gitlab.com/t1lde/tweet_delete_cli).
Delete tweets from the commandline, using the twitter API (requires a twitter API account).

## Usage:

All commands require a twitter API consumer keypair, set with the --consumer_private and --consumer_public flags.

```
tweetdeletecli - automatically delete old tweets

Usage: tweetdelete-exe --consumer_private CONSUMER_PRIVATE
                       --consumer_public CONSUMER_PUBLIC COMMAND

Available options:
  --consumer_private CONSUMER_PRIVATE
                           API Consumer Private Key
  --consumer_public CONSUMER_PUBLIC
                           API Consumer Public Key
  -h,--help                Show this help text

Available commands:
  auth                     Acquire access keypair via oauth pin.
  delete                   Delete tweets

```

### Acquiring Access Keys - auth command

The auth command sets up OAuth access, allowing you to log and authenticate API access for a twitter account using a PIN obtained from a web interface. 

``` 
Usage: tweetdelete-exe auth
  Acquire access keypair via oauth pin.

Available options:
  -h,--help                Show this help text
```

### Deleting Tweets - delete command

The delete command deletes your tweets based on a query. A Twitter API access keypair is required for this command, set with the --access_private and --access_public flags.

The --before_duration, --before_data and --delete_all flags allow you to specify a query, selecting which tweets to delete.

The --force, --dry and --prompt flags set the prompt mode, allowing tweets to be deleted with/without prompt or not at all.

By default, your likes on tweets older than the query will also be deleted, this can be changed with the --no_likes flag.
**Note:** Currently, the likes will be queried based on the *liked tweet's* post time, rather than the time that the authenticated used liked the tweet. As far as I know, the twitter API doesn't provide information about *when* a tweet was liked...

``` 
Usage: tweetdelete-exe delete --access_private ACCESS_PRIVATE
                              --access_public ACCESS_PUBLIC
                              (--delete_all | --before_duration DURATION |
                                --before_date DATETIME [-f|--time_format FMT])
                              [(-d|--dry) | (-f|--force) | (-p|--prompt)]
                              [--no_likes | --delete_likes]
  Delete tweets

Available options:
  --access_private ACCESS_PRIVATE
                           Account Access API Private Key
  --access_public ACCESS_PUBLIC
                           Account Access API Public Key
  --delete_all             Delete All Tweets
  --before_duration DURATION
                           Delete all tweets older than DURATION in the form "%b
                           months %w weeks %d days %h hours %m minutes"
  --before_date DATETIME   Delete all tweets posted before DATETIME, formatted
                           according to FMT, or %Y/%m/%d %H:%M:%S by default.
  -f,--time_format FMT     Parse input date according to FMT.
  -d,--dry                 Run without deleting, displaying tweets that would be
                           deleted
  -f,--force               Delete tweets without prompting for confirmation
  -p,--prompt              Prompt for confirmation before deleting tweets
                           (default, this flag can be ommitted)
  --no_likes               Don't delete likes
  --delete_likes           Delete likes as well as tweets (default, this flag
                           can be ommitted)
  -h,--help                Show this help text
```


