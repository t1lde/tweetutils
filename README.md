# tweetutils


## Obtaining API Consumer Keys

The application requires Twitter API consumer keys.  These are **not safe** to share, since the Consumer key acts as an Access key for the owner's account.

Follow the instructions [here](https://developer.twitter.com/en/docs/authentication/oauth-1-0a/api-key-and-secret) on how to obtain API Consumer keys. 

Keep them somewhere as safe as your Twitter password!

## Building with Nix Flakes

The simplest method is with [nix](https://nixos.org/) 

- Install an up-to-date version of nix, with flakes enabled
- *optional*: use [haskell.nix binary cache](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache)
- `nix build .#tweetutils:exe:tweetutils-exe` will build the executable locally, in the `result` directory

## Usage:

All commands require a twitter API consumer keypair, set with the --consumer_private and --consumer_public flags.

```
tweetutils - cli utils with the Twitter API

Usage: tweetutils-exe [--consumer_private CONSUMER_PRIVATE |
                        --consumer_private_file path]
                      [--consumer_public CONSUMER_PUBLIC |
                        --consumer_public_file path]
                      [(-q|--quiet) | (-v|--verbose)] COMMAND

  run twitter API commands

Available options:
  --consumer_private CONSUMER_PRIVATE
                           API Consumer Private Key
  --consumer_private_file path
                           API Consumer Private Key (file)
  --consumer_public CONSUMER_PUBLIC
                           API Consumer Public Key
  --consumer_public_file path
                           API Consumer Public Key (file)
  -q,--quiet               don't print log messages
  -v,--verbose             print more log messages
  -h,--help                Show this help text

Available commands:
  auth                     Acquire access keypair via oauth pin.
  dump-tweets              Dump tweets & interactions to file
```

### Acquiring Access Keys - auth command

The auth command sets up OAuth access, allowing you to log and authenticate API access for a twitter account using a PIN obtained from a web interface. 

``` 
Usage: tweetdelete-exe auth
  Acquire access keypair via oauth pin.

Available options:
  -h,--help                Show this help text
```

### Dumping Threads - dump-tweets command

The dump-tweets command queries for threads from the authorised user and renders them to an output file.

```
Usage: tweetutils-exe dump-tweets [--access_private ACCESS_PRIVATE |
                                    --access_private_file path]
                                  [--access_public ACCESS_PUBLIC |
                                    --access_public_file path] --out_dir path
                                  --format <markdown | html | org>
                                  [--download_media | --no_download_media]

  Dump tweets & interactions to file

Available options:
  --access_private ACCESS_PRIVATE
                           Account Access Private Key
  --access_private_file path
                           Account Access Private Key (file)
  --access_public ACCESS_PUBLIC
                           Account Access Public Key
  --access_public_file path
                           Account Access Public Key (file)
  --out_dir path           output directory
  --format <markdown | html | org>
                           output file render format
  --download_media         download embedded media files
  --no_download_media      don't download embedded media files
  -h,--help                Show this help text

```

### Deleting Tweets - delete command (currently disabled)

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


