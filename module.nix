({ config, lib, pkgs, tweetdelete }:

  with lib;

  let

    cfg = config.services.tweetdelete;

    accountOpts = { name, config, ... }: {
      options = {
        user = mkOption {
          access = mkOption {
            public = mkOption {
              type = types.path;
              default = "${cfg.keysDir}/${name}/access.public";
              description = ''
                Location of user access public key.
              '';
            };
            private = mkOption {
              type = types.path;
              default = "${cfg.keysDir}/${name}/access.private";
              description = ''
                Location of user access public key.
              '';
            };
            timerFrequency = mkOption {
              type = types.path;
              default = "daily";
              description = ''
                Frequency to run deletion command. Accepts systemd OnCalendar values.
              '';
            };
            deleteAll = mkOption {
              type = types.bool;
              default = false;
              description = ''
                Delete all tweets
              '';
            };
            deleteBefore = mkOption {
              type = types.nonEmptyStr;
              default = "1 week";
              description = ''
                Delete all tweets before a duration.
                Accepts strings of form %w weeks %d days %h hours %m minutes.
              '';
            };
            deleteLikes = mkOption {
              types = types.bool;
              default = true;
              description = ''
                Delete likes as well as tweets
              '';
            };

          };
        };
      };
    };

    mkAccountConfig = (name: accountCfg:
      {
        config =
          {
            systemd.services."tweetdelete-${name}" = {
              enable = true;
              description = "tweet delete service for ${name}";
              serviceConfig = {
                ExecStart =
                  (
                    let
                      cmd = "${pkgs.tweetdelete}/bin/tweetdelete-exe";
                      cpriv = "--consumer_private \$(cat ${cfg.consumer.private})";
                      cpub = "--consumer_public \$(cat ${cfg.consumer.public})";
                      apriv = "--access_private \$(cat ${accountCfg.access.private})";
                      apub = "--access_public \$(cat ${accountCfg.access.public})";
                      duration = (
                        if accountCfg.deleteAll then "--delete_all" else "--before_duration \'${delete_before}\'"
                      );
                      likes = (
                        if accountCfg.deleteLikes then "--delete_likes" else "--no_likes"
                      );
                    in
                    "${cmd} ${cpriv} delete --force ${apriv} ${apub} ${duration} ${likes} "
                  );
                User = cfg.user;
              };
            };
            systemd.timers."tweetdelete-${name}" = {
              enable = true;
              description = "timer for tweetdelete-${name}.service";
              timerConfig = {
                OnCalendar = accountCfg.timerFrequency;
                Unit = "tweetdelete-${name}.service";
                Persistent = true;
              };
              wantedBy = [ "default.target" ];
            };
            users.users."${cfg.user}" = {
              isSystemUser = true;
            };

          };

      }

    );

  in


  {
    imports = mapAttrsToList mkAccountConfig cfg.accounts;

    options.services.tweetdelete = {
      enable = mkEnableOption "Tweetdelete utility application";

      user = mkOption {
        type = types.str;
        default = "tweetdelete";
        description = "User account under which this service runs";
      };

      keysDir = mkOption {
        type = types.path;
        default = "/var/lib/tweetdelete";
        description = ''
          The directory in which to look for auth keys by default
        '';
      };

      keys.consumer.private = mkOption {
        type = types.path;
        default = "${cfg.keysDir}/consumer.private";
        description = "Location of API consumer private key";
      };

      keys.consumer.public = mkOption {
        types = types.path;
        default = "${cfg.keysDir}/consumer.public";
      };

      accounts = mkOption {
        type = types.attrsOf (types.subModule accountOpts);
        description = ''
          Accounts to run tweetdelete actions for.
        '';
      };

    };

  })
