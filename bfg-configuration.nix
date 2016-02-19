let
  bfg = import ./default.nix {};
in
{
  network.description = "bfg";
  network.enableRollback = true;

  webserver =
    { config, pkgs, ... }:
    { services.sshd =
        { enable = true;
          passwordAuthentication = false;
        };

      networking.firewall.allowedTCPPorts = [ 22 80 ];

      security.sudo =
        { enable = true;
          wheelNeedsPassword = false;
        };

      users.extraUsers.bfg =
        { isNormalUser = true;
          extraGroups = [ "wheel" ];
          openssh.authorizedKeys.keys =
            [ "ssh-rsa
            AAAAB3NzaC1yc2EAAAADAQABAAABAQCkFQ1XlTlAEj+TqMH0rgJyRqR8ADumMAF2XOUKQnP84QT3YRrJGNKhvzfNSSeePVTePXxTfyKjmeGF+RgeEhl5u76Wd/L58iZWjNAokA2qYdN1gPeR5TIg7LWZPhQeVRExMfvR/9bgj43f6xmdO1o+IPl2vNSP9x4Uxzs4c0w6Nwjp/DPvD87xF6tk9koPPXEdVaZsOl0z9USAmaxtpUFQl4QvxWP+y6pcunIKF2wcCYKvI1Y7YPuj4pIHVHYXuwwjKxJbknz1nZzXpqY3oiKB9A7SaFAf3wwsHXkln8fyX1ECEJ69vKeyQ5gBBrHkZth6VD0bmcTlzfgyJ8w4V+gJ sebas007estrella@gmail.com" ];
        };

      environment.systemPackages = [ bfg ];

      systemd.services.bfg =
        { description = "bfg";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${bfg}/bin/bfg-api";
            };
        };

      services.nginx =
        { enable = true;
          httpConfig =
            ''
              server {
                listen 80;

                location / {
                  proxy_pass http://127.0.0.1:8081;
                }
              }
            '';
        };
    };
}
