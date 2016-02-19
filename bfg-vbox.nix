{
  webserver =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox =
        { headless = true;
          memorySize = 2048;
        };
    };
}
