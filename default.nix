let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=a00e836ae8b0ebe36ac337fe4c7f845695011aee";
        sha256 = "0ph261rhix775cq8n90gh0cy94m9068cbczsf2wdfa7j1575z8w6";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            xss-sanitize
            p.ihp
            tz
            string-interpolate
            ihp-oauth-google
            ihp-sentry
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
