let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=7d4f6170c7a98cf8625361fe7ffce6d27d9afd56";
        sha256 = "083s3k6cbd719jij3qy0gcqb4bsaqqibiq00zx29d8wvay28crzm";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
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
