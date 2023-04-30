# Shipnix recommended settings
# IMPORTANT: These settings are here for ship-nix to function properly on your server
# Modify with care

{ config, pkgs, modulesPath, lib, ... }:
{
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
    settings = {
      trusted-users = [ "root" "ship" "nix-ssh" ];
    };
  };

  programs.git.enable = true;
  programs.git.config = {
    advice.detachedHead = false;
  };

  services.openssh = {
    enable = true;
    # ship-nix uses SSH keys to gain access to the server
    # Manage permitted public keys in the `authorized_keys` file
    passwordAuthentication = false;
    #  permitRootLogin = "no";
  };


  users.users.ship = {
    isNormalUser = true;
    extraGroups = [ "wheel" "nginx" ];
    # If you don't want public keys to live in the repo, you can remove the line below
    # ~/.ssh will be used instead and will not be checked into version control. 
    # Note that this requires you to manage SSH keys manually via SSH,
    # and your will need to manage authorized keys for root and ship user separately
    openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDBkCCgisu0iVNM3LVxhSKMf9XrNMIjX4563Wg4u0C3qC/jwRMpfapXEZ1o1zZ5D3MPzDVUG/DfOQFXpdfOTy9QhywXJjj4hlu3al4p01T4IEX3aZIdusqZYmKVPqnmNT0o2JrVmby6Ufr3ZJCR4Dy4k1gf71CXc7cpiIik/NP+thcpmhuRQ08A9sazpgJ4dcvyBni7j0vrp6IlGd1GW0FLXM63BbODKFk06QegL/11Q5JxGRnYPmF5WPE1eHRpxgGHYNByzSHqhhh9uHaqyaNfKavkwoThzOC5fhaGFsk1TqH9t89BIwDboYlr2ZX5ltipF5wGGBtEHmNL4BLhCCeKnUnmB3oCTVcfvPNbQIpvtGf8JRDjPZDnpgwPkIFm3aDVJW3pSd7xmc6seqblC0fXn2nNMEvvXlfH7L4eQlZFDGAAamqXwkNtE90Fb7YkdGdguAWUqsa6QYMln8ojZNAQ7q2rXTVjTxu7vVEPn4LCay6FYTIGRWHE6BneQLHZ3/M= ship@tite-ship
"
    ];
  };

  # Can be removed if you want authorized keys to only live on server, not in repository
  # Se note above for users.users.ship.openssh.authorizedKeys.keyFiles
  users.users.root.openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDBkCCgisu0iVNM3LVxhSKMf9XrNMIjX4563Wg4u0C3qC/jwRMpfapXEZ1o1zZ5D3MPzDVUG/DfOQFXpdfOTy9QhywXJjj4hlu3al4p01T4IEX3aZIdusqZYmKVPqnmNT0o2JrVmby6Ufr3ZJCR4Dy4k1gf71CXc7cpiIik/NP+thcpmhuRQ08A9sazpgJ4dcvyBni7j0vrp6IlGd1GW0FLXM63BbODKFk06QegL/11Q5JxGRnYPmF5WPE1eHRpxgGHYNByzSHqhhh9uHaqyaNfKavkwoThzOC5fhaGFsk1TqH9t89BIwDboYlr2ZX5ltipF5wGGBtEHmNL4BLhCCeKnUnmB3oCTVcfvPNbQIpvtGf8JRDjPZDnpgwPkIFm3aDVJW3pSd7xmc6seqblC0fXn2nNMEvvXlfH7L4eQlZFDGAAamqXwkNtE90Fb7YkdGdguAWUqsa6QYMln8ojZNAQ7q2rXTVjTxu7vVEPn4LCay6FYTIGRWHE6BneQLHZ3/M= ship@tite-ship
"
  ];

  security.sudo.extraRules = [
    {
      users = [ "ship" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" "SETENV" ];
        }
      ];
    }
  ];
}
