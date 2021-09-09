# README

## これは何

私のdotfileです。
NixOSでの利用を前提としています。

## インストール方法

### NixOS

`/etc/nixos/configuration.nix`を編集してflakeを有効にし、gitをインストールします。

```nix /etc/nixos/configuration.nix
{ config, pkgs, lib, ... }:
{
  nix = {
    package = pkgs.nixFlakes;
	extraOptions = ''
	  experimental-features nix-command flakes
    '';
  };

  environment.systemPackages = with pkgs; [ git ];

  ...

}
```

リポジトリをcloneして設定を適用します。
ホスト名はbravoに、ユーザー名はhnakanoなります。

```console
$ git clone https://github.com/hnakano863/configuration.nix.git

$ cd configuration.nix

$ sudo nixos-rebuild switch --flake .#bravo
```

### WSL2

まず、https://github.com/Trundle/NixOS-WSL のリリースページからNixOSのsystem tarballをダウンロードし、WSL2にインストールします。

```console
wsl --import NixOS .\NixOS\ nixos-system-x86_64-linux.tar.gz --version 2

wsl -d NixOS
```

shが起動するのでそこでnixをactivateします

```console
$ /nix/var/nix/profiles/system/activate
```

NixOSのシェルを再起動して、`/etc/nixos/configuration.nix`を編集し、flakeを有効にします。

```nix /etc/nixos/configuration.nix
{ config, pkgs, lib, ... }:
{
  nix = {
    package = pkgs.nixFlakes;
	extraOptions = ''
	  experimental-features nix-command flakes
    '';
  };

  environment.systemPackages = with pkgs; [ git ];

  # WSL上のときだけ必要
  systemd.services.systemd-pstore.enable = false;

  ...

}
```

```console
$ sudo nixos-rebuild switch
```


リポジトリをcloneして設定を適用します。
ホスト名はnixosに、ユーザー名はhnakanoなります。

```console
$ git clone https://github.com/hnakano863/configuration.nix.git

$ cd configuration.nix

$ sudo nixos-rebuild switch --flake .#nixos
```
