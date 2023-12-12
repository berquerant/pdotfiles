# Dotfiles for macOS

```shell
make init
make deploy
make git
make emacs
make install
```

## Additional tools

``` shell
make help
```

## Update

``` shell
make update
```

## Ignore specific tools

Use `IVG_SH_IGNORE` like

``` shell
IVG_SH_IGNORE="mpv-settings" make update
```

See [install-via-git.sh](bin/install-via-git.sh).

## Install one by one

``` shell
./install ruby
```

Install libraries.

``` shell
./install r ruby
```

Install language and libraries.

``` shell
./install t ruby
```
