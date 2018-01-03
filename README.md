```
███╗   ███╗██╗██╗  ██╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██║██║ ██╔╝██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║█████╔╝ █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║██╔═██╗ ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║██║██║  ██╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝╚═╝╚═╝  ╚═╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════
```

- - -

MIKEMACS
=====

Mike's Emacs. This configuration is heavily influenced by [gilbertw1](https://github.com/gilbertw1),
and is written as a literate org file. It has been optimized to be run graphically on Arch Linux
using Emacs 26+.

Like [the original](https://github.com/gilbertw1/bmacs), it is heavily influenced by and would not
exist without the fantastic [DOOM Emacs configuration](https://github.com/hlissner/doom-emacs).


### Usage

Install

    git clone git@github.com:michaelkhaney/mikemacs.git ~/.emacs.d
    cd ~/.emacs.d

Compile

    make compile

Run

    emacs

### Make Commands

**clean**: Delete compiled files

    make clean

**compile**: Byte compile for performance (Recompile required when new changes are made)

    make compile

**update**: Update Packages

    make update

### Notes

Be sure to change `Configuration -> Variables -> Font` if you don't have Iosevka installed.
