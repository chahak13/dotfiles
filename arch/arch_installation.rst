Arch Linux Setup
================

Post installation steps
------------------------

Installing AUR helper
^^^^^^^^^^^^^^^^^^^^^^

For an AUR helper, one of the community recommendations is `yay <https://github.com/Jguer/yay>`_. It is written in ``go``, is quite fast and has an interface to ``pacman`` too. To install ``yay``, perform the following commans:

.. code-block:: bash

    sudo pacman -S --needed git base-devel
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si

We first install ``git`` and ``base-devel`` as requirements for ``yay`` and then use ``makepkg`` to install it. Once installed, ``yay`` can be used as a direct interface for ``pacman`` without using ``sudo``.

Installing packages
^^^^^^^^^^^^^^^^^^^

After installing an AUR helper, the next steps are to install the required packages for arch. A complete list of my current packages can be found in the file ``package_list.txt`` and can be installed using the command ``yay -S < package_list.txt``.

It contains various basic packages that I use almost everyday. A list of which is as follows. One can follow the list to selectively install the packages too.

Package List
~~~~~~~~~~~~

`BSPWM <https://github.com/baskerville/bspwm>`_
"""""""""""""""""""""""""""""""""""""""""""""""

I don't use a DE since I prefer to install stuff myself. I use BSPWM as
Tiling Windows Manager. It's written in C and it follows the *"Do what you
do good"* policy. It only responds to X events and the messages that it
receives on a dedicated socket.

The ``bspc`` program is used to write message on the socket.

BSPWM doesn't handle any keyboard or pointer inputs and requires a third
party program in order to translate such events to ``bspc`` invocations.
The author of BSPWM has also written a tool for this too - `sxhkd <https://github.com/baskerville/sxhkd>`_


To install bspwm, run

.. code-block:: bash

    yay -S bspwm

`SXHKD <https://github.com/baskerville/sxhkd>`_
"""""""""""""""""""""""""""""""""""""""""""""""

SXHKD is a hotkey X daemon that reacts to input events by executing commands.
It can be configured by defining a series of keybindings and their
associations with input events. The format of the configurations can be
found at the github link in the section title.

Make sure that you install sxhkd along with bspwm since without sxhkd, bspwm
is useless.

To install sxhkd, run

.. code-block:: bash

    yay -S sxhkd

Kitty Terminal Emulator
"""""""""""""""""""""""
Polybar
"""""""
Firefox
"""""""
Pulseaudio
""""""""""
PcManFM
"""""""
Arc-Dark-Theme
""""""""""""""
Pipenv
""""""


ToDo
----

- [ ] Add steps for installing Arch
- [ ] Add list of important packages and their installation guides
- [ ] Create an ``install.sh`` to automate the post-OS installation process.
- [ ] Shift to `paru <https://github.com/morganamilo/paru>`_ to remove dependency of ``go`` just for ``yay``
