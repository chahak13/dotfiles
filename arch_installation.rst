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

ToDo
----

- [ ] Add steps for installing Arch
- [ ] Add list of important packages and their installation guides
- [ ] Create an ``install.sh`` to automate the post-OS installation process.
- [ ] Shift to `paru <https://github.com/morganamilo/paru>`_ to remove dependency of ``go`` just for ``yay``
