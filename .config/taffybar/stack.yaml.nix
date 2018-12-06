packages:
- .
- location: './taffybar'
  #extra-dep: true
- location:
    git: https://github.com/willdonnelly/dyre
    commit: 11412752b483135e0c151ab90be17b217f837c4b
    #extra-dep: true
extra-deps:
      - X11-xft-0.3.1
      - dbus-1.0.1
      - dbus-hslogger-0.1.0.1
      - gi-dbusmenu-0.4.1
      - gi-dbusmenugtk3-0.4.1
      - gi-gdk-3.0.15
      - gi-gdkpixbuf-2.0.16
      - gi-gdkx11-3.0.2
      - gi-gio-2.0.16
      - gi-gtk-3.0.21
      - gi-gtk-hs-0.3.6.1
      - gi-pango-1.0.16
      - gi-xlib-2.0.2
      - gio-0.13.4.1
      - gtk-sni-tray-0.1.5.0
      - gtk-strut-0.1.2.1
      - gtk-traymanager-1.0.1
      - gtk3-0.14.9
      - haskell-gi-0.21.2
      - haskell-gi-base-0.21.1
      - libxml-sax-0.7.5
      - rate-limit-1.1.1
      - spool-0.1
      - status-notifier-item-0.3.0.0
      - time-units-1.0.0
      - xml-helpers-1.0.0
      - process-1.6.3.0
      - bytestring-0.10.8.2
      - download-curl-0.1.4
      - gi-glib-2.0.17
resolver: lts-11.2
nix:
  enable: true
  pure: false
  packages:
        - gcc
        - libxml2
        - pkgconfig
        - upower
        - x11
        - xorg.libX11
        - xorg.libXext
        - xorg.libXinerama
        - xorg.libXrandr
        - xorg.libXrender
        - xorg.libXft
        - curl
        - gobjectIntrospection
        - cairo
        - pango
        - gtk3
        - libdbusmenu-glib
        - libdbusmenu-gtk3
        - zlib
        - haskellPackages.gi-atk
        - haskellPackages.gi-cairo
        - haskellPackages.gi-dbusmenu
        - haskellPackages.gi-dbusmenugtk3
        - haskellPackages.gi-gdk
        - haskellPackages.gi-gdkpixbuf
        - haskellPackages.gi-gio
        - haskellPackages.gi-glib
        - haskellPackages.gi-gobject
        - haskellPackages.gi-gtk
        - haskellPackages.gi-gtk-hs
        - haskellPackages.gi-pango
        - haskellPackages.gi-xlib
        - haskellPackages.gtk-sni-tray
        - haskellPackages.gtk-strut
        - haskellPackages.haskell-gi