This fork/branch is a hack to allow sdl2-image to build and install on Windows. There's a [bug involving GHC, Windows, and Template Haskell](https://github.com/haskell-game/sdl2/issues/41#issuecomment-73564593) so the TH code has been removed and replaced with its own output. The generated code is dumped from GHC's `-ddump-splices -ddump-to-file` flags.

To install:

```bash
set PKG_CONFIG_PATH=%PATH_TO_SDL2_IMAGE_MINGW_DEVEL%\lib\pkgconfig;%PATH_TO_SDL2_MINGW_DEVEL%\lib\pkgconfig
set PATH=%PATH_TO_SDL2_IMAGE_MINGW_DEVEL%\bin;%PATH_TO_SDL2_MINGW_DEVEL\bin;%PATH%
cabal install --extra-lib-dirs=%PATH_TO_SDL2_IMAGE_MINGW_DEVEL%\lib --extra-include-dirs=%PATH_TO_SDL2_IMAGE_MINGW_DEVEL%\include\SDL2
```


# sdl2-image

[![Build Status](https://travis-ci.org/sbidin/sdl2-image.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-image)

#### Haskell bindings to SDL2_image

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_image library. Please report an issue if you encounter a
bug or feel that something is missing.

##### Install

This library depends on the new API version of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2), available on
Hackage as
[sdl2 version 2.0.0 or greater](http://hackage.haskell.org/package/sdl2). With
that in mind, you can install sdl2-image from source like this:

```bash
git clone git@github.com:sbidin/sdl2-image.git
cd sdl2-image
cabal install
```

Note that you might get compile errors if you're not using the latest GHC. Only
7.10 is currently tested.

##### Documentation

You can find the documentation [here](https://bidin.eu/docs/sdl2-image).

The
[original SDL2_image documentation](http://www.libsdl.org/projects/SDL_image/docs/SDL_image.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It loads and displays
a given image. You can find it in the `example` directory.

```bash
cd sdl2-image
cabal run path/to/some/image.type
```
