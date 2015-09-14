{-|

Module      : SDL.Raw.Image
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_image@ library. No error-handling is done here. For
more information about specific function behaviour, see the @SDL2_image@
documentation.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Raw.Image
  (
  -- * Loading images
    Free
  , load
  , load_RW
  , Format
  , loadTyped_RW
  , loadCUR_RW
  , loadICO_RW
  , loadBMP_RW
  , loadPNM_RW
  , loadXPM_RW
  , loadXCF_RW
  , loadPCX_RW
  , loadGIF_RW
  , loadJPG_RW
  , loadTIF_RW
  , loadPNG_RW
  , loadTGA_RW
  , loadLBM_RW
  , loadXV_RW
  , loadWEBP_RW

  -- * Testing for formats
  , isCUR
  , isICO
  , isBMP
  , isPNM
  , isXPM
  , isXCF
  , isPCX
  , isGIF
  , isJPG
  , isTIF
  , isPNG
  , isLBM
  , isXV
  , isWEBP

  -- * Other
  , InitFlags
  , pattern IMG_INIT_JPG
  , pattern IMG_INIT_PNG
  , pattern IMG_INIT_TIF
  , pattern IMG_INIT_WEBP
  , init
  , getVersion
  , quit
  ) where

#include "SDL_image.h"

import Control.Monad.IO.Class
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops)

foreign import ccall safe "static IMG_Linked_Version" getVersion' :: IO (Ptr Version)
{-# INLINE getVersion #-}
getVersion :: forall m_a5M0. MonadIO m_a5M0 => m_a5M0 (Ptr Version)
getVersion = liftIO getVersion'

type InitFlags = CInt

pattern IMG_INIT_JPG  = #{const IMG_INIT_JPG}
pattern IMG_INIT_PNG  = #{const IMG_INIT_PNG}
pattern IMG_INIT_TIF  = #{const IMG_INIT_TIF}
pattern IMG_INIT_WEBP = #{const IMG_INIT_WEBP}

foreign import ccall safe "static IMG_Init" init' :: InitFlags -> IO InitFlags
{-# INLINE init #-}
init x_a5Ng = liftIO (init' x_a5Ng)

foreign import ccall safe "static IMG_Quit" quit' :: IO ()
{-# INLINE quit #-}
quit :: forall m_a5YZ. MonadIO m_a5YZ => m_a5YZ ()
quit = liftIO quit'

foreign import ccall safe "static IMG_Load" load' :: CString -> IO (Ptr Surface)
{-# INLINE load #-}
load x_a5ZS = liftIO (load' x_a5ZS)

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

foreign import ccall safe "static IMG_Load_RW" load_RW' :: Ptr RWops -> Free -> IO (Ptr Surface)
{-# INLINE load_RW #-}
load_RW x_a617 x_a618 = liftIO (load_RW' x_a617 x_a618)

-- | A case-insensitive desired format, e.g. @\"jpg\"@ or @\"PNG\"@.
type Format = CString

foreign import ccall safe "static IMG_LoadTyped_RW" loadTyped_RW' :: Ptr RWops -> Free -> Format -> IO (Ptr Surface)
{-# INLINE loadTyped_RW #-}
loadTyped_RW x_a62r x_a62s x_a62t = liftIO (loadTyped_RW' x_a62r x_a62s x_a62t)

foreign import ccall safe "static IMG_LoadCUR_RW" loadCUR_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadCUR_RW #-}
loadCUR_RW x_a63x = liftIO (loadCUR_RW' x_a63x)

foreign import ccall safe "static IMG_LoadICO_RW" loadICO_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadICO_RW #-}
loadICO_RW x_a64z = liftIO (loadICO_RW' x_a64z)

foreign import ccall safe "static IMG_LoadBMP_RW" loadBMP_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadBMP_RW #-}
loadBMP_RW x_a65B = liftIO (loadBMP_RW' x_a65B)

foreign import ccall safe "static IMG_LoadPNM_RW" loadPNM_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadPNM_RW #-}
loadPNM_RW x_a66D = liftIO (loadPNM_RW' x_a66D)

foreign import ccall safe "static IMG_LoadXPM_RW" loadXPM_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadXPM_RW #-}
loadXPM_RW x_a67F = liftIO (loadXPM_RW' x_a67F)

foreign import ccall safe "static IMG_LoadXCF_RW" loadXCF_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadXCF_RW #-}
loadXCF_RW x_a68H = liftIO (loadXCF_RW' x_a68H)

foreign import ccall safe "static IMG_LoadPCX_RW" loadPCX_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadPCX_RW #-}
loadPCX_RW x_a69J = liftIO (loadPCX_RW' x_a69J)

foreign import ccall safe "static IMG_LoadGIF_RW" loadGIF_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadGIF_RW #-}
loadGIF_RW x_a6aL = liftIO (loadGIF_RW' x_a6aL)

foreign import ccall safe "static IMG_LoadJPG_RW" loadJPG_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadJPG_RW #-}
loadJPG_RW x_a6bN = liftIO (loadJPG_RW' x_a6bN)

foreign import ccall safe "static IMG_LoadTIF_RW" loadTIF_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadTIF_RW #-}
loadTIF_RW x_a6cP = liftIO (loadTIF_RW' x_a6cP)

foreign import ccall safe "static IMG_LoadPNG_RW" loadPNG_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadPNG_RW #-}
loadPNG_RW x_a6dR = liftIO (loadPNG_RW' x_a6dR)

foreign import ccall safe "static IMG_LoadTGA_RW" loadTGA_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadTGA_RW #-}
loadTGA_RW x_a6eT = liftIO (loadTGA_RW' x_a6eT)

foreign import ccall safe "static IMG_LoadLBM_RW" loadLBM_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadLBM_RW #-}
loadLBM_RW x_a6fV = liftIO (loadLBM_RW' x_a6fV)

foreign import ccall safe "static IMG_LoadXV_RW" loadXV_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadXV_RW #-}
loadXV_RW x_a6gX = liftIO (loadXV_RW' x_a6gX)

foreign import ccall safe "static IMG_LoadWEBP_RW" loadWEBP_RW' :: Ptr RWops -> IO (Ptr Surface)
{-# INLINE loadWEBP_RW #-}
loadWEBP_RW x_a6hZ = liftIO (loadWEBP_RW' x_a6hZ)

foreign import ccall safe "static IMG_isCUR" isCUR' :: Ptr RWops -> IO CInt
{-# INLINE isCUR #-}
isCUR x_a6iV = liftIO (isCUR' x_a6iV)

foreign import ccall safe "static IMG_isICO" isICO' :: Ptr RWops -> IO CInt
{-# INLINE isICO #-}
isICO x_a6jQ = liftIO (isICO' x_a6jQ)

foreign import ccall safe "static IMG_isBMP" isBMP' :: Ptr RWops -> IO CInt
{-# INLINE isBMP #-}
isBMP x_a6kL = liftIO (isBMP' x_a6kL)

foreign import ccall safe "static IMG_isPNM" isPNM' :: Ptr RWops -> IO CInt
{-# INLINE isPNM #-}
isPNM x_a6lG = liftIO (isPNM' x_a6lG)

foreign import ccall safe "static IMG_isXPM" isXPM' :: Ptr RWops -> IO CInt
{-# INLINE isXPM #-}
isXPM x_a6mB = liftIO (isXPM' x_a6mB)

foreign import ccall safe "static IMG_isXCF" isXCF' :: Ptr RWops -> IO CInt
{-# INLINE isXCF #-}
isXCF x_a6nw = liftIO (isXCF' x_a6nw)

foreign import ccall safe "static IMG_isPCX" isPCX' :: Ptr RWops -> IO CInt
{-# INLINE isPCX #-}
isPCX x_a6or = liftIO (isPCX' x_a6or)

foreign import ccall safe "static IMG_isGIF" isGIF' :: Ptr RWops -> IO CInt
{-# INLINE isGIF #-}
isGIF x_a6pm = liftIO (isGIF' x_a6pm)

foreign import ccall safe "static IMG_isJPG" isJPG' :: Ptr RWops -> IO CInt
{-# INLINE isJPG #-}
isJPG x_a6qh = liftIO (isJPG' x_a6qh)

foreign import ccall safe "static IMG_isTIF" isTIF' :: Ptr RWops -> IO CInt
{-# INLINE isTIF #-}
isTIF x_a6rc = liftIO (isTIF' x_a6rc)

foreign import ccall safe "static IMG_isPNG" isPNG' :: Ptr RWops -> IO CInt
{-# INLINE isPNG #-}
isPNG x_a6s7 = liftIO (isPNG' x_a6s7)

foreign import ccall safe "static IMG_isLBM" isLBM' :: Ptr RWops -> IO CInt
{-# INLINE isLBM #-}
isLBM x_a6t2 = liftIO (isLBM' x_a6t2)

foreign import ccall safe "static IMG_isXV" isXV' :: Ptr RWops -> IO CInt
{-# INLINE isXV #-}
isXV x_a6tX = liftIO (isXV' x_a6tX)

foreign import ccall safe "static IMG_isWEBP" isWEBP' :: Ptr RWops -> IO CInt
{-# INLINE isWEBP #-}
isWEBP x_a6uS = liftIO (isWEBP' x_a6uS)
