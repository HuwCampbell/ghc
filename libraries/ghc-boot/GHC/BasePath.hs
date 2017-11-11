{-# LANGUAGE CPP #-}
module GHC.BasePath (getBaseDir) where

#if defined(mingw32_HOST_OS)
#if MIN_VERSION_Win32(2,5,0)
import qualified System.Win32.Types as Win32
#else
import qualified System.Win32.Info as Win32
#endif
import Data.Char
import Exception
import Foreign
import Foreign.C.String
import System.Win32.Types (DWORD, LPTSTR, HANDLE)
import System.Win32.Types (failIfNull, failIf, iNVALID_HANDLE_VALUE)
import System.Win32.File (createFile,closeHandle, gENERIC_READ, fILE_SHARE_READ, oPEN_EXISTING, fILE_ATTRIBUTE_NORMAL, fILE_FLAG_BACKUP_SEMANTICS )
import System.Win32.DLL (loadLibrary, getProcAddress)
#endif

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

getBaseDir :: [String] -> IO (Maybe String)
#if defined(mingw32_HOST_OS)
-- Assuming we are running ghc, accessed by path  $(stuff)/<foo>/ghc.exe,
-- return the path $(stuff)/lib.
getBaseDir exeNames = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> do
                path <- peekCWString buf
                real <- getFinalPath path -- try to resolve symlinks paths
                let libdir = (rootDir . sanitize . maybe path id) real
                exists <- doesDirectoryExist libdir
                if exists
                   then return $ Just libdir
                   else fail path
            | otherwise  -> try_size (size * 2)

    -- getFinalPath returns paths in full raw form.
    -- Unfortunately GHC isn't set up to handle these
    -- So if the call succeeded, we need to drop the
    -- \\?\ prefix.
    sanitize s = if "\\\\?\\" `isPrefixOf` s
                    then drop 4 s
                    else s

    rootDir s = case splitFileName $ normalise s of
                (d, ghc_exe)
                 | lower ghc_exe `elem` (map lower exeNames) ->
                    case splitFileName $ takeDirectory d of
                    -- ghc is in $topdir/bin/ghc.exe
                    (d', _) -> takeDirectory d' </> "lib"
                _ -> fail s

    fail s = panic ("can't decompose ghc.exe path: " ++ show s)
    lower = map toLower

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32

-- Attempt to resolve symlinks in order to find the actual location GHC
-- is located at. See Trac #11759.
getFinalPath :: FilePath -> IO (Maybe FilePath)
getFinalPath name = do
    dllHwnd <- failIfNull "LoadLibrary"     $ loadLibrary "kernel32.dll"
    -- Note: The API GetFinalPathNameByHandleW is only available starting from Windows Vista.
    -- This means that we can't bind directly to it since it may be missing.
    -- Instead try to find it's address at runtime and if we don't succeed consider the
    -- function failed.
    addr_m  <- (fmap Just $ failIfNull "getProcAddress" $ getProcAddress dllHwnd "GetFinalPathNameByHandleW")
                  `catch` (\(_ :: SomeException) -> return Nothing)
    case addr_m of
      Nothing   -> return Nothing
      Just addr -> do handle  <- failIf (==iNVALID_HANDLE_VALUE) "CreateFile"
                                        $ createFile name
                                                     gENERIC_READ
                                                     fILE_SHARE_READ
                                                     Nothing
                                                     oPEN_EXISTING
                                                     (fILE_ATTRIBUTE_NORMAL .|. fILE_FLAG_BACKUP_SEMANTICS)
                                                     Nothing
                      let fnPtr = makeGetFinalPathNameByHandle $ castPtrToFunPtr addr
                      -- First try to resolve the path to get the actual path
                      -- of any symlinks or other file system redirections that
                      -- may be in place. However this function can fail, and in
                      -- the event it does fail, we need to try using the
                      -- original path and see if we can decompose that.
                      -- If the call fails Win32.try will raise an exception
                      -- that needs to be caught. See #14159
                      path    <- (Win32.try "GetFinalPathName"
                                    (\buf len -> fnPtr handle buf len 0) 512
                                    `finally` closeHandle handle)
                                `catch`
                                 (\(_ :: IOException) -> return name)
                      return $ Just path

type GetFinalPath = HANDLE -> LPTSTR -> DWORD -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "dynamic"
  makeGetFinalPathNameByHandle :: FunPtr GetFinalPath -> GetFinalPath
#elif defined(darwin_HOST_OS) || defined(linux_HOST_OS)
-- on linux and macOS: for relocatable ghc builds we will assume the following:
--
-- $topdir/bin/ghc <- the ghc binary (getExecutablePath with yield this)
-- $topdir/lib     <- the baseDirectory, where the package database
--                    and libraries reside.
--
getBaseDir _ = Just . (\p -> p </> "lib") . takeDirectory . takeDirectory <$> getExecutablePath
#else
getBaseDir _ = return Nothing
#endif
