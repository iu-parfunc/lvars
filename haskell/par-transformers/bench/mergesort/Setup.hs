
-- Code lifted from:
--   http://stackoverflow.com/questions/2528887/c-compiler-selection-in-cabal-package
-- And then modified.

import Data.Maybe
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.Setup (BuildFlags)
import System.FilePath
import System.Process

-- TEMP:
-- import Text.Show.Pretty

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { buildHook = myBuildHook }

myBuildHook :: PackageDescription
     -> LocalBuildInfo
     -> UserHooks
     -> BuildFlags
     -> IO ()
myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
    do
--    print $ ppShow pkg_descr
    print $ pkg_descr
--    print $ local_bld_info
    let

--        lib       = fromJust (library pkg_descr)
        [exe]     = executables pkg_descr
        exe_bi = buildInfo exe
--        lib_bi    = libBuildInfo lib
--        custom_bi = customFieldsBI lib_bi
        custom_bi = customFieldsBI exe_bi        
        cpp_name  = case (lookup "x-cc-name" custom_bi) of
                      Nothing -> error "REQUIRING x-cc-name custom cabal field"
                      Just x  -> x
--        c_srcs    = (lines . fromJust) (cSources lib_bi)
        c_srcs    = cSources exe_bi
        -- cc_opts   = ccOptions lib_bi
        -- inc_dirs  = includeDirs lib_bi
        -- lib_dirs  = extraLibDirs lib_bi

        cc_opts   = ccOptions exe_bi
        inc_dirs  = includeDirs exe_bi
        lib_dirs  = extraLibDirs exe_bi
        bld_dir   = buildDir local_bld_info

    -- Compile C/C++ sources
    putStrLn " [Setup.hs] Invoking my custom compile phase."
--    print lib_bi
    objs <- mapM (compileCxx cpp_name cc_opts inc_dirs bld_dir (exeName exe)) c_srcs
    -- Remove C/C++ source code from the hooked build (don't change libs)
    let -- lib_bi'    = lib_bi { cSources = [] }
        exe_bi'    = exe_bi -- { cSources = [] }
--        lib'       = exe    { libBuildInfo = exe_bi' }
        exe'       = exe    { buildInfo = exe_bi' }
        pkg_descr' = pkg_descr { executables = [exe'] }
    -- The following line invokes the standard build behaviour
    putStrLn " [Setup.hs] Invoke default build hook"
    bh <- buildHook simpleUserHooks pkg_descr' local_bld_info user_hooks bld_flags
    return bh

compileCxx :: String -> [String] -> [FilePath] -> FilePath -> FilePath -> String -> IO ()
compileCxx cmd args unknown targDir cFile exeName = do
  putStrLn$ "FINISH ME :"++show (cmd,args,unknown,targDir,cFile,exeName)
  
  -- [2013.11.13] HACK: I don't know the proper place to learn the "-tmp" part from:
  let tmp = exeName ++ "-tmp"
      dir = targDir </> exeName </> tmp
  putStrLn$ " [Setup.hs] This part is extremely hacky, target dir: "++dir
  putStrLn$ "   Let's see what's in the target dir:"
  system $ "mkdir -p " ++ dir
  system $ "ls "++dir

  

