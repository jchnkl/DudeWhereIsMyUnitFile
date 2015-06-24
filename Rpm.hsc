{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "rpm_stub.c"

module Rpm where

import Control.Monad
import Foreign.Ptr
import Foreign.C
import Foreign.C.String
import Foreign.Storable

data C_String_List = C_String_List
    { count :: CSize
    , strings :: [CString]
    }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

foreign import ccall "rpm_file_list" c_rpm_file_list :: CString -> IO (Ptr C_String_List)
foreign import ccall "string_list_free" c_string_list_free :: Ptr C_String_List -> IO ()

rpmFileList :: String -> IO [String]
rpmFileList r = do
    withCString r $ \cr -> do
        ptr <- c_rpm_file_list cr
        (count_ :: CSize) <- #{peek string_list, count} ptr
        (cstrings_ :: Ptr CString) <- #{peek string_list, strings} ptr
        strings_ <- forM [0 .. fromIntegral count_ - 1] $ \n -> do
            peekElemOff cstrings_ n
        ss <- mapM peekCString strings_
        c_string_list_free ptr
        return ss
