{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.Lua.Raw where

import Foreign.C
import Foreign.Lua.Types
import Foreign.Ptr

#include "lua.h"

-- TODO: lua_getallocf, lua_setallocf
-- TODO: Debugger functions

-- Some of the Lua functions may call a Haskell function, and trigger
-- garbage collection, rescheduling etc. This means we must declare these
-- functions as 'safe'.
--
-- FIXME: Currently everthing marked as 'safe'. Change the ones that don't
-- call Haskell to 'unsafe'.


--------------------------------------------------------------------------------
-- * State manipulation

foreign import ccall "lua.h lua_newstate"
  c_lua_newstate :: FunPtr LuaAlloc -> Ptr () -> IO LuaState

foreign import ccall "lua.h lua_close"
  c_lua_close :: LuaState -> IO ()

foreign import ccall "lua.h lua_newthread"
  c_lua_newthread :: LuaState -> IO LuaState

foreign import ccall "lua.h lua_atpanic"
  c_lua_atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)


--------------------------------------------------------------------------------
-- * Basic stack manipulation

foreign import ccall "lua.h lua_gettop"
  c_lua_gettop :: LuaState -> IO CInt

foreign import ccall "lua.h lua_settop"
  c_lua_settop :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushvalue"
  c_lua_pushvalue :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_remove"
  c_lua_remove :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_insert"
  c_lua_insert :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_replace"
  c_lua_replace :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_checkstack"
  c_lua_checkstack :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_xmove"
  c_lua_xmove :: LuaState -> LuaState -> CInt -> IO ()


--------------------------------------------------------------------------------
-- * Stack access functions

foreign import ccall "lua.h lua_isnumber"
  c_lua_isnumber :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isstring"
  c_lua_isstring :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_iscfunction"
  c_lua_iscfunction :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isuserdata"
  c_lua_isuserdata :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_type"
  c_lua_type :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_typename"
  c_lua_typename :: LuaState -> CInt -> IO (Ptr CChar)

foreign import ccall "lua.h lua_equal"
  c_lua_equal :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_rawequal"
  c_lua_rawequal :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_lessthan"
  c_lua_lessthan :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_tonumber"
  c_lua_tonumber :: LuaState -> CInt -> IO LuaNumber

foreign import ccall "lua.h lua_tointeger"
  c_lua_tointeger :: LuaState -> CInt -> IO LuaInteger

foreign import ccall "lua.h lua_toboolean"
  c_lua_toboolean :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_tolstring"
  c_lua_tolstring :: LuaState -> CInt -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall "lua.h lua_objlen"
  c_lua_objlen :: LuaState -> CInt -> IO CSize

foreign import ccall "lua.h lua_tocfunction"
  c_lua_tocfunction :: LuaState -> CInt -> IO (FunPtr LuaCFunction)

foreign import ccall "lua.h lua_touserdata"
  c_lua_touserdata :: LuaState -> CInt -> IO (Ptr a)

foreign import ccall "lua.h lua_tothread"
  c_lua_tothread :: LuaState -> CInt -> IO LuaState

foreign import ccall "lua.h lua_topointer"
  c_lua_topointer :: LuaState -> CInt -> IO (Ptr ())


--------------------------------------------------------------------------------
-- * Push functions

foreign import ccall "lua.h lua_pushnil"
  c_lua_pushnil :: LuaState -> IO ()

foreign import ccall "lua.h lua_pushnumber"
  c_lua_pushnumber :: LuaState -> LuaNumber -> IO ()

foreign import ccall "lua.h lua_pushinteger"
  c_lua_pushinteger :: LuaState -> LuaInteger -> IO ()

foreign import ccall "lua.h lua_pushlstring"
  c_lua_pushlstring :: LuaState -> Ptr CChar -> CSize -> IO ()

foreign import ccall "lua.h lua_pushstring"
  c_lua_pushstring :: LuaState -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_pushcclosure"
  c_lua_pushcclosure :: LuaState -> FunPtr LuaCFunction -> CInt -> IO ()

foreign import ccall "lua.h lua_pushboolean"
  c_lua_pushboolean :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushlightuserdata"
  c_lua_pushlightuserdata :: LuaState -> Ptr a -> IO ()

foreign import ccall "lua.h lua_pushthread"
  c_lua_pushthread :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Get functions

foreign import ccall "lua.h lua_gettable"
  c_lua_gettable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_getfield"
  c_lua_getfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawget"
  c_lua_rawget :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawgeti"
  c_lua_rawgeti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_createtable"
  c_lua_createtable :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_newuserdata"
  c_lua_newuserdata :: LuaState -> CInt -> IO (Ptr ())

foreign import ccall "lua.h lua_getmetatable"
  c_lua_getmetatable :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_getfenv"
  c_lua_getfenv :: LuaState -> CInt -> IO ()


--------------------------------------------------------------------------------
-- * Get functions

foreign import ccall "lua.h lua_settable"
  c_lua_settable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_setfield"
  c_lua_setfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawset"
  c_lua_rawset :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawseti"
  c_lua_rawseti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_setmetatable"
  c_lua_setmetatable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_setfenv"
  c_lua_setfenv :: LuaState -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * 'load' and 'call' functions (load and run Lua code)

foreign import ccall "lua.h lua_call"
  c_lua_call :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_pcall"
  c_lua_pcall :: LuaState -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_cpcall"
  c_lua_cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO CInt

foreign import ccall "lua.h lua_load"
  c_lua_load :: LuaState -> FunPtr LuaReader -> Ptr () -> Ptr CChar -> IO CInt

foreign import ccall "lua.h lua_dump"
  c_lua_dump :: LuaState -> FunPtr LuaWriter -> Ptr () -> IO ()


--------------------------------------------------------------------------------
-- * Coroutine functions

foreign import ccall "lua.h lua_yield"
  c_lua_yield :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_resume"
  c_lua_resume :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_status"
  c_lua_status :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Garbage-collection functions and options

foreign import ccall "lua.h lua_gc"
  c_lua_gc :: LuaState -> CInt -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * Miscellaneous functions

foreign import ccall "lua.h lua_error"
  c_lua_error :: LuaState -> IO CInt

foreign import ccall "lua.h lua_next"
  c_lua_next :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_concat"
  c_lua_concat :: LuaState -> CInt -> IO ()

foreign import ccall "lualib.h luaL_openlibs"
  c_luaL_openlibs :: LuaState -> IO ()

foreign import ccall "lualib.h luaopen_base"
  c_lua_open_base :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_base"
  c_lua_open_base_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_table"
  c_lua_open_table :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_table"
  c_lua_open_table_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_io"
  c_lua_open_io :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_io"
  c_lua_open_io_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_os"
  c_lua_open_os :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_os"
  c_lua_open_os_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_string"
  c_lua_open_string :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_string"
  c_lua_open_string_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_math"
  c_lua_open_math :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_math"
  c_lua_open_math_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_debug"
  c_lua_open_debug :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_debug"
  c_lua_open_debug_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_package"
  c_lua_open_package :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_package"
  c_lua_open_package_ptr :: FunPtr (LuaState -> IO CInt)


--------------------------------------------------------------------------------
-- * The Auxiliary Library

foreign import ccall "lauxlib.h luaL_newstate"
  c_luaL_newstate :: IO LuaState

foreign import ccall "lauxlib.h luaL_newmetatable"
  c_luaL_newmetatable :: LuaState -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_argerror"
  c_luaL_argerror :: LuaState -> CInt -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_ref"
  c_luaL_ref :: LuaState -> CInt -> IO CInt

foreign import ccall "lauxlib.h luaL_unref"
  c_luaL_unref :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lauxlib.h luaL_loadfile"
  c_luaL_loadfile :: LuaState -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_loadstring"
  c_luaL_loadstring :: LuaState -> Ptr CChar -> IO CInt