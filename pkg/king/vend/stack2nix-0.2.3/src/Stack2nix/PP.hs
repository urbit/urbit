module Stack2nix.PP
   (ppIndented, ppSingletons) where

import           Text.PrettyPrint (Doc, Mode(..), TextDetails(..), fullRender, render)

-- | Formats the derivation doc with indentation and lines wrapped at 100 chars.
ppIndented :: Doc -> String
ppIndented = render

-- | Formats the derivation doc without indentation and with each list
-- element and function argument on its own line.
-- We go to this effort to "ugly-print" so that the resulting file is
-- less susceptible to merge conflicts when checked into git.
ppSingletons :: Doc -> String
ppSingletons doc = fixSpace . snd $ fullRender LeftMode 80 1.5 printer (0, "") doc
  where
    printer :: TextDetails -> (Int, String) -> (Int, String)
    printer (Chr c) (n, s) = ppChar n s c
    printer (Str s1) s2    = fmap (s1 ++) s2
    printer (PStr s1) s2   = fmap (s1 ++) s2

    ppChar :: Int -> String -> Char -> (Int, String)
    ppChar n s c = (n', s')
      where
        s' = case c of
               ',' -> "\n," ++ s
               ' ' -> (if n /= 0 then '\n' else ' '):s
               '{' -> "{\n " ++ s
               '}' -> "\n}" ++ s
               _   -> c:s
        n' = case c of
               '[' -> n - 1  -- PrettyPrint works backwards
               ']' -> n + 1
               _   -> n

    -- remove single trailing spaces
    fixSpace :: String -> String
    fixSpace (' ':'\n':s) = '\n':fixSpace s
    fixSpace (c:s) = c:fixSpace s
    fixSpace [] = []
