module Common.Types where

type Url = String
type Route = String
type UserName = String
type Password = String
type ProjectName = String
type PackageName = String

infixr 5 </>
(</>) :: String -> String -> String
(</>) s1 s2 = s1 ++ "/" ++ s2
