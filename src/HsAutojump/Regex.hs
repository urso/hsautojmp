
module HsAutojump.Regex where

import Data.ByteString as BS (ByteString)
import Data.Maybe (isJust)
import qualified Text.Regex.PCRE.Light as R (Regex,PCREOption,PCREExecOption,
                                             compileM, caseless, match, 
                                             exec_no_utf8_check)

class Regexable a where
  compileRegex :: a -> Either String R.Regex
  execOps :: a -> [R.PCREExecOption]
  (=~) :: ByteString -> a -> Bool

  execOps = const [R.exec_no_utf8_check]

  bs =~ r = either (error "invalid regex") apply $ compileRegex r
    where apply regex = isJust (R.match regex bs (execOps r))

instance Regexable R.Regex where
  compileRegex = Right . id

instance Regexable ByteString where
  compileRegex bs = R.compileM bs []

data RegexOptPattern = RegexOpt [R.PCREOption] [R.PCREExecOption] ByteString

instance Regexable RegexOptPattern where
  compileRegex (RegexOpt opts _ bs) = R.compileM bs opts
  execOps (RegexOpt _ opts _) = opts

cfgRegex compileOpts execOpts r = RegexOpt compileOpts execOpts r

regexCase True  = cfgRegex [R.caseless] [R.exec_no_utf8_check]
regexCase False = cfgRegex [] [R.exec_no_utf8_check]

