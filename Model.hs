{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Model where
import Database.Persist.TH
import Database.Persist.Sql
import qualified Data.Text as T
import Data.Text.Read
import Parseable

-- Thousands separated comma double
newtype TCSDouble = TCSDouble Double
    deriving (Show
             , PersistFieldSql
             , PersistField)

instance Parseable TCSDouble where
    tRead x = readEither' tParse x
    tParse = TParse f
        where
          f txt = do
            case (double . removeCommas . removeSpaces) txt of
              Left msg -> Left msg
              Right (val, rem) -> Right (TCSDouble val, rem)
            
          removeCommas = T.replace (T.singleton ',') T.empty
          removeSpaces = T.replace (T.singleton ' ') T.empty

newtype ZipCode = ZipCode Int
    deriving (Show
              , PersistFieldSql
              , PersistField)

instance Parseable ZipCode where
    tRead x = readEither' tParse x
    tParse = TParse f
        where
          f txt = do
            case (decimal . removeDashes) txt of
              Left msg -> Left msg
              Right (val, rem) -> Right (ZipCode val, rem)
          removeDashes = T.replace (T.singleton '-') T.empty

qq = [persistLowerCase|
Dept
  deptno Int
  dname String
  loc String
  Primary deptno
  deriving Show
Emp
  empno Int
  ename String
  job String
  mgr Int Maybe
  hireDate String
  sal Double
  comm Double Maybe
  deptNo Int
  Primary empno
  deriving Show
Mortgage
  hudProjectNumber Int maxlen=10
  premiseId Int Maybe maxlen=10
  propertyName T.Text Maybe maxlen=32
  propertyStreet T.Text Maybe maxlen=32
  propertyCity T.Text Maybe maxlen=32
  propertyState T.Text Maybe maxlen=2
  propertyZip ZipCode Maybe maxlen=9
  units Int Maybe maxlen=6
  initialEndorsementDate T.Text Maybe
  finalEndorsementDate T.Text Maybe
  originalMortgageAmount TCSDouble Maybe
  firstPaymentDate T.Text Maybe
  maturityDate T.Text Maybe
  termInMonths T.Text Maybe
  interestRate Double Maybe
  holderName T.Text Maybe maxlen=32
  holderCity T.Text Maybe maxlen=32
  holderState T.Text Maybe maxlen=2
  servicerName T.Text Maybe maxlen=32
  servicerCity T.Text Maybe maxlen=32
  servicerState T.Text Maybe maxlen=2
  sectionOfActCode T.Text Maybe maxlen=3
  soaCategory/SubCategory T.Text Maybe maxlen=64
  term_type T.Text Maybe maxlen=2
  terminationTypeDescription T.Text Maybe maxlen=128
  type T.Text Maybe maxlen=8
  term_date T.Text Maybe
  te T.Text Maybe maxlen=2
  tc T.Text Maybe maxlen=2
  status T.Text Maybe maxlen=1
  deriving Show
Payment
  stateCode Int maxlen=2
  countyCode Int maxlen=3
  customerNumber String maxlen=9
  programCode Int maxlen=4
  programYear Int maxlen=4
  commodityCode Int maxlen=4
  transactionAmount TCSDouble maxlen=14
  transactionDate String maxlen=10
  categoryCode String maxlen=2
  farmNumber Int maxlen=7
  calendarYear Int maxlen=4
  fiscalYear Int maxlen=4
  sequenceNumber Int maxlen=9
  Primary sequenceNumber
  deriving Show
|]

