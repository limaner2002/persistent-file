{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Model where
import Database.Persist.TH
import Data.Text (Text)

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
  propertyName Text Maybe maxlen=32
  propertyStreet Text Maybe maxlen=32
  propertyCity Text Maybe maxlen=32
  propertyState Text Maybe maxlen=2
  propertyZip Int Maybe maxlen=9
  units Int Maybe maxlen=6
  initialEndorsementDate Text Maybe
  finalEndorsementDate Text Maybe
  originalMortgageAmount Double Maybe
  firstPaymentDate Text Maybe
  maturityDate Text Maybe
  termInMonths Text Maybe
  interestRate Double Maybe
  holderName Text Maybe maxlen=32
  holderCity Text Maybe maxlen=32
  holderState Text Maybe maxlen=2
  servicerName Text Maybe maxlen=32
  servicerCity Text Maybe maxlen=32
  servicerState Text Maybe maxlen=2
  sectionOfActCode Text Maybe maxlen=3
  soaCategory/SubCategory Text Maybe maxlen=64
  term_type Int Maybe maxlen=2
  terminationTypeDescription Text Maybe maxlen=128
  type Text Maybe maxlen=8
  term_date Text Maybe
  te Text Maybe maxlen=2
  tc Text Maybe maxlen=2
  status Text Maybe maxlen=1
  deriving Show
Payment
  stateCode Int maxlen=2
  countyCode Int maxlen=3
  customerNumber String maxlen=9
  programCode Int maxlen=4
  programYear Int maxlen=4
  commodityCode Int maxlen=4
  transactionAmount Double maxlen=14
  transactionDate String maxlen=10
  categoryCode String maxlen=2
  farmNumber Int maxlen=7
  calendarYear Int maxlen=4
  fiscalYear Int maxlen=4
  sequenceNumber Int maxlen=9
  Primary sequenceNumber
  deriving Show
|]
    