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
  hudProjectNumber Int
  premiseId Text Maybe
  propertyName Text Maybe
  propertyStreet Text Maybe
  propertyCity Text Maybe
  propertyState Text Maybe
  propertyZip Int Maybe
  units Int Maybe
  initialEndorsementDate Text Maybe
  finalEndorsementDate Text Maybe
  originalMortgageAmount Text Maybe
  firstPaymentDate Text Maybe
  maturityDate Text Maybe
  termInMonths Text Maybe
  interestRate Int Maybe
  holderName Text Maybe
  holderCity Text Maybe
  holderState Text Maybe
  servicerName Text Maybe
  servicerCity Text Maybe
  servicerState Text Maybe
  sectionOfActCode Text Maybe
  soaCategory/SubCategory Text Maybe
  term_type Text Maybe
  terminationTypeDescription Text Maybe
  type Text Maybe
  term_date Text Maybe
  te Text Maybe
  tc Text Maybe
  status Text Maybe
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
    