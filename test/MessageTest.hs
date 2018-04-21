module MessageTest where

import Test.HUnit

import Message (sumMessages)
import Types


testSumMessage = TestCase (assertEqual
  "Sum of three messages" 
  (sumMessages [Message 0.5 100, Message 1 99, Message 0.3 101])
  (1 * 1 + 2 * 0.5 + 3 * 0.3)
  )

messageTests = TestList [TestLabel "Sum three messages with different timestamps" testSumMessage]

