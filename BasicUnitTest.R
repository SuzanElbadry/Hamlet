library(testthat)

test_that('First 2 Scenes Speakers', {
  Sub <- substr(Hamlet,0,21039)
  SubHamletTest <- data.table(text=Sub)
  SpeakersPart <- setorder(Task1(SubHamletTest),-Total)
  
  
  RightSpeakersCount <- data.table(Speakers=c("HORATIO","HAMLET","KING CLAUDIUS","MARCELLUS","BERNARDO","FRANCISCO","QUEEN GERTRUDE","LAERTES","LORD POLONIUS","CORNELIUS","VOLTIMAND"),
                                   Total=c(149,95,93,52,38,10,10,7,4,1,1))
  
  expect_that(SpeakersPart[,1], equals(RightSpeakersCount[,1]))
})

test_that('First 2 Scenes Speakers Count', {
  Sub <- substr(Hamlet,0,21039)
  SubHamletTest <- data.table(text=Sub)
  SpeakersPart <- setorder(Task1(SubHamletTest),-Total)
  RightSpeakersCount <- data.table(Speakers=c("HORATIO","HAMLET","KING CLAUDIUS","MARCELLUS","BERNARDO","FRANCISCO","QUEEN GERTRUDE","LAERTES","LORD POLONIUS","CORNELIUS","VOLTIMAND"),
                                   Total=c(149,95,93,52,38,10,10,7,4,1,1))
  expect_that(SpeakersPart[,2], equals(RightSpeakersCount[,2]))
})