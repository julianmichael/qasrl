package qasrl.crowd

// default settings
trait QASRLSettings {

  // used as URL parameters that indicate to the client which interface to use

  val generationTaskKey = "generation"
  val validationTaskKey = "validation"
  val dashboardTaskKey = "dashboard"

  // annotation pipeline hyperparameters

  def generationRewardCents = 5
  def generationReward = generationRewardCents * 0.01

  def generationBonus(nValidQAs: Int) = {
    // no bonus for the first question, hence -1
    val cents = (0 until (nValidQAs - 1)).map(_ + generationRewardCents).sum
    cents * 0.01
  }

  def validationReward = 0.08
  def validationBonusPerQuestion = 0.02
  def validationBonusThreshold = 4

  def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  def generationCoverageQuestionsPerVerbThreshold = 2.0
  def generationCoverageGracePeriod = 15

  def generationAccuracyBlockingThreshold = 0.85
  def generationAccuracyGracePeriod = 15

  def validationAgreementBlockingThreshold = 0.85
  def validationAgreementGracePeriod = 10
}

object QASRLSettings {
  val default = new QASRLSettings {}
}
