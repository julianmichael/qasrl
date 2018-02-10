package qasrl.crowd

// default settings
trait QASRLEvaluationSettings {

  // used as URL parameters that indicate to the client which interface to use

  val evaluationTaskKey = "evaluation"

  // annotation pipeline hyperparameters

  val validationReward = 0.02
  val validationBonusPerQuestion = 0.02
  val validationBonusThreshold = 1

  def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  val validationAgreementBlockingThreshold = 0.8
  val validationAgreementGracePeriod = 20

  val invalidProportionBlockingThreshold = 0.8
  val invalidProportionBlockingGracePeriod = 10
}

object QASRLEvaluationSettings {
  val default = new QASRLEvaluationSettings {}
}
