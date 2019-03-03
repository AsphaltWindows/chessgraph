package analysis.dbnb.model

trait DynamicBinaryNaiveBayes {

  type Model
  type FeatureId

  def currentFeatures(model: Model): Set[FeatureId]

  def addFeatures(model: Model, features: Set[FeatureId]): Model

  def cullFeatures(model: Model): Model

  def trainModel(model: Model, dataPoint: Map[FeatureId, Boolean], result: Boolean): Unit

  def predict(model: Model, dataPoint: Map[FeatureId, Boolean]): Boolean

}
