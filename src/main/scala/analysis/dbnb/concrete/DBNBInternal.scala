package analysis.dbnb.concrete

import scala.collection.mutable.{Map => MutMap}

class DBNBInternal[IdType](initialIds: Set[IdType]) {

  import DBNBInternal._

  private var frequencyTables: MutMap[IdType, MutMap[(FeatureType, LabelType), Double]] = MutMap() ++
    initialIds.map(_ -> initialFeatureFreq)

//  def addDataPoint(feats: Map[IdType, Boolean], label: LabelType): Unit = {
//    feats
//      .foreach { case (id, v) =>
//        frequencyTables
//          .get(id)
//          .foreach
//      }
//  }
}


object DBNBInternal {

  type FeatureType = Boolean
  type LabelType = Boolean

  def initialFeatureFreq: MutMap[(FeatureType, LabelType), Double] = MutMap() ++ initialFreqMap

  private val initialFreqMap: Map[(FeatureType, LabelType), Double] = Set(
    (false, false),
    (false, true),
    (true, false),
    (true, true)
  ).map(_ -> 1D)
    .toMap
}
