// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{
  BuiltInDevices, BuiltInZeroDeviceParams, BuiltInErrorDeviceParams, HasBuiltInDeviceParams
}
import freechips.rocketchip.tilelink.{
  TLArbiter, RegionReplicator, ReplicatedRegion, HasTLBusParams, TLBusWrapper,
  TLBusWrapperInstantiationLike, TLXbar, TLEdge, TLInwardNode, TLOutwardNode,
  TLFIFOFixer, TLTempNode,BankSystemHasher
}
import freechips.rocketchip.util.Location

case class SystemBusParams(
    beatBytes: Int,
    blockBytes: Int,
    policy: TLArbiter.Policy = TLArbiter.roundRobin,
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[BuiltInZeroDeviceParams] = None,
    errorDevice: Option[BuiltInErrorDeviceParams] = None,
    replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): SystemBus = {
    val sbus = LazyModule(new SystemBus(this, loc.name))
    sbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> sbus)
    sbus
  }
}

case class CacheHashParams(
    blockBytes: Int = 64,
    nBanks: Int = 0,
    hashTagBits: Int = 12){
}

case object CacheHashParamsKey extends Field[CacheHashParams]

class WithNoCacheHash() extends Config((site, here, up) => {
  case CacheHashParamsKey => CacheHashParams()
})

class WithCacheHash(blockBytes: Int = 64,nBanks: Int = 4,hashTagBits: Int = 12) extends Config((site, here, up) => {
  case CacheHashParamsKey => CacheHashParams(blockBytes, nBanks, hashTagBits)
})


class SystemBus(params: SystemBusParams, name: String = "system_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val system_bus_xbar = LazyModule(new TLXbar(policy = params.policy, nameSuffix = Some(name)))
  //输出CacheHashParamsKey的值
  println(s"CacheHashParamsKey: ${p(CacheHashParamsKey)}")
  val inwardNode: TLInwardNode = system_bus_xbar.node :=* BankSystemHasher("sbus",p(CacheHashParamsKey).blockBytes,p(CacheHashParamsKey).nBanks,p(CacheHashParamsKey).hashTagBits) :=* TLFIFOFixer(TLFIFOFixer.allVolatile) :=* replicator.map(_.node).getOrElse(TLTempNode())
  val outwardNode: TLOutwardNode = system_bus_xbar.node
  def busView: TLEdge = system_bus_xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
