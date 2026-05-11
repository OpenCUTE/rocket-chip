// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, TransferSizes}
import chisel3.util.{log2Ceil}
import chisel3._
import freechips.rocketchip.util.SeqToAugmentedSeq
import chisel3.util.Cat
import freechips.rocketchip.subsystem.ExtMem

case class BankBinderNode(mask: BigInt)(implicit valName: ValName) extends TLCustomNode
{
  private val bit = mask & -mask
  val maxXfer = TransferSizes(1, if (bit == 0 || bit > 4096) 4096 else bit.toInt)
  val ids = AddressSet.enumerateMask(mask)

  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    val ports = ids.size
    val oStar = if (oStars == 0) 0 else (ports - oKnown) / oStars
    val iStar = if (iStars == 0) 0 else (ports - iKnown) / iStars
    require (ports == iKnown + iStar*iStars, s"${name} must have ${ports} inputs, but has ${iKnown} + ${iStar}*${iStars} (at ${lazyModule.line})")
    require (ports == oKnown + oStar*oStars, s"${name} must have ${ports} outputs, but has ${oKnown} + ${oStar}*${oStars} (at ${lazyModule.line})")
    (iStar, oStar)
  }

  def mapParamsD(n: Int, p: Seq[TLMasterPortParameters]): Seq[TLMasterPortParameters] =
    (p zip ids) map { case (cp, id) => cp.v1copy(clients = cp.clients.map { c => c.v1copy(
      visibility         = c.visibility.flatMap { a => a.intersect(AddressSet(id, ~mask))},
      supportsProbe      = c.supports.probe      intersect maxXfer,
      supportsArithmetic = c.supports.arithmetic intersect maxXfer,
      supportsLogical    = c.supports.logical    intersect maxXfer,
      supportsGet        = c.supports.get        intersect maxXfer,
      supportsPutFull    = c.supports.putFull    intersect maxXfer,
      supportsPutPartial = c.supports.putPartial intersect maxXfer,
      supportsHint       = c.supports.hint       intersect maxXfer)})}

  def mapParamsU(n: Int, p: Seq[TLSlavePortParameters]): Seq[TLSlavePortParameters] =
    (p zip ids) map { case (mp, id) => mp.v1copy(managers = mp.managers.flatMap { m =>
      val addresses = m.address.flatMap(a => a.intersect(AddressSet(id, ~mask)))
      if (addresses.nonEmpty)
        Some(m.v1copy(
          address            = addresses,
          supportsAcquireT   = m.supportsAcquireT   intersect maxXfer,
          supportsAcquireB   = m.supportsAcquireB   intersect maxXfer,
          supportsArithmetic = m.supportsArithmetic intersect maxXfer,
          supportsLogical    = m.supportsLogical    intersect maxXfer,
          supportsGet        = m.supportsGet        intersect maxXfer,
          supportsPutFull    = m.supportsPutFull    intersect maxXfer,
          supportsPutPartial = m.supportsPutPartial intersect maxXfer,
          supportsHint       = m.supportsHint       intersect maxXfer))
      else None
    })}
}

/* A BankBinder is used to divide contiguous memory regions into banks, suitable for a cache  */
class BankBinder(mask: BigInt)(implicit p: Parameters) extends LazyModule
{
  val node = BankBinderNode(mask)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}

object BankBinder
{
  def apply(mask: BigInt)(implicit p: Parameters): TLNode = {
    val binder = LazyModule(new BankBinder(mask))
    binder.node
  }

  def apply(nBanks: Int, granularity: Int)(implicit p: Parameters): TLNode = {
    if (nBanks > 0) apply(granularity * (nBanks-1))
    else TLTempNode()
  }
}

class BankSystemHasher(retype: Boolean, ename: String, blockBytes: Int, nBanks: Int, hashTagBits: Int)(implicit p: Parameters) extends LazyModule
{

  val node = TLAdapterNode()

  val bankBits = log2Ceil(nBanks)
  val offsetBits = log2Ceil(blockBytes)
  //nbanks must be a power of two
  require((nBanks & (nBanks - 1)) == 0, s"nBanks must be a power of two, got $nBanks")

  //找到dram的地址范围
  val dram_base = p(ExtMem).get.master.base
  val dram_size = p(ExtMem).get.master.size

  // Ipoly-cache bank_id hash,degree 2, ipoly(7) m=2
  //I_id  1 0
  //   0	  1
  //   1	1	 
  //   2	1	1
  //   3	0	1
  //   4	1	0
  //   5	1	1 
  //   6	0	1
  //   7	1	0
  //   8	1	1
  //   9	0	1
  //   10	1	0
  //   11	1	1
  //   12	0	1
  //   13	1	0
  //   14	1	1
  //   15	0	1
  //   16 1 0
  //   17 1 1
  //   18 0 1
  //   19 1 0
  //   20 1 1
  //   21 0 1
  //   22 1 0
  //   23 1 1
  //   24 0 1
  //   25 1 0
  
  // Ipoly-cache hash,degree 10, ipoly(2011) m=10
  // I_id	9                 0
  //   0	 									1
  //   1	 								1	 
  //   2	 							1	  	
  //   3	 						1		  	
  //   4	 					1			  	
  //   5	 				1				  	
  //   6	 			1					  	
  //   7	 		1						  	
  //   8	 	1							  	
  //   9	1								  	
  //   10	0	0	0	0	0	0	1	0	0	1
  //   11 0	0	0	0	0	1	0	0	1	0
  //   12 0	0	0	0	1	0	0	1	0	0
  //   13 0	0	0	1	0	0	1	0	0	0
  //   14 0	0	1	0	0	1	0	0	0	0
  //   15 0	1	0	0	1	0	0	0	0	0
  //   16 1	0	0	1	0	0	0	0	0	0
  //   17 0	0	1	0	0	0	1	0	0	1
  //   18 0	1	0	0	0	1	0	0	1	0
  //   19 1	0	0	0	1	0	0	1	0	0
  //   20 0	0	0	1	0	0	0	0	0	1
  //   21 0	0	1	0	0	0	0	0	1	0
  //   22 0	1	0	0	0	0	0	1	0	0
  //   23 1	0	0	0	0	0	1	0	0	0

  def xorhashSetBankid(address: UInt): UInt = {
    
    val a = address >> (offsetBits)
    val b = address >> (offsetBits + bankBits)

    val offsetaddr = address(offsetBits-1,0) // offset address
    val highaddr = address >> (offsetBits + hashTagBits) // high address bits
    //b初始化为12个bool
    val slice_id = WireInit(VecInit(Seq.fill(bankBits)(false.B)))
    //根据面前生成的H-matrix，计算b的值
    slice_id(0) := a(0) ^ a(2) ^ a(3) ^ a(5) ^ a(6) ^ a(8) ^ a(9)  ^ a(11) ^ a(12) ^ a(14) ^ a(15) ^ a(17) ^ a(18) ^ a(20) ^ a(21) ^ a(23) ^ a(24)
    slice_id(1) := a(1) ^ a(2) ^ a(4) ^ a(5) ^ a(7) ^ a(8) ^ a(10) ^ a(11) ^ a(13) ^ a(14) ^ a(16) ^ a(17) ^ a(19) ^ a(20) ^ a(22) ^ a(23)


    val set_id = WireInit(VecInit(Seq.fill(hashTagBits - bankBits)(false.B)))
    set_id(0) := b(0) ^ b(10) ^ b(17) ^ b(20)
    set_id(1) := b(1) ^ b(11) ^ b(18) ^ b(21)
    set_id(2) := b(2) ^ b(12) ^ b(19) ^ b(22)
    set_id(3) := b(3) ^ b(10) ^ b(13) ^ b(17) ^ b(23)
    set_id(4) := b(4) ^ b(11) ^ b(14) ^ b(18)
    set_id(5) := b(5) ^ b(12) ^ b(15) ^ b(19)
    set_id(6) := b(6) ^ b(13) ^ b(16) ^ b(20)
    set_id(7) := b(7) ^ b(14) ^ b(17) ^ b(21)
    set_id(8) := b(8) ^ b(15) ^ b(18) ^ b(22)
    set_id(9) := b(9) ^ b(16) ^ b(19) ^ b(23)

    Cat(highaddr,set_id.asUInt,slice_id.asUInt,offsetaddr) // concatenate a[hashTagBits-1:0], slice_id[bankBits-1:0], a[offsetBits-1:0]
  }

  def rexorhashSetBankid(address: UInt): UInt = {
    

    val b = address >> (offsetBits + bankBits)

    val offsetaddr = address(offsetBits-1,0) // offset address
    val highaddr = address >> (offsetBits + hashTagBits) // high address bits
    val slice_id = address >> offsetBits // bank id

    val re_set_id = WireInit(VecInit(Seq.fill(hashTagBits - bankBits)(false.B)))
    re_set_id(0) := b(0) ^ b(10) ^ b(17) ^ b(20)
    re_set_id(1) := b(1) ^ b(11) ^ b(18) ^ b(21)
    re_set_id(2) := b(2) ^ b(12) ^ b(19) ^ b(22)
    re_set_id(3) := b(3) ^ b(10) ^ b(13) ^ b(17) ^ b(23)
    re_set_id(4) := b(4) ^ b(11) ^ b(14) ^ b(18)
    re_set_id(5) := b(5) ^ b(12) ^ b(15) ^ b(19)
    re_set_id(6) := b(6) ^ b(13) ^ b(16) ^ b(20)
    re_set_id(7) := b(7) ^ b(14) ^ b(17) ^ b(21)
    re_set_id(8) := b(8) ^ b(15) ^ b(18) ^ b(22)
    re_set_id(9) := b(9) ^ b(16) ^ b(19) ^ b(23)
    // printf("re_set_id = %b%b%b%b%b%b%b%b%b%b\n", re_set_id(9), re_set_id(8), re_set_id(7), re_set_id(6), re_set_id(5), re_set_id(4), re_set_id(3), re_set_id(2), re_set_id(1), re_set_id(0))
    // printf("re_set_id.asUInt = %b\n", re_set_id.asUInt)

    val a = Cat(highaddr,re_set_id.asUInt,slice_id(bankBits-1,0)) // concatenate a[hashTagBits-1:0], slice_id[bankBits-1:0], a[offsetBits-1:0]
    val re_slice_id = WireInit(VecInit(Seq.fill(bankBits)(false.B)))
    //根据面前生成的H-matrix，计算b的值
    re_slice_id(0) := a(0) ^ a(2) ^ a(3) ^ a(5) ^ a(6) ^ a(8) ^ a(9)  ^ a(11) ^ a(12) ^ a(14) ^ a(15) ^ a(17) ^ a(18) ^ a(20) ^ a(21) ^ a(23) ^ a(24)
    re_slice_id(1) := a(1) ^ a(2) ^ a(4) ^ a(5) ^ a(7) ^ a(8) ^ a(10) ^ a(11) ^ a(13) ^ a(14) ^ a(16) ^ a(17) ^ a(19) ^ a(20) ^ a(22) ^ a(23)

    Cat(highaddr,re_set_id.asUInt,re_slice_id.asUInt,offsetaddr) // concatenate a[hashTagBits-1:0], slice_id[bankBits-1:0], a[offsetBits-1:0]
  }

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
      println("[edge]BankSystemHasher: blockBytes = " + blockBytes + ", nBanks = " + nBanks + ", hashTagBits = " + hashTagBits)
      when(in.a.bits.address >= dram_base.U && in.a.bits.address < (dram_base+dram_size).U) {
        // If the address is in the DDR region, we hash it
        if(!retype) {
          out.a.bits.address := xorhashSetBankid(in.a.bits.address)
        } else {
          out.a.bits.address := rexorhashSetBankid(in.a.bits.address)
        }
      }
      // when(in.b.bits.address >= 0x80000000L.U && in.b.bits.address < 0xC0000000L.U) {
      //   // If the address is in the DDR region, we hash it
      //   out.b.bits.address := xorhashbankid(in.b.bits.address)
      // }
      when(in.c.bits.address >= dram_base.U && in.c.bits.address < (dram_base+dram_size).U) {
        // If the address is in the DDR region, we hash it
        if(!retype) {
          out.c.bits.address := xorhashSetBankid(in.c.bits.address)
        } else {
          out.c.bits.address := rexorhashSetBankid(in.c.bits.address)
        }
      }
      when(out.b.bits.address >= dram_base.U && out.b.bits.address < (dram_base+dram_size).U) {
        // If the address is in the DDR region, we hash it
        if(!retype) {
          in.b.bits.address := rexorhashSetBankid(out.b.bits.address)
        } else {
          in.b.bits.address := xorhashSetBankid(out.b.bits.address)
        }
      }
    }
  }
}

object BankSystemHasher {

  def apply(name: String,blockBytes: Int, nBanks: Int, hashTagBits: Int)(implicit p: Parameters): TLNode = {
    println()
    if (nBanks >= 2) {
      println("BankSystemHasher: blockBytes = " + blockBytes + ", nBanks = " + nBanks + ", hashTagBits = " + hashTagBits)
      val hasher = LazyModule(new BankSystemHasher(retype=false,name,blockBytes, nBanks, hashTagBits))
      hasher.node
    } else TLTempNode()
  }
}

object ReBankSystemHasher {

  def apply(name: String,blockBytes: Int, nBanks: Int, hashTagBits: Int)(implicit p: Parameters): TLNode = {
    println()
    if (nBanks >= 2) {
      println("BankSystemHasher: blockBytes = " + blockBytes + ", nBanks = " + nBanks + ", hashTagBits = " + hashTagBits)
      val hasher = LazyModule(new BankSystemHasher(retype=true,name,blockBytes, nBanks, hashTagBits))
      hasher.node
    } else TLTempNode()
  }
}