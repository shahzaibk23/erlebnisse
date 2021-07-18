import chisel3._
import buraq_mini.core.Core
import caravan.bus.common.{AddressMap, BusDecoder, Switch1toN}
import caravan.bus.wishbone.{Peripherals, WBRequest, WBResponse, WishboneConfig, WishboneDevice, WishboneErr, WishboneHost, WishboneMaster, WishboneSlave}
import caravan.bus.tilelink.{TLRequest, TLResponse, TilelinkConfig, TilelinkDevice, TilelinkErr, TilelinkHost, TilelinkMaster, TilelinkSlave}

import chisel3.experimental.Analog
import chisel3.stage.ChiselStage
import jigsaw.fpga.boards.artyA7._
import jigsaw.rams.fpga.BlockRam
import jigsaw.peripherals.gpio._

class Picofoxy(programFile: Option[String]) extends Module {
  val io = IO(new Bundle {
    val gpio_io = Vec(4, Analog(1.W))
  })

  val top = Module(new Top(programFile))
  val pll = Module(new PLL_8MHz())

  pll.io.clk_in1 := clock
  top.clock := pll.io.clk_out1

  val gpioInputWires = Wire(Vec(4, Bool()))
  val gpioOutputWires = Wire(Vec(4, Bool()))
  val gpioEnableWires = Wire(Vec(4, Bool()))

  val gpioPads = TriStateBuffer(quantity=4)
  val triStateBufferWires = for {
    ((((a,b),c),d),e) <- gpioPads zip gpioInputWires zip gpioOutputWires zip gpioEnableWires zip io.gpio_io
  } yield (a,b,c,d,e)

  triStateBufferWires map { case(buf: IOBUF, in: Bool, out: Bool, en: Bool, io: Analog) => {
    buf.io.connect(in, out, io, en)
  }}

  top.io.gpio_i := gpioInputWires.asUInt()
  gpioOutputWires := top.io.gpio_o.asBools()
  gpioEnableWires := top.io.gpio_en_o.asBools()

}


class Top(programFile: Option[String]) extends Module {
  val io = IO(new Bundle {
    val gpio_o = Output(UInt(4.W))
    val gpio_en_o = Output(UInt(4.W))
    val gpio_i = Input(UInt(4.W))
    //val gpio_intr_o = Output(UInt(32.W))
  })

  implicit val config: TilelinkConfig = TilelinkConfig()
  
  val tl_imem_host = Module(new TilelinkHost())
  val tl_imem_slave = Module(new TilelinkDevice())
  val tl_dmem_host = Module(new TilelinkHost())
  val tl_dmem_slave = Module(new TilelinkDevice())
  val tl_gpio_slave = Module(new TilelinkDevice())
  val imem = Module(BlockRam.createNonMaskableRAM(programFile, bus=config, rows=1024))
  val dmem = Module(BlockRam.createMaskableRAM(bus=config, rows=1024))
  val gpio = Module(new Gpio(new TLRequest(), new TLResponse()))
  val tlErr = Module(new TilelinkErr())
  val core = Module(new Core())



  val addressMap = new AddressMap
  addressMap.addDevice(Peripherals.DCCM, "h40000000".U(32.W), "h00000FFF".U(32.W), tl_dmem_slave)
  addressMap.addDevice(Peripherals.GPIO, "h40001000".U(32.W), "h00000FFF".U(32.W), tl_gpio_slave)
  val devices = addressMap.getDevices

  val switch = Module(new Switch1toN(new TilelinkMaster(), new TilelinkSlave(), devices.size))

  // WB <-> Core (fetch)
  tl_imem_host.io.reqIn <> core.io.imemReq
  core.io.imemRsp <> tl_imem_host.io.rspOut
  tl_imem_slave.io.reqOut <> imem.io.req
  tl_imem_slave.io.rspIn <> imem.io.rsp

  // WB <-> WB (fetch)
  tl_imem_host.io.tlMasterTransmitter <> tl_imem_slave.io.tlMasterReceiver
  tl_imem_slave.io.tlSlaveTransmitter <> tl_imem_host.io.tlSlaveReceiver

  // WB <-> Core (memory)
  tl_dmem_host.io.reqIn <> core.io.dmemReq
  core.io.dmemRsp <> tl_dmem_host.io.rspOut
  tl_dmem_slave.io.reqOut <> dmem.io.req
  tl_dmem_slave.io.rspIn <> dmem.io.rsp


  // Switch connection
  switch.io.hostIn <> tl_dmem_host.io.tlMasterTransmitter
  switch.io.hostOut <> tl_dmem_host.io.tlSlaveReceiver
  for (i <- 0 until devices.size) {
    switch.io.devIn(devices(i)._2.litValue().toInt) <> devices(i)._1.asInstanceOf[TilelinkDevice].io.tlSlaveTransmitter
    switch.io.devOut(devices(i)._2.litValue().toInt) <> devices(i)._1.asInstanceOf[TilelinkDevice].io.tlMasterReceiver
  }
  switch.io.devIn(devices.size) <> tlErr.io.tlSlaveTransmitter
  switch.io.devOut(devices.size) <> tlErr.io.tlMasterReceiver
  switch.io.devSel := BusDecoder.decode(tl_dmem_host.io.tlMasterTransmitter.bits.a_address, addressMap)


  tl_gpio_slave.io.reqOut <> gpio.io.req
  tl_gpio_slave.io.rspIn <> gpio.io.rsp

  io.gpio_o := gpio.io.cio_gpio_o(3,0)
  io.gpio_en_o := gpio.io.cio_gpio_en_o(3,0)
  gpio.io.cio_gpio_i := io.gpio_i

  core.io.stall_core_i := false.B
  core.io.irq_external_i := false.B


}

object PicofoxyDriver extends App {
  (new ChiselStage).emitVerilog(new Picofoxy(None))
}