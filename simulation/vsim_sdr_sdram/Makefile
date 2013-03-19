#
#	Makefile for ModelSim simulation
#


SRCDIR=../../
#OPTIONS=-93 -quiet -check_synthesis -lint -pedanticerrors
OPTIONS=-93 -quiet

ifeq ($(WINDIR),)
	S=:
else
	S=\;
endif

# Use Wine on OSX
# I would like to use a better way, but some shell variables
# are not set within make.... Don't know why...
ifeq ($(TERM_PROGRAM),Apple_Terminal)
	WINE=wine
else
	WINE=
endif

ifeq ($(USE_DMA_DEVICE),y)
TB = sdr_sdram_dma_controller_tb

VHDL_FILES += $(SRCDIR)../patmos/ise/ml605_edk/pcores/dma_controller_dtl_v1_00_a/hdl/vhdl/dma_controller_dtl.e.vhd
VHDL_FILES += $(SRCDIR)../patmos/ise/ml605_edk/pcores/dma_controller_dtl_v1_00_a/hdl/vhdl/dma_controller_dtl.p.vhd
VHDL_FILES += $(SRCDIR)../patmos/ise/ml605_edk/pcores/dma_controller_dtl_v1_00_a/hdl/vhdl/dma_controller_dtl.a.vhd
else
TB = sdr_sdram_tb
endif

VHDL_FILES += $(SRCDIR)vhdl/sdram_config.vhd
VHDL_FILES += $(SRCDIR)vhdl/sdram_config-IS42S16160B-7TLI.vhd
VHDL_FILES += $(SRCDIR)vhdl/sdram_controller_interface.vhd
VHDL_FILES += $(SRCDIR)vhdl/sdr_dram.vhd

VHDL_FILES += $(SRCDIR)simulation/mt48lc2m32b2_2.vhd

VHDL_FILES += $(SRCDIR)simulation/$(TB).vhd




all: simulate

batch: VSIM_OPT:=-c -t 100ps -do batch.do
batch: vsim


simulate: VSIM_OPT:=-t 100ps -do sim.do
simulate: vsim

vsim: clean base
	$(WINE) vsim $(VSIM_OPT) $(TB)

base:
	-$(WINE) vlib work
	for f in $(VHDL_FILES) ; do \
	$(WINE) vcom $(OPTIONS) $$f \
	; done

get_timing:
	cat /dev/null | make batch > log
	grep -B 1 'Time:' log | sed '/Time:/! N;s/\n/ /' > log.time
	grep '\(RD\|WR\) at' log.time | awk '{print $$5" "$$11}' > log.cmd_time
	l=0; cat log.cmd_time | while read c t ; do echo $$lc $$((t-l)); lc=$$c; l=$$t; done

clean:
	-rm -f transcript
	-rm -f *.wlf
	-rm -rf work
