

cva6_build_dir = $(build_dir)/cva6
cva6_rev = f37feb2b63a8c83197212b57cc1846749b51e215

$(cva6_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/cva6.git $@
	cd $@ && git checkout $(cva6_rev)
	cd $@ && git submodule update --init --recursive

$(build_dir)/cva6.pickle.sv: | $(cva6_build_dir)
	bender sources -f -d $(cva6_build_dir) -t cv64a6_imafdc_sv39 | morty -f /dev/stdin -o $@

$(build_dir)/cva6_top.pickle.sv: | $(cva6_build_dir)
	bender sources -f -d $(cva6_build_dir) -t cv64a6_imafdc_sv39 | morty -f /dev/stdin -o $@ --top cva6

pickles: $(build_dir)/cva6.pickle.sv $(build_dir)/cva6_top.pickle.sv
