# Repository

snitch_build_dir = $(build_dir)/snitch
# snitch_rev = 18b0d683b03a308c1942ed764eeb1523c622379d
snitch_rev = 1f2a396b70584b0dd9f4ee595501ec5db5d60024
# fatal: reference is not a tree: 18b0d683b03a308c1942ed764eeb1523c622379d

$(snitch_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/snitch.git $@
	cd $@ && git checkout $(snitch_rev)

# Snitch cluster

snitch_cluster_dir = $(shell realpath -q $(snitch_build_dir)/hw/system/snitch_cluster)

$(build_dir)/snitch_cluster_wrapper.pickle.sv: | $(snitch_build_dir)
	make -BC $(snitch_cluster_dir) $(snitch_cluster_dir)/generated/snitch_cluster_wrapper.sv
	bender sources -f -d $(snitch_cluster_dir) | morty -f /dev/stdin -o $@

$(build_dir)/snitch_cluster_wrapper_top.pickle.sv: | $(snitch_build_dir)
	make -BC $(snitch_cluster_dir) $(snitch_cluster_dir)/generated/snitch_cluster_wrapper.sv
	bender sources -f -d $(snitch_cluster_dir) | morty -f /dev/stdin --top snitch_cluster_wrapper -o $@

bla: $(build_dir)/snitch_cluster_wrapper_top.pickle.sv


pickles: $(build_dir)/snitch_cluster_wrapper.pickle.sv $(build_dir)/snitch_cluster_wrapper_top.pickle.sv

# Occamy

$(build_dir)/occamy_top.pickle.sv: | $(snitch_build_dir)
	bender sources -f -d $(snitch_build_dir)/hw/system/occamy) | morty -f /dev/stdin -o $@

# TODO @paulsc: makes morty stack-overflow
#pickles: $(build_dir)/occamy_top.pickle.sv
