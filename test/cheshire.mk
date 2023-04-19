
cheshire_build_dir = $(build_dir)/cheshire
cheshire_rev = ff4e8cd0558355bf9672721f68401f2affba9b3e

$(cheshire_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/cheshire.git $@
	cd $@ && git checkout $(cheshire_rev)
	cd $@ && git submodule update --init --recursive
	cd $@ && make all
	cd $@ && make slang-check-cheshire

$(build_dir)/cheshire_top.pickle.sv: | $(cheshire_build_dir)
	cp $(cheshire_build_dir)/build/cheshire_top.pickle.sv $@

pickles: $(build_dir)/cheshire_top.pickle.sv