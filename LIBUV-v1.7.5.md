urbit libuv-v1.7.5
==================

This branch investigates a transition from `libuv_0.11` to `libuv-v1.7.5`.

Creating this branch
--------------------

This step is done already (you don't do it).  These notes are for reference.

From `github.com` create branch `libuv-v1.7.5` from `https://github.com/jfranklin9000/urbit/tree/test`.

Get the newly created branch:

	git clone -b libuv-v1.7.5 https://github.com/jfranklin9000/urbit
	mv urbit urbit-libuv-v1.7.5

Get `libuv-v1.7.5.tar.gz` and `libuv-v1.7.5.tar.gz.sign`, verify, unpack:

	curl -o libuv-v1.7.5.tar.gz http://dist.libuv.org/dist/v1.7.5/libuv-v1.7.5.tar.gz
	curl -o libuv-v1.7.5.tar.gz.sign http://dist.libuv.org/dist/v1.7.5/libuv-v1.7.5.tar.gz.sign
	gpg --keyserver pool.sks-keyservers.net --recv-keys AE9BC059
	gpg --verify libuv-v1.7.5.tar.gz.sign
	tar xfz libuv-v1.7.5.tar.gz

Move the `libuv-v1.7.5` source tree into the `urbit-libuv-v1.7.5` source tree:

	mv libuv-v1.7.5 urbit-libuv-v1.7.5/outside

Update the `github.com` repository:

	cd urbit-libuv-v1.7.5
	git add -f outside/libuv-v1.7.5
	git commit -m 'Add outside/libuv-v1.7.5'
	git tag -a DIST-libuv-v1.7.5 -m 'Add outside/libuv-v1.7.5'
	git push --follow-tags

This creates commit `f7abf9341376e9840827ea64204fd6e4a8e9a661` and is tagged `DIST-libuv-v1.7.5`.

Verifying this branch
---------------------

This step is optional.  It verifies that the (unpacked) tarball from `dist.libuv.org` was committed unchanged.

Get the branch:

	git clone -b libuv-v1.7.5 https://github.com/jfranklin9000/urbit
	mv urbit urbit-libuv-v1.7.5
	cd urbit-libuv-v1.7.5
	git checkout DIST-libuv-v1.7.5
	cd ..

Get `libuv-v1.7.5.tar.gz` and `libuv-v1.7.5.tar.gz.sign`, verify, unpack:

	curl -o libuv-v1.7.5.tar.gz http://dist.libuv.org/dist/v1.7.5/libuv-v1.7.5.tar.gz
	curl -o libuv-v1.7.5.tar.gz.sign http://dist.libuv.org/dist/v1.7.5/libuv-v1.7.5.tar.gz.sign
	gpg --keyserver pool.sks-keyservers.net --recv-keys AE9BC059
	gpg --verify libuv-v1.7.5.tar.gz.sign
	tar xfz libuv-v1.7.5.tar.gz

Compare the (unpacked) tarball with the branch:

	diff -r libuv-v1.7.5 urbit-libuv-v1.7.5/outside/libuv-v1.7.5

You should get a null diff.

You can delete `libuv-v1.7.5.tar.gz`, `libuv-v1.7.5.tar.gz.sign` and `libuv-v1.7.5` as they are not needed anymore.
