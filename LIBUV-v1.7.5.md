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

Build the branch
----------------

Tag `MAKE-libuv-v1.7.5` is a commit that only has `Makefile` changes to build urbit with `libuv-v1.7.5`.
This commit has no changes to the urbit code to address possible issues with using `libuv-v1.7.5`.

If you already cloned the branch from verifying:

	cd urbit-libuv-v1.7.5
	git checkout MAKE-libuv-v1.7.5

Otherwise, get the branch:

	git clone -b libuv-v1.7.5 https://github.com/jfranklin9000/urbit
	mv urbit urbit-libuv-v1.7.5
	cd urbit-libuv-v1.7.5
	git checkout MAKE-libuv-v1.7.5

Build it:

	make

Sanity check:

	find . -name "libuv.a"

Try it
------

	bin/urbit -c mycomet

	~
	urbit: home is mycomet
	loom: mapped 2048MB
	boot: installed 229 jets
	boot: loading /Users/urbit/urbit-libuv-v1.7.5/urb/urbit.pill
	arvo: time: ~2015.10.26..09.54.15..1946
	generating 2048-bit RSA pair...
	cp /Users/urbit/urbit-libuv-v1.7.5/urb/urbit.pill mycomet/.urb
	saving passcode in mycomet/.urb/code.~pasnev-nachut
	(for real security, write it down and delete the file...)
	ames: czar zod.urbit.org: ip .192.241.195.84
	ames: on localhost, UDP 53365.
	http: live (insecure) on 8081
	http: live ("secure") on 8444
	; ~zod |Tianming| is your neighbor
	; ~zod |Tianming| is your neighbor
	<awaiting hood, this may take a few minutes>
	[%dill-init ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %hood]
	activated sync from %kids on ~zod to %base
	activated sync from %base on ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl to %home
	sync succeeded from %base on ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl to %home
	'initial merge succeeded'
	[~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl, driving ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl]
	activated app base/dojo
	activated app base/talk
	[linked to [p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%talk]]
	--------------| new mailbox %porch: default home
	--------------| cap default home
	--------------| met ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %h
	[linked to [p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%dojo]]
	~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl:dojo> ^x
	~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl:talk() ;join ~doznec/urbit-meta
	sync succeeded from %kids on ~zod to %base
	--------------| new glyph '='
	----------------| ;join ~doznec/urbit-meta
	--------------| porch: hey '=' ~doznec/urbit-meta
	; ~doznec _Urban Republic_ is your neighbor
	; ~doznec _Urban Republic_ is your neighbor
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-kill %talk 2]
	[%talk-pull ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl 2 /sole]
	[%drum-quit ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl 1 p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%talk]
	--------------| new '=' ~doznec/urbit-meta
	--------------| --met ~hidduc-posmeg-hidduc-posmeg %hear
	--------------| --met ~parret-borden %hear
	--------------| --met ~loppeg-milper %hear
	--------------| --met ~doznec %hear
	--------------| --met ~racbes-solmun %hear
	--------------| --met ~sivbud-barnel %hear
	--------------| --met ~fortux-nartun %hear
	--------------| --met ~divpex-lidruc %hear
	--------------| --met ~ramtev-wisbyt %hear
	--------------| --met ~bilmet-havwer %hear
	--------------| --met ~sivtyv-barnel %hear
	--------------| --met ~tacden-lodret %hear
	--------------| --met ~nodpet-rinned %hear
	--------------| --met ~racleb-fitwyt %hear
	--------------| --met ~dilnem-nodfun %hear
	--------------| --met ~ripnul-rilnyx %hear
	--------------| --met ~talsur-todres %hear
	--------------| --met ~hodnym-ronsut-famnyt-fitpyl--pacryl-bisdet-dardeg-ronnyd 
	--------------| --met ~novrud-hanweb %hear
	--------------| --met ~todsef-nathes %hear
	--------------| --met ~fodrem-michex %hear
	--------------| --met ~worlen-wicwet %hear
	--------------| --met ~hidduc-posmeg %hear
	--------------| --met ~ramdun-mirpec %hear
	--------------| --met ~bosmex-palseg %hear
	--------------| --met ~pormev-taglyn %hear
	--------------| --met ~simtex-wicnup-pagbes-matbec--tommyr-bilnyd-haptec-libdem 
	--------------| --met ~pagmel-doclur-sarmer-dislyn--lasnem-bisred-mirdyt-nalwyx 
	--------------| --met ~matwyc-sablud %hear
	--------------| --met ~rabtug-molhut %hear
	[unlinked from [p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%talk]]
	[linked to [p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%talk]]
	--------------| new mailbox %porch: default home
	--------------| met ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %h
	--------------| new '=' ~doznec/urbit-meta
	--------------| --met ~hidduc-posmeg-hidduc-posmeg %hear
	--------------| --met ~parret-borden %hear
	--------------| --met ~loppeg-milper %hear
	--------------| --met ~doznec %hear
	--------------| --met ~racbes-solmun %hear
	--------------| --met ~sivbud-barnel %hear
	--------------| --met ~fortux-nartun %hear
	--------------| --met ~divpex-lidruc %hear
	--------------| --met ~ramtev-wisbyt %hear
	--------------| --met ~bilmet-havwer %hear
	--------------| --met ~sivtyv-barnel %hear
	--------------| --met ~tacden-lodret %hear
	--------------| --met ~nodpet-rinned %hear
	--------------| --met ~racleb-fitwyt %hear
	--------------| --met ~dilnem-nodfun %hear
	--------------| --met ~ripnul-rilnyx %hear
	--------------| --met ~talsur-todres %hear
	--------------| --met ~hodnym-ronsut-famnyt-fitpyl--pacryl-bisdet-dardeg-ronnyd 
	--------------| --met ~novrud-hanweb %hear
	--------------| --met ~todsef-nathes %hear
	--------------| --met ~fodrem-michex %hear
	--------------| --met ~worlen-wicwet %hear
	--------------| --met ~hidduc-posmeg %hear
	--------------| --met ~ramdun-mirpec %hear
	--------------| --met ~bosmex-palseg %hear
	--------------| --met ~pormev-taglyn %hear
	--------------| --met ~simtex-wicnup-pagbes-matbec--tommyr-bilnyd-haptec-libdem 
	--------------| --met ~pagmel-doclur-sarmer-dislyn--lasnem-bisred-mirdyt-nalwyx 
	--------------| --met ~matwyc-sablud %hear
	--------------| --met ~rabtug-molhut %hear
	--------------| cap default home
	--------------| porch: hey '=' ~doznec/urbit-meta
	--------------| ~doznec/urbit-meta: cap for urbit meta-discussion
	--------------| ~doznec/urbit-meta: but channel
	--------------| for '=' ~doznec/urbit-meta
	--------------| --met ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl 
	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]

	SNIP

	[%ap-fill-full [~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl %talk] ~doznec 2]
	[%ap-kill %talk 2]
	[%talk-pull ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl 2 /sole]
	[%drum-quit ~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl 1 p=~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl q=%talk]
	--------------[0]

	SNIP

	~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl:talk= testing
	~doslet_dozmyl= testing
	~doslet-ronnep-moptux-nomhec--rivtyr-lidled-hoddur-dozmyl:talk=

This is the only testing I've done so far.  Don't use your planet with this branch.

Reverting to libuv_0.11
-----------------------

Do some cleaning:

	make clean
	make -C outside/libuv-v1.7.5 distclean

Edit `Makefile` from:

	# libuv version
	#LIBUV_VER=libuv_0.11
	LIBUV_VER=libuv-v1.7.5

to:

	# libuv version
	LIBUV_VER=libuv_0.11
	#LIBUV_VER=libuv-v1.7.5

Build it:

	make

Sanity check:

	find . -name "libuv.a"
