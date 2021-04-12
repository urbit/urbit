::  tests for the scrypt key derivation algorithm
::
/+  *test
=,  scr:crypto
::
|%
::
::  Test vectors from Section 12 of RFC 7914:
::  https://tools.ietf.org/html/rfc7914.html#section-12
::
+$  vector  [pw=@ux salt=@ux n=@ud r=@ud p=@ud out=@ux]
::
++  test-vectors
  ^-  tang
  |^  (zing (turn vectors case))
  ::
  ++  case
    |=  vector
    %+  expect-eq
      !>  (swp 3 out)
      !>  `@ux`(hsh pw salt n r p 64)
  ::
  ++  vectors
    ::  TODO: until scrypt has been jetted, we can only test the
    ::  first vector; the others do not finish in a reasonable
    ::  amount of time.
    %+  scag  1  ^-  (list vector)
    :~
      :*
        0x0
        0x0
        16  1  1
        0x77d6.5762.3865.7b20.3b19.ca42.c18a.0497.
          f16b.4844.e307.4ae8.dfdf.fa3f.ede2.1442.
          fcd0.069d.ed09.48f8.326a.753a.0fc8.1f17.
          e8d3.e0fb.2e0d.3628.cf35.e20c.38d1.8906
      ==
    ::
      :*
        0x7061.7373.776f.7264
        0x4e61.436c
        1.024  8  16
        0xfdba.be1c.9d34.7200.7856.e719.0d01.e9fe.
          7c6a.d7cb.c823.7830.e773.7663.4b37.3162.
          2eaf.30d9.2e22.a388.6ff1.0927.9d98.30da.
          c727.afb9.4a83.ee6d.8360.cbdf.a2cc.0640
      ==
    ::
      :*
        0x70.6c65.6173.656c.6574.6d65.696e
        0x536f.6469.756d.4368.6c6f.7269.6465
        16.384  8  1
        0x7023.bdcb.3afd.7348.461c.06cd.81fd.38eb.
          fda8.fbba.904f.8e3e.a9b5.43f6.545d.a1f2.
          d543.2955.613f.0fcf.62d4.9705.242a.9af9.
          e61e.85dc.0d65.1e40.dfcf.017b.4557.5887
      ==
    ::
      :*
        0x70.6c65.6173.656c.6574.6d65.696e
        0x536f.6469.756d.4368.6c6f.7269.6465
        1.048.576  8  1
        0x2101.cb9b.6a51.1aae.addb.be09.cf70.f881.
          ec56.8d57.4a2f.fd4d.abe5.ee98.20ad.aa47.
          8e56.fd8f.4ba5.d09f.fa1c.6d92.7c40.f4c3.
          3730.4049.e8a9.52fb.cbf4.5c6f.a77a.41a4
      ==
    ==
  --
--
